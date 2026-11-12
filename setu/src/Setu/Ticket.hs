{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Setu.Ticket
-- Description : The table of numbered tickets: a request is taken, and answered exactly once.
--
-- One 'Table' is one connection.  A caller picks an 'Id' and hands in some 'Work' and a 'Done'
-- (the callback).  The table promises:
--
--  * a request that 'submit' accepts is answered exactly once, whether it finished, was cancelled
--    or was closed under (T-01);
--  * a request that 'submit' refuses is never answered;
--  * when a finish and a cancel meet, only the one that takes the ticket out of the table speaks
--    (T-03).  /Only that one acts./  The loser does nothing, and in particular kills nothing: a
--    loser that kills the winner's thread half way through its hand-off leaves a ticket nobody
--    answers (found in the S0 spike, 6000 races, once by itself and every time with the gap widened);
--  * a ticket has a serial number besides its id, and a finish only takes the ticket it was started
--    for.  An id may be used again as soon as it is answered, and the work that was cancelled and
--    still runs on must not answer the new ticket;
--  * the callback never runs on the thread that called 'submit', 'cancel' or 'close' (T-13);
--  * 'close' returns after every callback has returned and no work is running (T-14), so what the
--    caller gave as callback data may be freed after it.  A callback must not call 'close'.
--
-- Cancelling means /stop waiting/: work that has begun is not interrupted (an interrupted write
-- could leave a half-written file).  Its result is dropped.
module Setu.Ticket
  ( Id, Status(..), statusCode
  , Work, Done, Refusal(..)
  , Config(..), defaultConfig
  , Table, newTable, submit, cancel, close, pendingCount
  , isKill
  ) where

import           Control.Concurrent (forkIO, forkIOWithUnmask)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import           Data.Word (Word64)

type Id = Word64

-- | How a call ended.  The numbers (0 to 7) are those of the C header and part of the ABI.
data Status
  = StOk
  | StApp          -- ^ the API's own error (the header names it per API)
  | StProtocol
  | StIo
  | StTimeout
  | StCancelled
  | StClosed
  | StInternal
  deriving (Eq, Ord, Show, Enum, Bounded)

statusCode :: Status -> Int
statusCode = fromEnum

-- | What to do for a request.  It must not throw; one that does is answered 'StInternal'.
type Work = IO (Status, ByteString)

-- | The callback, as Haskell: id, how it ended, the body.
type Done = Id -> Status -> ByteString -> IO ()

-- | Why 'submit' said no.
data Refusal
  = DuplicateId      -- ^ the id is still waiting (T-02)
  | TooManyPending   -- ^ the limit is reached (T-07)
  | TableClosed
  deriving (Eq, Show)

data Config = Config
  { cfgLimit     :: !Int                    -- ^ most tickets waiting at once (T-07)
  , cfgWorkers   :: !Int                    -- ^ how many requests run at once; they start in the order of submit (T-06)
  , cfgSynthBody :: Status -> ByteString    -- ^ the body of an answer the table makes up itself ('StCancelled', 'StClosed')
  }

defaultConfig :: Config
defaultConfig = Config { cfgLimit = 1024, cfgWorkers = 4, cfgSynthBody = const B.empty }

data Slot = Slot { slotSerial :: !Word64, slotDone :: Done }

data Job = Run !Id !Word64 Work | Stop

data Table = Table
  { tCfg        :: Config
  , tPending    :: TVar (M.Map Id Slot)
  , tSerial     :: TVar Word64
  , tClosed     :: TVar Bool
  , tQueue      :: TQueue Job
  , tRunning    :: TVar Int    -- ^ work that has begun and not ended
  , tDelivering :: TVar Int    -- ^ callbacks handed off and not yet returned
  }

newTable :: Config -> IO Table
newTable cfg = do
  t <- Table cfg <$> newTVarIO M.empty <*> newTVarIO 0 <*> newTVarIO False
                 <*> newTQueueIO <*> newTVarIO 0 <*> newTVarIO 0
  replicateM_ (max 1 (cfgWorkers cfg)) (forkIO (worker t))
  pure t

pendingCount :: Table -> IO Int
pendingCount t = M.size <$> readTVarIO (tPending t)

-- | Take a request.  'Right' means it will be answered once, on a thread of the table.
submit :: Table -> Id -> Work -> Done -> IO (Either Refusal ())
submit t i work done = atomically $ do
  closed <- readTVar (tClosed t)
  m <- readTVar (tPending t)
  if closed then pure (Left TableClosed)
  else if M.member i m then pure (Left DuplicateId)
  else if M.size m >= cfgLimit (tCfg t) then pure (Left TooManyPending)
  else do
    -- the ticket and its place in the queue appear together, so a worker never meets one without the other
    s <- readTVar (tSerial t)
    writeTVar (tSerial t) (s + 1)
    writeTVar (tPending t) (M.insert i (Slot s done) m)
    writeTQueue (tQueue t) (Run i s work)
    pure (Right ())

-- | Ask for a ticket to be cancelled.  Any number of times; a ticket that is gone is not touched.
cancel :: Table -> Id -> IO ()
cancel t i = void (settle t i Nothing StCancelled (cfgSynthBody (tCfg t) StCancelled))

-- | Refuse new requests, answer every waiting one with 'StClosed', and return once every callback
-- has returned and no work is running.  Work already begun is not interrupted: it is waited for.
-- Calling it again is harmless.
close :: Table -> IO ()
close t = do
  mask_ $ do
    taken <- atomically $ do
      writeTVar (tClosed t) True
      m <- readTVar (tPending t)
      writeTVar (tPending t) M.empty
      modifyTVar' (tDelivering t) (+ M.size m)
      pure (M.toList m)
    forM_ taken $ \(i, sl) -> deliver t (slotDone sl) i StClosed (cfgSynthBody (tCfg t) StClosed)
  atomically $ do
    r <- readTVar (tRunning t)
    d <- readTVar (tDelivering t)
    when (r > 0 || d > 0) retry
  replicateM_ (max 1 (cfgWorkers (tCfg t))) (atomically (writeTQueue (tQueue t) Stop))

-- | Take the ticket out of the table and answer it.  'True' if this call was the one that took it.
-- With a serial, only that ticket; without, whichever ticket has the id now.
settle :: Table -> Id -> Maybe Word64 -> Status -> ByteString -> IO Bool
settle t i serial st body = mask_ $ do
  taken <- atomically $ do
    m <- readTVar (tPending t)
    case M.lookup i m of
      Just sl | maybe True (== slotSerial sl) serial -> do
        writeTVar (tPending t) (M.delete i m)
        modifyTVar' (tDelivering t) (+ 1)     -- counted in the transaction that took the ticket, so 'close' cannot miss it
        pure (Just (slotDone sl))
      _ -> pure Nothing
  case taken of
    Nothing   -> pure False
    Just done -> deliver t done i st body >> pure True

-- | Run the callback on a thread of its own (T-13).  The caller has counted it in 'tDelivering'.
-- A callback is not allowed to throw; one that does is not allowed to hurt the table either.
-- (The new thread starts masked, like the one that forks it; the callback is let out of that, so
-- that whatever it does behaves as it would on any other thread.)
deliver :: Table -> Done -> Id -> Status -> ByteString -> IO ()
deliver t done i st body = void $ forkIOWithUnmask $ \unmask ->
  (unmask (done i st body) `catch` \(_ :: SomeException) -> pure ())
    `finally` atomically (modifyTVar' (tDelivering t) (subtract 1))

worker :: Table -> IO ()
worker t = loop
  where
    loop = do
      job <- atomically (readTQueue (tQueue t))
      case job of
        Stop -> pure ()
        Run i s work -> do
          mask $ \restore -> do
            -- a ticket cancelled while it waited in the queue is not run; and a ticket whose id was
            -- used again is not the one this job was made for
            go <- atomically $ do
              m <- readTVar (tPending t)
              case M.lookup i m of
                Just sl | slotSerial sl == s -> modifyTVar' (tRunning t) (+ 1) >> pure True
                _                            -> pure False
            when go $ do
              (do (st, body) <- restore (runWork work)
                  void (settle t i (Just s) st body))
                `finally` atomically (modifyTVar' (tRunning t) (subtract 1))
          loop

-- | Work that throws is answered 'StInternal' (T-11), and so is work whose answer cannot be read
-- (a status or a body that is itself an exception waiting to happen: the callback would meet it
-- on its own thread, where nobody could tell the host).  Only a request to end the thread goes on.
runWork :: Work -> IO (Status, ByteString)
runWork work = (work >>= \(st, body) -> evaluate (st `seq` body `seq` (st, body)))
  `catch` \(e :: SomeException) ->
    if isKill e then throwIO e else pure (StInternal, B.empty)

-- | An exception that asks a thread to end (and is not the work's own doing).  A stack or heap
-- overflow is thrown to the thread too, but it is the work's doing: it is an 'StInternal'.
isKill :: SomeException -> Bool
isKill e = case fromException e of
  Just ThreadKilled   -> True
  Just UserInterrupt  -> True
  _                   -> False
