{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Setu.Abi
-- Description : Connections: what stands behind the handles of the C ABI, in Haskell, without C.
--
-- The generated Haskell module (see "Setu.Gen.HsExport") is a thin skin of @foreign export@s over
-- this one: it turns pointers into 'ByteString's and function pointers into 'Done's, and leaves
-- every decision to the functions here, which can be tried without a C compiler.
--
-- A handle is a number, not a pointer.  A number that was never given, or was closed, is refused
-- ('SubmitBadHandle'), so a host that calls with a handle it has closed is told so and does not
-- crash.  (A pointer would have to be believed.)
--
-- /Opening is quick./  'abiOpen' gives the handle at once and starts the library's 'libConnect' on
-- a thread of its own.  Requests wait for it to end; if it failed, each of them is answered with
-- that failure (the same one, with the status the library gave it).  So a host never has to wait
-- for a slow start (the JVM of a database, say) before it can submit.
module Setu.Abi
  ( Library(..)
  , Abi, newAbi
  , SubmitCode(..)
  , abiOpen, abiSubmit, abiCancel, abiClose, abiCloseAll
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad (forM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word64)

import           Setu.Dispatch
import           Setu.Spec (Api)
import           Setu.Ticket
import           Setu.Wire (Reply(..), encodeReply)

-- | What a library gives the ABI: its description and how to start it.
data Library = Library
  { libApi     :: Api
  , libLimit   :: Int    -- ^ most requests waiting on one connection (T-07)
  , libWorkers :: Int    -- ^ most requests running at once on one connection
  , libConnect :: Text -> IO (Either Failure (Dispatch, IO ()))
    -- ^ Start from what the host passed to @open@ (UTF-8 text; the library says what it means).
    -- Gives the handlers, and what to run to release everything when the connection is closed
    -- (after every request has ended).  May take its time; may throw (that is a 'StInternal').
  }

data Conn = Conn
  { cTable :: Table
  , cReady :: MVar (Either Failure (Dispatch, IO ()))   -- ^ filled when 'libConnect' has ended
  }

data Abi = Abi
  { aLib   :: Library
  , aNext  :: IORef Word64
  , aConns :: IORef (M.Map Word64 Conn)
  }

newAbi :: Library -> IO Abi
newAbi lib = Abi lib <$> newIORef 0 <*> newIORef M.empty

-- | How @submit@ ended.  The numbers are what the generated Haskell hands to the C layer, which
-- turns them into errno values; they are not part of the public ABI.
data SubmitCode
  = SubmitOk
  | SubmitDuplicate    -- ^ the id is waiting already
  | SubmitBusy         -- ^ too many are waiting
  | SubmitClosed       -- ^ the connection is closed
  | SubmitBadHandle    -- ^ not a handle that is open
  deriving (Eq, Show, Enum, Bounded)

-- | A handle, at once.  Never 0.
abiOpen :: Abi -> ByteString -> IO Word64
abiOpen abi uri = do
  let lib = aLib abi
  t <- newTable Config
    { cfgLimit = libLimit lib, cfgWorkers = libWorkers lib
    , cfgSynthBody = synthBody }
  ready <- newEmptyMVar
  _ <- forkIO $ do
    r <- case TE.decodeUtf8' uri of
      Left _    -> pure (Left (Failure StProtocol "bad-open" "the uri is not valid UTF-8"))
      Right txt -> do
        x <- try (libConnect lib txt)
        pure $ case x of
          Right y -> y
          Left (e :: SomeException) -> Left (Failure StInternal "open-exception" ("opening failed with an exception: " <> T.pack (show e)))
    putMVar ready r
  h <- atomicModifyIORef' (aNext abi) (\n -> (n + 1, n + 1))
  atomicModifyIORef' (aConns abi) (\m -> (M.insert h (Conn t ready) m, ()))
  pure h

-- | What the table says for itself when it ends a ticket without the work (EDN, like every body).
synthBody :: Status -> ByteString
synthBody st = case st of
  StCancelled -> encodeReply (ReplyErr "cancelled" "the request was cancelled before it ended")
  StClosed    -> encodeReply (ReplyErr "closed" "the connection was closed before the request ended")
  _           -> B.empty

abiSubmit :: Abi -> Word64 -> Id -> ByteString -> Done -> IO SubmitCode
abiSubmit abi h i body done = do
  m <- readIORef (aConns abi)
  case M.lookup h m of
    Nothing -> pure SubmitBadHandle
    Just c  -> do
      r <- submit (cTable c) i (work c) done
      pure $ case r of
        Right ()               -> SubmitOk
        Left DuplicateId       -> SubmitDuplicate
        Left TooManyPending    -> SubmitBusy
        Left TableClosed       -> SubmitClosed
  where
    work c = do
      r <- readMVar (cReady c)
      case r of
        Left f       -> pure (failureReply f)
        Right (d, _) -> runRequest d body

-- | 'False' if the handle is not open.
abiCancel :: Abi -> Word64 -> Id -> IO Bool
abiCancel abi h i = do
  m <- readIORef (aConns abi)
  case M.lookup h m of
    Nothing -> pure False
    Just c  -> cancel (cTable c) i >> pure True

-- | Close one connection: every request still waiting is answered 'StClosed', and this returns when
-- every callback has returned and no request is running (T-14); then the library lets go of what
-- it holds.  A handle that is not open is left alone (so closing twice is harmless).
abiClose :: Abi -> Word64 -> IO ()
abiClose abi h = do
  taken <- atomicModifyIORef' (aConns abi) (\m -> (M.delete h m, M.lookup h m))
  mapM_ closeConn taken

-- | Close every connection (the end of the runtime).  They are closed together, not one after
-- the other: what waits on any of them is answered at once, and a request that runs long on one
-- connection holds up only that connection's own close.
abiCloseAll :: Abi -> IO ()
abiCloseAll abi = do
  all_ <- atomicModifyIORef' (aConns abi) (\m -> (M.empty, M.elems m))
  ended <- forM all_ $ \c -> do
    v <- newEmptyMVar
    _ <- forkIO (closeConn c `finally` putMVar v ())
    pure v
  mapM_ takeMVar ended

closeConn :: Conn -> IO ()
closeConn c = do
  close (cTable c)
  -- the connection may still be starting: its release has to wait for that
  r <- readMVar (cReady c)
  case r of
    Right (_, release) -> release `catch` \(_ :: SomeException) -> pure ()
    Left _             -> pure ()
