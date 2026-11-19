-- | A waiting room for writes.
--
-- Every write of a 'Hosi' takes the one lock of its events, and each one costs an fsync of the tail and a
-- transaction of the khana: with many people writing at the same moment they stand in line at the door, each
-- paying for the whole trip.  Here they stand in a room instead.  One thread takes from the room what has gathered
-- while it was busy with the last trip (at most 'qcMaxBatch' writes) and makes the trip once for all of them, with
-- 'writeMany'.  There is no waiting for the sake of waiting: when nobody is in the room a write goes at once, and the
-- busier it is the more go together, so the cost of the trip is shared by as many as there are.
--
-- It does not go down.  A room has a size ('qcMaxPending'); when it is full a write is answered 'Busy' (the caller
-- decides: wait and try again, or tell its own caller to come back) and not held until memory is gone.  If a trip
-- fails (a full disk, a khana that does not answer) the writes in it are answered 'Crashed' with the reason and the
-- room goes on with the next.  And it counts what it sees: 'queueStats' says it, and 'queueAdvice' says in words what
-- the counts suggest about the settings and the structure around it.
--
-- Each write is still its own batch in the events: it is atomic and has its own 'WriteReport' (its own temp ids),
-- exactly as with 'writeMany'; the order of the events is the order the room took them in.
module Hosigraph.WriteQueue
  ( WriteQueue
  , QueueConfig(..)
  , defaultQueueConfig
  , SubmitResult(..)
  , QueueStats(..)
  , newWriteQueue
  , submit
  , queueStats
  , queueAdvice
  , closeWriteQueue
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Exception (SomeException, finally, try)
import           Control.Monad

import           Hosigraph

data QueueConfig = QueueConfig
  { qcMaxPending :: Int   -- ^ how many writes may wait in the room (default 10000); the next is answered 'Busy'
  , qcMaxBatch   :: Int   -- ^ how many go on one trip (default 500)
  } deriving Show

defaultQueueConfig :: QueueConfig
defaultQueueConfig = QueueConfig { qcMaxPending = 10000, qcMaxBatch = 500 }

-- | What a caller of 'submit' gets back.
data SubmitResult
  = Written WriteReport   -- ^ it is in the events (fsynced) and in the khana, as with 'write'
  | Refused HosiError     -- ^ the write itself was refused (the same answer 'writeMany' gives for it); nothing of it was written
  | Busy                  -- ^ the room is full; nothing was taken
  | Closed                -- ^ the room is closed; nothing was taken
  | Crashed String        -- ^ the trip failed (the reason is the string); the write may or may not be in the events: look before writing it again
  deriving Show

data QueueStats = QueueStats
  { qsSubmitted    :: !Int          -- ^ writes taken into the room
  , qsBusy         :: !Int          -- ^ writes turned away because the room was full
  , qsBatches      :: !Int          -- ^ trips made
  , qsLargestBatch :: !Int          -- ^ the most writes on one trip
  , qsFullBatches  :: !Int          -- ^ trips that left with 'qcMaxBatch' writes (and more waiting or not)
  , qsHighWater    :: !Int          -- ^ the most writes waiting at once
  , qsCrashedTrips :: !Int          -- ^ trips that failed
  , qsLastCrash    :: Maybe String  -- ^ the reason of the last one
  , qsPending      :: !Int          -- ^ waiting now
  } deriving Show

data Pending = Pending [Op] (MVar SubmitResult)

data QState = QState
  { qPending :: [Pending]            -- newest first
  , qCount   :: !Int
  , qClosed  :: !Bool
  , qStats   :: !QueueStats
  }

data WriteQueue = WriteQueue
  { wqHosi  :: Hosi
  , wqCfg   :: QueueConfig
  , wqState :: MVar QState
  , wqWake  :: MVar ()               -- full when there may be something to take
  , wqDone  :: MVar ()               -- filled when the thread has finished
  }

emptyStats :: QueueStats
emptyStats = QueueStats 0 0 0 0 0 0 0 Nothing 0

-- | Open a room for the writes of this 'Hosi' and start its thread.  (The program must be built with @-threaded@
-- for callers to run at the same time; without it they still take turns correctly.)
newWriteQueue :: Hosi -> QueueConfig -> IO WriteQueue
newWriteQueue h cfg0 = do
  let cfg = cfg0 { qcMaxPending = max 0 (qcMaxPending cfg0), qcMaxBatch = max 1 (qcMaxBatch cfg0) }
  st   <- newMVar (QState [] 0 False emptyStats)
  wake <- newEmptyMVar
  done <- newEmptyMVar
  let q = WriteQueue h cfg st wake done
  _ <- forkIO (worker q `finally` (abandon q >> putMVar done ()))
  pure q

-- | Put a write in the room and wait for its answer.
submit :: WriteQueue -> [Op] -> IO SubmitResult
submit q ops = do
  v <- newEmptyMVar
  r <- modifyMVar (wqState q) $ \s ->
    if qClosed s then pure (s, Just Closed)
    else if qCount s >= qcMaxPending (wqCfg q)
      then pure (s { qStats = (qStats s) { qsBusy = qsBusy (qStats s) + 1 } }, Just Busy)
      else do
        let c  = qCount s + 1
            st = qStats s
        pure ( s { qPending = Pending ops v : qPending s, qCount = c
                 , qStats = st { qsSubmitted = qsSubmitted st + 1, qsHighWater = max (qsHighWater st) c } }
             , Nothing )
  case r of
    Just answer -> pure answer
    Nothing     -> void (tryPutMVar (wqWake q) ()) >> takeMVar v

-- | The counts so far.
queueStats :: WriteQueue -> IO QueueStats
queueStats q = withMVar (wqState q) $ \s -> pure ((qStats s) { qsPending = qCount s })

-- | Close the room: no new write is taken (they are answered 'Closed'), what is in the room is written, and the call
-- returns when the thread has finished.  The 'Hosi' itself is not closed.
closeWriteQueue :: WriteQueue -> IO ()
closeWriteQueue q = do
  modifyMVar_ (wqState q) (\s -> pure s { qClosed = True })
  _ <- tryPutMVar (wqWake q) ()
  readMVar (wqDone q)

-- ---------------------------------------------------------------- the thread

worker :: WriteQueue -> IO ()
worker q = loop
  where
    maxBatch = qcMaxBatch (wqCfg q)
    loop = do
      takeMVar (wqWake q)
      (batch, closed, nothingLeft) <- modifyMVar (wqState q) $ \s -> do
        let (now, later) = splitAt maxBatch (reverse (qPending s))
            n  = length now
            st = qStats s
            st' | n == 0    = st
                | otherwise = st { qsBatches = qsBatches st + 1, qsLargestBatch = max (qsLargestBatch st) n
                                 , qsFullBatches = qsFullBatches st + (if n >= maxBatch then 1 else 0) }
        pure (s { qPending = reverse later, qCount = length later, qStats = st' }, (now, qClosed s, null later))
      unless (null batch) (trip batch)
      if not nothingLeft then void (tryPutMVar (wqWake q) ()) >> loop
      else if closed then pure ()
      else loop

    -- one trip for the whole batch: one writeMany
    trip batch = do
      r <- try (writeMany (wqHosi q) [ ops | Pending ops _ <- batch ]) :: IO (Either SomeException [Either HosiError WriteReport])
      case r of
        Right rs | length rs == length batch ->
          forM_ (zip batch rs) $ \(Pending _ v, x) -> putMVar v (either Refused Written x)
        Right rs -> crashed batch ("writeMany answered " ++ show (length rs) ++ " for " ++ show (length batch) ++ " writes")
        Left e   -> crashed batch (show e)

    crashed batch why = do
      modifyMVar_ (wqState q) $ \s ->
        let st = qStats s in pure s { qStats = st { qsCrashedTrips = qsCrashedTrips st + 1, qsLastCrash = Just why } }
      forM_ batch $ \(Pending _ v) -> putMVar v (Crashed why)

-- | If the thread ever stops, nobody is left waiting for ever.
abandon :: WriteQueue -> IO ()
abandon q = do
  ps <- modifyMVar (wqState q) $ \s -> pure (s { qPending = [], qCount = 0, qClosed = True }, qPending s)
  forM_ ps $ \(Pending _ v) -> void (tryPutMVar v (Crashed "the thread of the waiting room stopped"))

-- ---------------------------------------------------------------- advice

-- | What the counts suggest about the settings, and about what is around the room.  An empty list means nothing to say.
-- The room does not go down for any of these: they are told so that a person can look.
queueAdvice :: QueueConfig -> QueueStats -> [String]
queueAdvice cfg st = concat
  [ [ show (qsBusy st) ++ " writes were turned away (Busy): the room, which holds " ++ show (qcMaxPending cfg)
      ++ ", was full.  If the rushes are short, a bigger qcMaxPending carries them; if the load is steady, the writes "
      ++ "come faster than the khana takes them, and a bigger room only moves the waiting: look at the speed of the khana "
      ++ "(its disk, its JVM) and at how many callers write at once."
    | qsBusy st > 0 ]
  , [ show (qsCrashedTrips st) ++ " trips failed (the last: " ++ maybe "?" id (qsLastCrash st) ++ "): look at the disk "
      ++ "(space, errors) and at whether the khana answers.  The writes of those trips were answered Crashed; "
      ++ "look at the events before writing them again."
    | qsCrashedTrips st > 0 ]
  , [ "At most " ++ show (qsHighWater st) ++ " of " ++ show (qcMaxPending cfg) ++ " places were in use: the room "
      ++ "has little left for a rush bigger than the biggest so far."
    | qcMaxPending cfg > 0, qsBusy st == 0, qsHighWater st * 5 >= qcMaxPending cfg * 4 ]
  , [ show (qsFullBatches st) ++ " of " ++ show (qsBatches st) ++ " trips left full (qcMaxBatch = " ++ show (qcMaxBatch cfg)
      ++ "): the khana is the limit now.  A bigger qcMaxBatch shares each trip among more writes, at the price of a longer "
      ++ "wait for the ones at the back."
    | qsBatches st >= 20, qsFullBatches st * 2 > qsBatches st ]
  , [ "Trips carry " ++ avg ++ " writes on average over " ++ show (qsBatches st) ++ " trips: the load is low "
      ++ "enough that the room is not needed; it costs a hand-over between threads for each write."
    | qsBatches st >= 200, qsSubmitted st < qsBatches st + qsBatches st `div` 20 ]
  ]
  where
    avg = let a = fromIntegral (qsSubmitted st) / fromIntegral (max 1 (qsBatches st)) :: Double
          in show (fromIntegral (round (a * 10) :: Int) / 10 :: Double)
