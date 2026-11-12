{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Tests of "Setu.Ticket": the rules of the requirements (T-01 to T-08, T-11) and the two that
-- setu adds (T-13, T-14), all in Haskell: no C, no database.
--
-- Waiting is done on MVars and STM, not on the clock, so a test does not depend on how fast the
-- machine is.  A test that would hang fails after a while instead ('patiently').
module TicketTests (ticketTests) where

import           Control.Concurrent
import           Control.Concurrent.STM hiding (check)
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.IORef
import           Data.List (sort)
import qualified Data.Map.Strict as M

import           Harness
import           Setu.Ticket

-- ---------------------------------------------------------------------------
-- What the callbacks heard

data Ans = Ans { ansId :: Id, ansSt :: Status, ansBody :: ByteString, ansThread :: ThreadId, ansMask :: MaskingState }

newtype Rec = Rec (TVar [Ans])

newRec :: IO Rec
newRec = Rec <$> newTVarIO []

-- | The callback: it writes down who answered, on which thread.
hear :: Rec -> Done
hear (Rec v) i st b = do
  tid <- myThreadId
  ms <- getMaskingState
  atomically (modifyTVar' v (Ans i st b tid ms :))

heard :: Rec -> IO [Ans]
heard (Rec v) = reverse <$> readTVarIO v

waitFor :: Rec -> Int -> IO ()
waitFor (Rec v) n = patiently ("answers (" ++ show n ++ ")") . atomically $ do
  xs <- readTVar v
  when (length xs < n) retry

-- | How many times each id was answered.
tally :: [Ans] -> M.Map Id Int
tally xs = M.fromListWith (+) [(ansId a, 1) | a <- xs]

okWork :: ByteString -> Work
okWork b = pure (StOk, b)

-- | Work that says it has begun, then waits for the gate.
gated :: MVar () -> MVar () -> ByteString -> Work
gated started gate b = putMVar started () >> readMVar gate >> pure (StOk, b)

accepted :: Either Refusal () -> IO ()
accepted r = checkEq "submit" (Right ()) r

ticketTests :: [Test]
ticketTests =
  [ ("ticket: every accepted request is answered exactly once, whether it ends, is cancelled or is closed under (T-01)", t01)
  , ("ticket: a request that is refused is never answered (T-01)", t01Refused)
  , ("ticket: an id that is waiting cannot be used; once answered it can (T-02)", t02)
  , ("ticket: work that was cancelled and runs on does not answer the ticket that uses its id next (T-02, T-03)", t02Serial)
  , ("ticket: cancel any number of times, on ids that are gone or never were (T-03)", t03Idempotent)
  , ("ticket: the finish comes first, then the cancel: one answer, the finish (T-03)", t03FinishFirst)
  , ("ticket: the cancel comes first, then the finish: one answer, the cancel (T-03)", t03CancelFirst)
  , ("ticket: a ticket cancelled in the queue is never run (T-03)", t03NeverRun)
  , ("ticket: finish and cancel meet 10000 times: each answered once, and both winners happen (T-03)", t03Race)
  , ("ticket: callbacks run on a thread of their own, not masked (T-04, T-13)", t13Thread)
  , ("ticket: cancel returns without waiting for its callback (T-13)", t13CancelReturns)
  , ("ticket: requests start in the order of submit (T-06)", t06)
  , ("ticket: workers run at the same time (T-06)", tWorkers)
  , ("ticket: the limit on waiting requests (T-07)", t07)
  , ("ticket: close answers every waiting request closed (T-08)", t08)
  , ("ticket: close returns after the callbacks have returned and the work has ended (T-14)", t14)
  , ("ticket: after close, submit is refused; close again is harmless", tAfterClose)
  , ("ticket: work that throws, an answer that cannot be read, a callback that throws (T-11)", t11)
  , ("ticket: submit, cancel and close from many threads: exactly the accepted are answered, once (T-01, T-13, T-14)", tStress)
  ]

-- ---------------------------------------------------------------------------

t01 :: IO ()
t01 = do
  rec <- newRec
  t <- newTable defaultConfig { cfgLimit = 100000, cfgWorkers = 4 }
  let n = 2000 :: Id
  forM_ [1 .. n] $ \i -> do
    accepted =<< submit t i (do when (i `mod` 3 == 0) yield; okWork (BC.pack (show i))) (hear rec)
    -- a third of them are cancelled at once, a few more after a yield
    when (i `mod` 3 == 1) (cancel t i)
    when (i `mod` 7 == 2) (yield >> cancel t i)
  close t
  xs <- heard rec
  checkEq "everyone answered once" (M.fromList [(i, 1) | i <- [1 .. n]]) (tally xs)
  forM_ xs $ \a -> case ansSt a of
    StOk        -> checkEq "an ok answer carries its work's body" (BC.pack (show (ansId a))) (ansBody a)
    StCancelled -> pure ()
    StClosed    -> pure ()
    other       -> throwIO (userError ("unexpected status " ++ show other))

t01Refused :: IO ()
t01Refused = do
  rec <- newRec
  gate <- newEmptyMVar
  started <- newEmptyMVar
  t <- newTable defaultConfig { cfgLimit = 1, cfgWorkers = 1 }
  accepted =<< submit t 1 (gated started gate "a") (hear rec)
  takeMVar started
  r1 <- submit t 1 (okWork "dup") (hear rec)
  r2 <- submit t 2 (okWork "full") (hear rec)
  checkEq "same id" (Left DuplicateId) r1
  checkEq "limit" (Left TooManyPending) r2
  putMVar gate ()
  waitFor rec 1
  close t
  r3 <- submit t 3 (okWork "closed") (hear rec)
  checkEq "closed" (Left TableClosed) r3
  xs <- heard rec
  checkEq "only the accepted one was answered" [1] (map ansId xs)

t02 :: IO ()
t02 = do
  rec <- newRec
  gate <- newEmptyMVar
  started <- newEmptyMVar
  t <- newTable defaultConfig { cfgWorkers = 2 }
  accepted =<< submit t 7 (gated started gate "first") (hear rec)
  takeMVar started
  checkEq "waiting" (Left DuplicateId) =<< submit t 7 (okWork "again") (hear rec)
  putMVar gate ()
  waitFor rec 1
  -- answered, so the id is free again.  (pendingCount goes down when the ticket is taken, which
  -- is before the callback is heard.)
  accepted =<< submit t 7 (okWork "second") (hear rec)
  waitFor rec 2
  close t
  xs <- heard rec
  checkEq "bodies" ["first", "second"] (map ansBody xs)

t02Serial :: IO ()
t02Serial = do
  rec <- newRec
  gateA <- newEmptyMVar
  startedA <- newEmptyMVar
  gateB <- newEmptyMVar
  startedB <- newEmptyMVar
  t <- newTable defaultConfig { cfgWorkers = 2 }
  accepted =<< submit t 1 (gated startedA gateA "A") (hear rec)
  takeMVar startedA
  cancel t 1                    -- A is cancelled and runs on
  waitFor rec 1
  accepted =<< submit t 1 (gated startedB gateB "B") (hear rec)      -- the same id, a new ticket
  takeMVar startedB
  putMVar gateA ()              -- A ends while B's ticket waits under the same id
  -- a probe: it runs on A's worker, so it runs only after that worker has finished with A
  accepted =<< submit t 2 (okWork "probe") (hear rec)
  waitFor rec 2
  checkEq "B's ticket is still waiting" 1 =<< pendingCount t
  putMVar gateB ()
  waitFor rec 3
  close t
  xs <- heard rec
  checkEq "A's cancel, the probe, then B's own finish" [(1, StCancelled, ""), (2, StOk, "probe"), (1, StOk, "B")] [(ansId a, ansSt a, ansBody a) | a <- xs]

t03Idempotent :: IO ()
t03Idempotent = do
  rec <- newRec
  gate <- newEmptyMVar
  started <- newEmptyMVar
  t <- newTable defaultConfig
  cancel t 99                   -- never was
  accepted =<< submit t 1 (gated started gate "x") (hear rec)
  takeMVar started
  replicateM_ 5 (cancel t 1)
  waitFor rec 1
  replicateM_ 5 (cancel t 1)    -- gone
  putMVar gate ()
  close t
  xs <- heard rec
  checkEq "one answer" [(1, StCancelled)] [(ansId a, ansSt a) | a <- xs]

t03FinishFirst :: IO ()
t03FinishFirst = do
  rec <- newRec
  t <- newTable defaultConfig
  accepted =<< submit t 1 (okWork "done") (hear rec)
  waitFor rec 1
  cancel t 1
  close t
  xs <- heard rec
  checkEq "the finish was the one reported" [(StOk, "done")] [(ansSt a, ansBody a) | a <- xs]

t03CancelFirst :: IO ()
t03CancelFirst = do
  rec <- newRec
  gate <- newEmptyMVar
  started <- newEmptyMVar
  t <- newTable defaultConfig { cfgSynthBody = \st -> if st == StCancelled then "cancelled-body" else "" }
  accepted =<< submit t 1 (gated started gate "late") (hear rec)
  takeMVar started
  cancel t 1
  waitFor rec 1
  putMVar gate ()
  close t                       -- the work ends here, and finds nothing to answer
  xs <- heard rec
  checkEq "the cancel was the one reported" [(StCancelled, "cancelled-body")] [(ansSt a, ansBody a) | a <- xs]

t03NeverRun :: IO ()
t03NeverRun = do
  rec <- newRec
  gate <- newEmptyMVar
  started <- newEmptyMVar
  ran <- newIORef False
  t <- newTable defaultConfig { cfgWorkers = 1 }
  accepted =<< submit t 1 (gated started gate "front") (hear rec)
  takeMVar started
  accepted =<< submit t 2 (writeIORef ran True >> okWork "never") (hear rec)
  cancel t 2                    -- still in the queue
  waitFor rec 1
  putMVar gate ()
  waitFor rec 2
  close t
  checkEq "its work did not run" False =<< readIORef ran
  xs <- heard rec
  checkEq "answers" [(2, StCancelled), (1, StOk)] [(ansId a, ansSt a) | a <- xs]

-- | The ticket of the spike: a finish and a cancel at the same moment.  Work and canceller each
-- give way a different number of times, so that the cancel lands before the start, in the work,
-- and after the end.  Every id gets one answer; and both kinds of winner must show up, or the test
-- is not looking at the race.
t03Race :: IO ()
t03Race = do
  rec <- newRec
  t <- newTable defaultConfig { cfgLimit = 100000, cfgWorkers = 4 }
  let n = 10000 :: Id
  forM_ [1 .. n] $ \i -> do
    accepted =<< submit t i (do replicateM_ (fromIntegral (i `mod` 4)) yield; okWork "w") (hear rec)
    void . forkIO $ do replicateM_ (fromIntegral (i `mod` 3)) yield; cancel t i
  waitFor rec (fromIntegral n)
  close t
  xs <- heard rec
  checkEq "everyone answered once" (M.fromList [(i, 1) | i <- [1 .. n]]) (tally xs)
  let oks = length [() | a <- xs, ansSt a == StOk]
      cancelled = length [() | a <- xs, ansSt a == StCancelled]
  checkEq "nothing else" n (fromIntegral (oks + cancelled))
  check ("the finish won " ++ show oks ++ " times, the cancel " ++ show cancelled ++ ": both should happen") (oks > 0 && cancelled > 0)

-- | A caller on the main thread, and callbacks that must be on other threads and not masked.
t13Thread :: IO ()
t13Thread = do
  rec <- newRec
  gate <- newEmptyMVar
  started <- newEmptyMVar
  self <- myThreadId
  t <- newTable defaultConfig { cfgWorkers = 2 }
  accepted =<< submit t 1 (okWork "finish") (hear rec)          -- answered by the finish
  accepted =<< submit t 2 (gated started gate "x") (hear rec)    -- answered by the cancel
  takeMVar started
  cancel t 2
  accepted =<< submit t 3 (gated started gate "y") (hear rec)    -- answered by close
  waitFor rec 2
  _ <- forkIO (waitFor rec 3 >> putMVar gate ())                 -- let the work end once close has answered
  close t
  xs <- heard rec
  checkEq "three answers" 3 (length xs)
  forM_ xs $ \a -> do
    check ("answer " ++ show (ansId a) ++ " came on the thread that called") (ansThread a /= self)
    checkEq ("answer " ++ show (ansId a) ++ " masking state") Unmasked (ansMask a)

t13CancelReturns :: IO ()
t13CancelReturns = do
  seen <- newEmptyMVar
  release <- newEmptyMVar
  gate <- newEmptyMVar
  started <- newEmptyMVar
  t <- newTable defaultConfig
  accepted =<< submit t 1 (gated started gate "x") (\_ st _ -> readMVar release >> putMVar seen st)
  takeMVar started
  patiently "cancel to return while its callback cannot" (cancel t 1)
  putMVar release ()
  st <- patiently "the callback" (takeMVar seen)
  checkEq "status" StCancelled st
  putMVar gate ()
  close t

t06 :: IO ()
t06 = do
  rec <- newRec
  order <- newIORef []
  t <- newTable defaultConfig { cfgLimit = 100000, cfgWorkers = 1 }
  let n = 300 :: Id
  forM_ [1 .. n] $ \i -> accepted =<< submit t i (atomicModifyIORef' order (\xs -> (i : xs, ())) >> okWork "") (hear rec)
  waitFor rec (fromIntegral n)
  close t
  checkEq "started in the order of submit" [1 .. n] . reverse =<< readIORef order

tWorkers :: IO ()
tWorkers = do
  rec <- newRec
  arrived <- newTVarIO (0 :: Int)
  t <- newTable defaultConfig { cfgWorkers = 3 }
  -- three works that each wait until all three are in: they can end only if three run at once
  let meet = do atomically (modifyTVar' arrived (+ 1)); atomically (readTVar arrived >>= \k -> when (k < 3) retry); okWork ""
  forM_ [1 .. 3] $ \i -> accepted =<< submit t i meet (hear rec)
  waitFor rec 3
  close t
  xs <- heard rec
  checkEq "all ok" [StOk, StOk, StOk] (map ansSt xs)

t07 :: IO ()
t07 = do
  rec <- newRec
  gate <- newEmptyMVar
  started <- newEmptyMVar
  t <- newTable defaultConfig { cfgLimit = 3, cfgWorkers = 1 }
  accepted =<< submit t 1 (gated started gate "a") (hear rec)
  takeMVar started
  accepted =<< submit t 2 (okWork "b") (hear rec)
  accepted =<< submit t 3 (okWork "c") (hear rec)
  checkEq "waiting" 3 =<< pendingCount t
  checkEq "full" (Left TooManyPending) =<< submit t 4 (okWork "d") (hear rec)
  cancel t 2                    -- a place is free as soon as the ticket is taken
  checkEq "waiting after the cancel" 2 =<< pendingCount t
  accepted =<< submit t 4 (okWork "d") (hear rec)
  putMVar gate ()
  waitFor rec 4
  close t
  xs <- heard rec
  checkEq "each accepted request answered once, the refused one not" (M.fromList [(i, 1) | i <- [1 .. 4]]) (tally xs)

t08 :: IO ()
t08 = do
  rec <- newRec
  gate <- newEmptyMVar
  started <- newEmptyMVar
  t <- newTable defaultConfig { cfgWorkers = 1, cfgSynthBody = \st -> if st == StClosed then "closed-body" else "" }
  accepted =<< submit t 1 (gated started gate "running") (hear rec)
  takeMVar started
  forM_ [2 .. 5] $ \i -> accepted =<< submit t i (okWork "queued") (hear rec)
  _ <- forkIO (waitFor rec 5 >> putMVar gate ())
  close t
  xs <- heard rec
  checkEq "all five, once" (M.fromList [(i, 1) | i <- [1 .. 5]]) (tally xs)
  checkEq "all closed, with the body for it" (replicate 5 (StClosed, "closed-body")) [(ansSt a, ansBody a) | a <- xs]

-- | Events are written down in the order they happen.  After close returns, every callback has
-- returned (one of them is slow on purpose) and the work that was running has ended.
t14 :: IO ()
t14 = do
  events <- newIORef []
  let ev s = atomicModifyIORef' events (\xs -> (s : xs, ()))
  gate <- newEmptyMVar
  started <- newEmptyMVar
  t <- newTable defaultConfig { cfgWorkers = 1 }
  accepted =<< submit t 1 (do putMVar started (); readMVar gate; ev "work-ended"; pure (StOk, "")) (\_ _ _ -> threadDelay 30000 >> ev "callback-1-returned")
  takeMVar started
  accepted =<< submit t 2 (okWork "") (\_ _ _ -> threadDelay 30000 >> ev "callback-2-returned")
  _ <- forkIO (threadDelay 100000 >> ev "work-released" >> putMVar gate ())
  close t
  ev "close-returned"
  es <- reverse <$> readIORef events
  let at e = case lookup e (zip es [0 :: Int ..]) of
        Just k -> pure k
        Nothing -> throwIO (userError ("event missing: " ++ e ++ " in " ++ show es))
  closeAt <- at "close-returned"
  forM_ ["callback-1-returned", "callback-2-returned", "work-ended"] $ \e -> do
    k <- at e
    check (e ++ " must come before close returned: " ++ show es) (k < closeAt)
  released <- at "work-released"
  ended <- at "work-ended"
  check ("the work ended after it was released: " ++ show es) (released < ended)

tAfterClose :: IO ()
tAfterClose = do
  rec <- newRec
  t <- newTable defaultConfig
  accepted =<< submit t 1 (okWork "x") (hear rec)
  close t
  close t
  checkEq "refused" (Left TableClosed) =<< submit t 2 (okWork "y") (hear rec)
  cancel t 1
  cancel t 2
  xs <- heard rec
  checkEq "just the one answer" [1] (map ansId xs)

t11 :: IO ()
t11 = do
  rec <- newRec
  -- a single worker: if a failure took the worker with it, nothing after would be answered
  t <- newTable defaultConfig { cfgWorkers = 1 }
  accepted =<< submit t 1 (throwIO (ErrorCall "boom")) (hear rec)
  accepted =<< submit t 2 (throwIO StackOverflow) (hear rec)
  accepted =<< submit t 3 (pure (StOk, error "an answer that cannot be read")) (hear rec)
  accepted =<< submit t 4 (okWork "fine") (\_ _ _ -> throwIO (ErrorCall "a callback that throws"))
  accepted =<< submit t 5 (okWork "after") (hear rec)
  waitFor rec 4                  -- 1, 2, 3 and 5 are heard; the callback of 4 throws before it writes anything
  close t                        -- also shows that the callback that threw was counted out
  xs <- heard rec
  checkEq "answered (callbacks are on different threads, so in any order)" [(1, StInternal), (2, StInternal), (3, StInternal), (5, StOk)] (sort [(ansId a, ansSt a) | a <- xs])

-- | A thread, and a way to know it has ended.
spawn :: IO () -> IO (MVar ())
spawn act = do
  d <- newEmptyMVar
  _ <- forkIO (act `finally` putMVar d ())
  pure d

-- | Four submitters and two cancellers run while the table is closed at an arbitrary moment.  When
-- close has returned and the threads are done: the ids that submit accepted were each answered
-- once, the ones it refused were not answered, and no answer was heard on a thread that called.
tStress :: IO ()
tStress = forM_ [0 .. 199 :: Int] $ \r -> do
  rec <- newRec
  t <- newTable defaultConfig { cfgLimit = 30, cfgWorkers = 3 }
  callers <- newTVarIO []
  acceptedIds <- newTVarIO []
  refusedIds <- newTVarIO []
  let iAm = myThreadId >>= \me -> atomically (modifyTVar' callers (me :))
      ids = [ b * 100 + k | b <- [1 .. 4], k <- [0 .. 19] ] :: [Id]
      job i = do
        yield
        when (i `mod` 7 == 0) (throwIO (ErrorCall "boom"))
        okWork (BC.pack (show i))
      submitter b = do
        iAm
        forM_ [0 .. 19] $ \k -> do
          let i = b * 100 + k
          when (k `mod` (b + 1) == 0) yield
          res <- submit t i (job i) (hear rec)
          atomically $ case res of
            Right () -> modifyTVar' acceptedIds (i :)
            Left _   -> modifyTVar' refusedIds (i :)
      canceller c = do
        iAm
        forM_ (zip [0 :: Int ..] ids) $ \(j, i) -> do
          when (j `mod` (c + 1) == 0) yield
          when ((j + c) `mod` 3 == 0) (cancel t i)
  dones <- (++) <$> mapM (spawn . submitter) [1 .. 4] <*> mapM (spawn . canceller) [1 .. 2]
  iAm
  replicateM_ (r `mod` 40) yield
  close t
  mapM_ (patiently "a thread" . takeMVar) dones
  close t                        -- anything accepted after the first close would be a bug; this one has nothing to do
  acc <- readTVarIO acceptedIds
  ref <- readTVarIO refusedIds
  cs <- readTVarIO callers
  xs <- heard rec
  checkEq ("round " ++ show r ++ ": the answered are the accepted, each once") (sort acc) (sort (map ansId xs))
  check ("round " ++ show r ++ ": a refused id was answered") (all (`notElem` map ansId xs) ref)
  check ("round " ++ show r ++ ": an answer came on a thread that called") (all ((`notElem` cs) . ansThread) xs)
  checkEq ("round " ++ show r ++ ": all ids accounted for") (length ids) (length acc + length ref)
