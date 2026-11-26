-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- The numbers in the guide's "Measurements" (what a call costs, what an open costs, how much memory a khana takes).
-- It is not part of the cabal package: one file, built by hand.
--
--   scripts/build-ghc.sh bench
--   build/hosigraph-bench dtlv /path/to/dtlv [ENTITIES]
--   build/hosigraph-bench jvm  /path/to/java /path/to/datalevin-1.2.10-standalone.jar [ENTITIES]
--
-- ENTITIES (default 25000) is the size of the big world: each entity is 4 events (name, age, score, tag), so 100,000 events.
-- It makes its worlds under the system temp directory and removes them.  Run it alone on the machine: the numbers are times.
module Main (main) where

import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Control.Exception (bracket, finally)
import           Control.Monad
import           Data.Char (isDigit)
import           Data.IORef
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           GHC.Clock (getMonotonicTime)
import           System.Directory
import           System.Environment (getArgs)
import           System.FilePath ((</>))
import           System.IO
import           System.Posix.Process (getProcessID)
import           System.Process (readProcess)
import           Text.Printf (printf)

import           Hosigraph
import qualified Hosigraph.Events as Ev

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  (be, needle, rest) <- case args of
    ("dtlv" : exe : r)      -> pure (KhanaDtlvRepl exe, exe, r)
    ("jvm" : java : jar : r) -> pure (KhanaDatalevinJvm java jar, jar, r)
    _ -> ioError (userError "usage: hosigraph-bench dtlv DTLV [ENTITIES] | jvm JAVA JAR [ENTITIES]")
  let n = case rest of (x : _) -> read x; _ -> 25000 :: Int
      cfg = defaultConfig { hcKhanaBackend = be }
  tmp <- getTemporaryDirectory
  let root = tmp </> "hosigraph-bench"
  removePathForcibly root
  createDirectoryIfMissing True root
  (do latency cfg (root </> "small")
      bigWorld cfg needle n (root </> "big")) `finally` removePathForcibly root

schema :: Schema
schema = M.fromList
  [ ("person/name",  identity (attr TString))
  , ("person/age",   attr TLong)
  , ("person/score", attr TDouble)
  , ("person/tag",   attr TString) ]

-- | Two facts: the item of the latency table.
small :: Int -> [Op]
small i = [ Create (Tmp 1) "person/name" (Lit (VStr (T.pack ("p" ++ show i)))), Create (Tmp 1) "person/age" (Lit (VInt (fromIntegral (i `mod` 90)))) ]

-- | Four facts: the item of the big world.
big :: Int -> [Op]
big i = small i ++ [ Create (Tmp 1) "person/score" (Lit (VDouble (fromIntegral i / 7))), Create (Tmp 1) "person/tag" (Lit (VStr (T.pack ("t" ++ show (i `mod` 10))))) ]

timed :: IO a -> IO (a, Double)
timed act = do t0 <- getMonotonicTime; r <- act; t1 <- getMonotonicTime; pure (r, t1 - t0)

ms :: Double -> String
ms s = printf "%.2f ms" (s * 1000)

-- ---------------------------------------------------------------------------

latency :: HosiConfig -> FilePath -> IO ()
latency cfg dir = bracket (openHosiWith cfg dir schema) closeHosi $ \h -> do
  putStrLn "== what a call costs (an item is a write of 2 facts) =="
  let k = 200
  (rs, t1) <- timed (forM [1 .. k] $ \i -> write h (small i))
  line "write, one item at a time" (t1 / fromIntegral k)
  let ids = [ wrTemps r M.! 1 | r <- rs ]
  (_, t2) <- timed (writeMany h [ small i | i <- [k + 1 .. 2 * k] ])
  line "the same count in writeMany (for each item)" (t2 / fromIntegral k)
  (_, t3) <- timed (forM_ ids $ \i -> getEntity h i)
  line "getEntity, one at a time" (t3 / fromIntegral k)
  (_, t4) <- timed (getEntities h ids)
  printf "  %-48s %s  (%d together)\n" ("getEntities" :: String) (ms t4) k
  let qs = [ "[:find (count ?e) :where [?e :person/name]]", "[:find (sum ?a) :where [?e :person/age ?a]]"
           , "[:find (avg ?a) :where [?e :person/age ?a]]", "[:find ?n :where [?e :person/age 30] [?e :person/name ?n]]"
           , "[:find ?e :where [?e :person/age 30]]", "[:find (count ?e) :where [?e :person/age 30]]" ]
  -- once untimed: the first use of a query pays for parsing it and, in the JVM, for warming up
  forM_ qs $ \q -> query h q []
  _ <- queryMany h [ (q, []) | q <- qs ]
  (_, t5) <- timed (forM_ qs $ \q -> query h q [])
  (_, t6) <- timed (queryMany h [ (q, []) | q <- qs ])
  printf "  %-48s %s -> %s\n" ("6 queries (one by one -> queryMany)" :: String) (ms t5) (ms t6)
  (_, t7) <- timed (stats h)
  printf "  %-48s %s\n" ("stats" :: String) (ms t7)
  where line what t = printf "  %-48s %s\n" (what :: String) (ms t)

-- ---------------------------------------------------------------------------

bigWorld :: HosiConfig -> String -> Int -> FilePath -> IO ()
bigWorld cfg needle n dir = do
  putStrLn ""
  printf "== a world of %d entities (%d events) ==\n" n (4 * n)
  let opened = bracket (openHosiWith cfg dir schema) closeHosi (const (pure ()))
  measure needle "written, in groups of 500 (writeMany), the khana kept in step" $
    bracket (openHosiWith cfg dir schema) closeHosi $ \h ->
      forM_ [0, 500 .. n - 1] $ \i -> void (writeMany h [ big j | j <- [i .. min (n - 1) (i + 499)] ])
  du "after the writes" dir
  -- 1. a healthy world, closed and opened again: the khana is in step
  measure needle "open: the khana is in step" opened
  -- 2. behind: 100 batches go into the events only (as if the khana had missed them)
  do ev <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = hcClock cfg, Ev.ecSync = hcSync cfg } (dir </> "events")
     forM_ [n .. n + 99] $ \j -> Ev.withEventsLock ev (Ev.record ev (big j) >>= Ev.commit ev)
     Ev.closeEvents ev
  measure needle "open: the khana is 100 batches behind" opened
  -- 3. gone: made again from the events
  removePathForcibly (dir </> khanaDirName (hcKhanaBackend cfg))
  measure needle "open: the khana is made again from all the events" opened
  du "after it was made again" dir

-- | Run, and say how long it took, and the most memory the khana's program and we ourselves held (sampled every 25 ms; a program that lives for less than that may be missed).
measure :: String -> String -> IO () -> IO ()
measure needle what act = do
  peakK <- newIORef 0
  peakS <- newIORef 0
  sampler <- forkIO $ forever $ do
    k <- rssOf needle
    s <- selfRss
    modifyIORef' peakK (max k)
    modifyIORef' peakS (max s)
    threadDelay 25000
  ((), t) <- timed act `finally` killThread sampler
  k <- readIORef peakK
  s <- readIORef peakS
  printf "  %-62s %7.1f s   khana's program %5d MB   hosigraph %4d MB\n" what t (k `div` 1024) (s `div` 1024)

-- | Sum of the resident memory (KB) of the processes whose command line has this text, other than ourselves.
rssOf :: String -> IO Int
rssOf needle = do
  me <- show <$> getProcessID
  out <- readProcess "ps" ["-axo", "pid=,rss=,command="] ""
  pure (sum [ read r | l <- lines out, (p : r : _) <- [words l], p /= me, all isDigit r, needle `isInfixOf` l ])

selfRss :: IO Int
selfRss = do
  me <- show <$> getProcessID
  out <- readProcess "ps" ["-o", "rss=", "-p", me] ""
  pure (case words out of (r : _) | all isDigit r -> read r; _ -> 0)

du :: String -> FilePath -> IO ()
du what dir = do
  out <- readProcess "du" ["-sk", dir </> "events"] ""
  kh <- doesDirectoryExist (dir </> "khana-dtlv")
  out2 <- if kh then readProcess "du" ["-sk", dir </> "khana-dtlv"] "" else pure "0"
  printf "  disk %s: events %s MB, khana %s MB\n" what (mb out) (mb out2)
  where mb o = printf "%.1f" (fromIntegral (read (takeWhile isDigit o) :: Int) / 1024 :: Double) :: String
