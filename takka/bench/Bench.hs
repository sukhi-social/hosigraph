{-# LANGUAGE OverloadedStrings #-}
-- | Rough, honest numbers (single machine, one run — treat as orders of magnitude).
--   build/takka-bench [dir]
module Main (main) where

import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           GHC.Clock (getMonotonicTime)
import           Numeric (showFFloat)
import           System.Directory
import           System.Environment (getArgs)
import           System.IO

import           Takka
import           Takka.Edn (readEdn)

attrOf' :: Text -> PullAttr
attrOf' t = PullAttr (Keyword t) (Keyword t) False Nothing Nothing Nothing Nothing RNone

schema :: Schema
schema = M.fromList
  [ ("person/name", identity (attr TString)), ("person/age", attr TLong)
  , ("person/city", attr TString)
  , ("node/id", identity (attr TLong)), ("node/next", many ref) ]

cities :: [Text]
cities = ["Tokyo", "Osaka", "Kyoto", "Seoul", "Busan", "Sapporo", "Nagoya", "Fukuoka"]

parseQ :: Text -> Query
parseQ t = either (error . ("parse: " ++)) id (readEdn t >>= parseQuery)

timed :: String -> IO a -> IO (a, Double)
timed label act = do
  t0 <- getMonotonicTime
  r  <- act
  t1 <- getMonotonicTime
  putStrLn ("  " ++ label ++ ": " ++ showFFloat (Just 3) (t1 - t0) " s")
  pure (r, t1 - t0)

perSec :: Int -> Double -> String
perSec n s = showFFloat (Just 0) (fromIntegral n / s) ""

micros :: Int -> Double -> String
micros n s = showFFloat (Just 1) (s * 1e6 / fromIntegral n) ""

fresh :: FilePath -> IO ()
fresh d = doesDirectoryExist d >>= \e -> when e (removeDirectoryRecursive d)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- getArgs
  let dir = case args of (d : _) -> d; _ -> "/tmp/takka-bench"
      n   = 100000 :: Int
  fresh dir
  withConn dir schema $ \conn -> do
    putStrLn ("bulk load: " ++ show n ++ " people x 3 attributes = " ++ show (3 * n) ++ " datoms, 10 transactions")
    (_, tLoad) <- timed "load" $ forM_ [0 .. 9] $ \chunk ->
      void $ transact conn
        [ EntityMap ENew [ ("person/name", TVal (VStr (T.pack ("person-" ++ show i))))
                         , ("person/age",  TVal (VInt (fromIntegral (i `mod` 100))))
                         , ("person/city", TVal (VStr (cities !! (i `mod` 8)))) ]
        | i <- [chunk * (n `div` 10) .. (chunk + 1) * (n `div` 10) - 1] ]
    putStrLn ("    = " ++ perSec (3 * n) tLoad ++ " datoms/s")

    putStrLn "\nreads on one snapshot (query pre-parsed):"
    withDb conn $ \db -> do
      let qPoint = parseQ "[:find ?a :in $ ?n :where [?e :person/name ?n] [?e :person/age ?a]]"
          k = 20000 :: Int
      (_, t1) <- timed (show k ++ " point lookups (unique name -> age)") $
        forM_ [1 .. k] $ \i ->
          runQuery db qPoint [InScalar (VStr (T.pack ("person-" ++ show ((i * 5) `mod` n))))]
      putStrLn ("    = " ++ micros k t1 ++ " us/query")

      let selText = "[:find ?n :where [?e :person/age 42] [?e :person/city \"Kyoto\"] [?e :person/name ?n]]"
      (rows, _) <- timed "selective join: age=42 AND city=Kyoto -> names" (runQuery db (parseQ selText) [])
      putStrLn ("    " ++ show (length rows) ++ " rows")
      (_, steps) <- explainEdn db selText []
      mapM_ (TIO.putStrLn . ("    plan" <>)) steps

      let qScan = parseQ "[:find ?n :where [?e :person/age ?a] [(> ?a 97)] [?e :person/name ?n]]"
      (rows2, _) <- timed "age > 97 (a range scan on the age index), then fetch names" (runQuery db qScan [])
      putStrLn ("    " ++ show (length rows2) ++ " rows")

      let qAgg = parseQ "[:find ?c (count ?e) :where [?e :person/city ?c]]"
      (rows3, _) <- timed "group by city over all 100,000 people" (runQuery db qAgg [])
      putStrLn ("    " ++ show (length rows3) ++ " groups")

      let qNarrow = parseQ "[:find ?n :where [?e :person/age ?a] [(> ?a 98)] [?e :person/name ?n]]"
      (rowsN, _) <- timed "age > 98 (1% of the people)" (runQuery db qNarrow [])
      putStrLn ("    " ++ show (length rowsN) ++ " rows")
      (_, tFull) <- timed "same condition without a usable range (age + 0 > 98: filter after a full scan)" $
        runQuery db (parseQ "[:find ?n :where [?e :person/age ?a] [(+ ?a 0) ?b] [(> ?b 98)] [?e :person/name ?n]]") []
      _ <- pure tFull

      let qOr = parseQ "[:find (count ?e) :where (or [?e :person/city \"Kyoto\"] [?e :person/city \"Osaka\"])]"
      (rowsO, _) <- timed "or over two cities (25,000 people)" (runQuery db qOr [])
      putStrLn ("    " ++ show rowsO)

      let qPull = parseQ "[:find ?e :in $ ?n :where [?e :person/name ?n]]"
          kp = 10000 :: Int
      (_, tPull) <- timed (show kp ++ " pulls ([:person/name :person/age :person/city]) after a name lookup") $
        forM_ [1 .. kp] $ \i -> do
          r <- runQuery db qPull [InScalar (VStr (T.pack ("person-" ++ show ((i * 7) `mod` n))))]
          case r of
            [[VInt e]] -> void (pull db [PAttr (attrOf' "person/name"), PAttr (attrOf' "person/age"), PAttr (attrOf' "person/city")] e)
            _          -> pure ()
      putStrLn ("    = " ++ micros kp tPull ++ " us per lookup + pull")

      (_, tCount) <- timed "2000 counts (datomCount / countDatoms: O(log n) via the counted tree)" $
        replicateM_ 1000 (datomCount db >> countDatoms db Nothing (Just "person/age") (Just (VInt 42)))
      putStrLn ("    = " ++ micros 2000 tCount ++ " us/count")

    putStrLn "\nlarge values (50 KB strings stored out of line):"
    let bigBody i = T.replicate 50000 (T.singleton (toEnum (97 + i `mod` 26))) <> T.pack (show i)
        nb = 400 :: Int
    (_, tBig) <- timed (show nb ++ " documents of ~50 KB (one transaction)") $
      void $ transact conn
        [ EntityMap ENew [("person/name", TVal (VStr (T.pack ("doc-" ++ show i)))), ("person/city", TVal (VStr (bigBody i)))]
        | i <- [1 .. nb] ]
    putStrLn ("    = " ++ perSec (nb * 50000 `div` 1000) tBig ++ " KB/s")
    withDb conn $ \db -> do
      let qBig = parseQ "[:find ?n :in $ ?b :where [?e :person/city ?b] [?e :person/name ?n]]"
      (_, tLook) <- timed "200 lookups by the full 50 KB value" $
        forM_ [1 .. 200 :: Int] $ \i -> runQuery db qBig [InScalar (VStr (bigBody ((i * 3) `mod` nb + 1)))]
      putStrLn ("    = " ++ micros 200 tLook ++ " us/lookup")

    putStrLn "\ndurable commits (fsync on every transaction):"
    let c = 300 :: Int
    (_, tCommit) <- timed (show c ++ " single-entity transactions") $
      forM_ [1 .. c] $ \i ->
        transact conn [EntityMap ENew [("person/name", TVal (VStr (T.pack ("late-" ++ show i))))]]
    putStrLn ("    = " ++ perSec c tCommit ++ " commits/s")

  putStrLn "\nrecursive rules (stratified semi-naive fixpoint) over a chain a0 -> a1 -> ... :"
  forM_ [100, 200, 400, 800 :: Int] $ \len -> do
    let d = dir ++ "-chain"
    fresh d
    withConn d schema $ \conn -> do
      void $ transact conn
        [ EntityMap ENew
            [ ("node/id", TVal (VInt (fromIntegral i)))
            , ("node/next", TMany [ TNest [("node/id", TVal (VInt (fromIntegral (i + 1)))) ] | i < len ]) ]
        | i <- [0 .. len] ]
      rules <- rulesEdn "[[(reach ?a ?b) [?a :node/next ?b]] [(reach ?a ?b) [?a :node/next ?c] (reach ?c ?b)]]"
      (r, _) <- timed ("chain of " ++ show len ++ " edges: all reachable pairs")
        (q conn "[:find (count ?x) :with ?y :in $ % :where (reach ?x ?y)]" [InRules rules])
      putStrLn ("    result = " ++ show r ++ "   (expected pairs: " ++ show (len * (len + 1) `div` 2) ++ ")")
  fresh (dir ++ "-chain")
