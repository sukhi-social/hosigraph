-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- Step 3 of the check on real posts (README.md, "On real posts"): the posts go into a world, and the queries are asked of it.
--
--   mikke-real-check <work-dir> <dtlv> <sudachi> <dictionary> [A|B|C] [R|all|N]
--
-- Reads <work-dir>/notes.json (extract.py) and <work-dir>/queries.txt (yardstick.py), makes the world <work-dir>/world (an old one is
-- removed), writes every note as the fulltext attribute note/body through KhanaDtlvRepl with a Sudachi tokenizer, asks each query as a
-- fulltext call with {:top T} (a search of several words finds the notes that have all of them, within the best T by BM25), and writes
-- <work-dir>/out.tsv (query, R, entities) and <work-dir>/idmap.tsv (note index, entity) for score.py.  T is R, the number of notes that
-- have the word (the default), or the number of notes ("all": the whole answer), or a number N.
-- What it prints is numbers and times, never a post.  The work directory holds real posts: keep it outside the repository, and delete it when done.
module Main (main) where

import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Clock
import           System.Directory
import           System.Environment
import           System.Exit (exitFailure)
import           System.IO
import           System.Process (readProcess)

import           Hosigraph
import           Hosigraph.Mikke.Sudachi

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

timed :: String -> IO a -> IO a
timed label act = do
  t0 <- getCurrentTime
  r <- act
  t1 <- getCurrentTime
  putStrLn (label ++ ": " ++ show (realToFrac (diffUTCTime t1 t0) :: Double) ++ " s")
  hFlush stdout
  pure r

-- | The memory of the processes we started (the program's name and its megabytes).
rss :: String -> IO ()
rss label = do
  out <- readProcess "ps" ["-axo", "rss=,command="] ""
  let ls = [ l | l <- lines out, any (`T.isInfixOf` T.pack l) ["sudachi -l", "dtlv repl"], not ("bash -c" `T.isInfixOf` T.pack l) ]
  forM_ ls $ \l -> do
    let (r, c) = break (== ' ') (dropWhile (== ' ') l)
    let prog = reverse (takeWhile (/= '/') (reverse (takeWhile (/= ' ') (dropWhile (== ' ') c))))
    putStrLn (label ++ " RSS " ++ show (read r `div` (1024 :: Int)) ++ " MB  " ++ prog)

main :: IO ()
main = do
  args <- getArgs
  (work, dtlvExe, sudachiExe, dictPath, modeS, topS) <- case args of
    [w, d, s, c]       -> pure (w, d, s, c, "A", "R")
    [w, d, s, c, m]    -> pure (w, d, s, c, m, "R")
    [w, d, s, c, m, t] -> pure (w, d, s, c, m, t)
    _                  -> hPutStrLn stderr "usage: mikke-real-check <work-dir> <dtlv> <sudachi> <dictionary> [A|B|C] [R|all|N]" >> exitFailure
  hSetEncoding stdout utf8
  let worldDir = work ++ "/world"
  Just notes <- A.decodeFileStrict' (work ++ "/notes.json") :: IO (Maybe [(Int, T.Text)])
  let mode = case modeS of "C" -> ModeC; "B" -> ModeB; _ -> ModeA
  tk <- newSudachi (SudachiSpec sudachiExe dictPath mode)
  let cfg = defaultConfig { hcKhanaBackend = KhanaDtlvRepl dtlvExe, hcTokenizer = Just tk }
      sch = M.fromList [("note/body", fulltext (attr TString))]
  exists <- doesDirectoryExist worldDir
  when exists (removeDirectoryRecursive worldDir)
  h <- timed "open (empty world)" (openHosiWith cfg worldDir sch)
  let withIdx = zip [0 :: Int ..] notes
  pairs <- timed ("write " ++ show (length notes) ++ " notes in batches of 200") $
    fmap concat . forM (chunksOf 200 withIdx) $ \chunk -> do
      r <- write h [ Create (Tmp k) "note/body" (Lit (VStr t)) | (k, (_, (_, t))) <- zip [1 ..] chunk ]
      pure [ (i, show (fromIntegral (wrTemps r M.! k) :: Integer)) | (k, (i, _)) <- zip [1 ..] chunk ]
  writeFile (work ++ "/idmap.tsv") (unlines [ show i ++ "\t" ++ s | (i, s) <- pairs ])
  rss "after write"
  qs <- map (T.splitOn "\t") . T.lines <$> TIO.readFile (work ++ "/queries.txt")
  t0 <- getCurrentTime
  outs <- forM qs $ \cols -> case cols of
    (q : r : _) -> do
      let top = case topS of
                  "R"   -> max 1 (read (T.unpack r) :: Int)
                  "all" -> max 1 (length notes)
                  n     -> max 1 (read n :: Int)
          qry = T.pack ("[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q {:top " ++ show top ++ "}) [[?e ?a ?v]]]]")
      rows <- query h qry [InScalar (VStr q)]
      pure (T.intercalate "\t" [q, r, T.unwords [ T.pack (show n) | [VInt n] <- rows ]])
    _ -> pure ""
  t1 <- getCurrentTime
  putStrLn ("queries: " ++ show (length qs) ++ " in " ++ show (realToFrac (diffUTCTime t1 t0) :: Double) ++ " s")
  TIO.writeFile (work ++ "/out.tsv") (T.unlines outs)
  rss "after queries"
  vs <- verify h
  putStrLn ("verify: " ++ show (length vs) ++ " problems")
  closeHosi h
  sz <- readProcess "du" ["-sk", worldDir] ""
  putStrLn ("on disk: " ++ takeWhile (/= '\t') sz ++ " KB")
  h2 <- timed "reopen (khana kept; the tokenizer starts again)" (openHosiWith cfg worldDir sch)
  timed "rebuildKhana (words made again from the events)" (rebuildKhana h2)
  rss "after rebuild"
  vs2 <- verify h2
  putStrLn ("verify after rebuild: " ++ show (length vs2) ++ " problems")
  closeHosi h2
  putStrLn "done"
