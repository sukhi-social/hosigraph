{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Setu.Gen
-- Description : The @main@ of a generator program: write the files, or check that they are as they should be.
--
-- > main = runMain [("include/hosi.h", cHeader api), ("cbits/hosi_abi.c", cAbi defaultCAbiConfig api), ...]
--
-- > prog --out DIR     write the files under DIR
-- > prog --check DIR   write nothing; exit 1 if a file under DIR differs from what would be written
--
-- The generators are pure functions to 'Text', so what they write is the same each time and
-- can be committed.  @--check@ is what keeps it so: a change to the description that was not
-- followed by a new generation shows up as a failure.
module Setu.Gen
  ( runMain
  , writeFiles
  , checkFiles
  ) where

import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.Environment (getArgs, getProgName)
import           System.Exit
import           System.FilePath (takeDirectory, (</>))
import           System.IO

runMain :: [(FilePath, Text)] -> IO ()
runMain files = do
  args <- getArgs
  case args of
    ["--out", dir]   -> writeFiles dir files >>= mapM_ (putStrLn . ("wrote " ++))
    ["--check", dir] -> do
      bad <- checkFiles dir files
      if null bad
        then putStrLn ("ok: " ++ show (length files) ++ " files are as they should be")
        else do
          mapM_ (\(f, why) -> hPutStrLn stderr (f ++ ": " ++ why)) bad
          hPutStrLn stderr "run the generator with --out to write them again"
          exitFailure
    _ -> do
      prog <- getProgName
      hPutStrLn stderr ("usage: " ++ prog ++ " --out DIR | --check DIR")
      exitWith (ExitFailure 2)

-- | Write each file under the directory (making directories as needed). Gives the paths written.
writeFiles :: FilePath -> [(FilePath, Text)] -> IO [FilePath]
writeFiles dir files = mapM one files
  where
    one (f, t) = do
      let path = dir </> f
      createDirectoryIfMissing True (takeDirectory path)
      B.writeFile path (TE.encodeUtf8 t)
      pure path

-- | The files that are missing or differ, each with why.
checkFiles :: FilePath -> [(FilePath, Text)] -> IO [(FilePath, String)]
checkFiles dir files = concat <$> mapM one files
  where
    one (f, t) = do
      let path = dir </> f
      there <- doesFileExist path
      if not there then pure [(path, "is missing")] else do
        have <- B.readFile path
        pure [ (path, "is not what the description gives" ++ firstDifference (TE.decodeUtf8With lenientDecode have) t)
             | have /= TE.encodeUtf8 t ]

-- | " (line 12 differs)", to say where to look.
firstDifference :: Text -> Text -> String
firstDifference a b = case [ n | (n, x, y) <- zip3 [1 :: Int ..] (pad la) (pad lb), x /= y ] of
  (n : _) -> " (line " ++ show n ++ " differs)"
  []      -> ""
  where
    la = T.lines a
    lb = T.lines b
    pad ls = take (max (length la) (length lb)) (map Just ls ++ repeat Nothing)
