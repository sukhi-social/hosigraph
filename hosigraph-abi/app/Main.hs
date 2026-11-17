-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- | @hosigraph stdio [OPEN]@: hosigraph on stdin and stdout (see "Hosigraph.Abi.Stdio").
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.Environment (getArgs)
import           System.Exit
import           System.IO

import           Hosigraph.Abi.Stdio (Format(..), runStdio)

usage :: String
usage = unlines
  [ "usage: hosigraph stdio [--json [--ids-as-strings]] [OPEN]"
  , ""
  , "  Answers requests on stdin, one EDN line each, with one EDN line each on stdout."
  , "  --json writes and reads JSON lines instead of EDN (see docs/guide.md); --ids-as-strings"
  , "  writes integers past 2^53 as strings, for languages that round them."
  , "  OPEN is what the C door's open is given: EDN text such as"
  , "      '{:dir \"/var/lib/hosi\" :schema {:person/name {:type :string}}}'"
  , "  or @FILE to read that text from a file. Without it nothing is opened, and only"
  , "  :ping and :abi are answered. End of input closes the database."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "stdio" : rest  -> stdio rest
    ["-h"]          -> putStr usage
    ["--help"]      -> putStr usage
    _               -> hPutStr stderr usage >> exitWith (ExitFailure 2)
  where
    stdio rest = do
      let (flags, others) = span (`elem` ["--json", "--ids-as-strings"]) rest
          json = "--json" `elem` flags
          ids  = "--ids-as-strings" `elem` flags
      if ids && not json then bad "--ids-as-strings goes with --json" else case others of
        []     -> go (fmt json ids) "{}"
        [open] -> openText open >>= go (fmt json ids)
        _      -> bad "too many arguments"
    fmt json ids = if json then FormatJson ids else FormatEdn
    bad why = hPutStrLn stderr ("hosigraph: " ++ why) >> hPutStr stderr usage >> exitWith (ExitFailure 2)
    go f txt = runStdio f txt stdin stdout >>= exitWith
    openText ('@' : file) = TE.decodeUtf8 <$> BS.readFile file
    openText s            = pure (T.pack s)
