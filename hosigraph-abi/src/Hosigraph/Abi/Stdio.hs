-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Abi.Stdio
-- Description : The door for a process that talks over a pipe: @hosigraph stdio@.
--
-- The same requests and answers as the C door (see "Hosigraph.Abi.Api"), one to a line:
--
-- > stdin:   {:op :write :args {:ops [[:create [:tmp 1] :person/name "Frege"]]}}
-- > stdout:  {:ok {:temps [[1 1234]] :first-seq 1 :last-seq 1 :sealed false}}
--
-- * A request is one line of EDN (UTF-8).  EDN writes a newline inside a string as @\\n@, so a line is
--   never cut in the middle.  A blank line is skipped.
-- * An answer is one line of EDN, written and flushed before the next request is read.  Requests are
--   answered one at a time, in the order they came; a client that wants several answered together
--   uses @write-many@, @query-many@ and the like.
-- * A request that is not the contract (not EDN, an operation nobody offers, arguments of the wrong
--   shape) is answered with an @{:error ...}@ line, and the process goes on.
-- * With @--json@ the lines are JSON instead ("Hosigraph.Abi.Json"); the contract is the same.
-- * End of input closes the database and ends the process.  stdout holds answers and nothing else;
--   what a person might want to read goes to stderr.
module Hosigraph.Abi.Stdio
  ( Format(..)
  , serve
  , runStdio
  ) where

import qualified Control.Exception as E
import           Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Exit (ExitCode(..))
import           System.IO

import           Hosigraph.Abi.Api (api)
import           Hosigraph.Abi.Json
import           Hosigraph.Abi.Library (OpenArgs(..), library, parseOpen)
import           Setu.Abi (Library(..))
import           Setu.Dispatch (Dispatch, Failure(..), failureReply, runRequest)

-- | How a line is written: the EDN of the contract, or JSON ("Hosigraph.Abi.Json").
data Format = FormatEdn | FormatJson { fjIdsAsStrings :: Bool }
  deriving (Eq, Show)

-- | Answer every line of the input, until it ends.  'JsonOptions' is read only for 'FormatJson'.
serve :: Format -> JsonOptions -> Dispatch -> Handle -> Handle -> IO ()
serve fmt jopts d inp out = loop
  where
    loop = do
      eof <- hIsEOF inp
      unless eof $ do
        line <- BC.hGetLine inp
        -- a line ends at \n only, so a client that writes \r\n leaves a \r
        let body = BC.dropWhileEnd (== '\r') line
        unless (BC.all isSpace body) $ do
          answer <- respond body
          BS.hPut out answer
          BS.hPut out "\n"
          hFlush out
        loop
    respond body = case fmt of
      FormatEdn    -> snd <$> runRequest d body
      FormatJson _ -> case jsonRequest api jopts body of
        Left why         -> pure (jsonError "bad-request" why)
        Right (op, edn)  -> jsonReply api jopts op . snd <$> runRequest d edn

-- | Open what the text says (EDN of the shape of @open@: see "Hosigraph.Abi.Api"), answer the
-- input on the output, close.  When it cannot open, the reason is the one line of the output, in the
-- shape of any other error, and the exit code is 1.
runStdio :: Format -> Text -> Handle -> Handle -> IO ExitCode
runStdio fmt openText inp out = do
  hSetBinaryMode inp True
  hSetBinaryMode out True
  opened <- libConnect library openText
  case opened of
    Left failure -> do
      refuse failure
      pure (ExitFailure 1)
    Right (d, release) -> do
      let schema = either (const mempty) oaSchema (parseOpen openText)
          jopts  = JsonOptions { joIdsAsStrings = fmt == FormatJson True, joSchema = schema }
      serve fmt jopts d inp out `E.finally` release
      pure ExitSuccess
  where
    refuse :: Failure -> IO ()
    refuse f = do
      let (_, edn) = failureReply f
          body = case fmt of
            FormatEdn    -> edn
            FormatJson _ -> jsonError (failKind f) (failMessage f)
      BS.hPut out body >> BS.hPut out "\n" >> hFlush out
      hPutStrLn stderr ("hosigraph: could not open (" ++ T.unpack (failKind f) ++ "): " ++ T.unpack (failMessage f))
