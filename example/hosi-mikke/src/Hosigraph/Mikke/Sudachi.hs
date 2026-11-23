-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Mikke.Sudachi
-- Description : A hosigraph 'Tokenizer' for Japanese, from one resident @sudachi@ (sudachi.rs) process.
--
-- The fulltext analyzer of the JVM cuts Han, kana and hangul into overlapping pairs of characters.  It finds a word of two or
-- more characters, but not one character (猫, 雨), not 受付 when the text says 受け付け, and it finds 京都 inside 東京都.  A
-- morphological analyzer cuts a text into words and gives each one a normal form (食べました → 食べる, 引越し → 引っ越し,
-- ネコ → 猫).  This module speaks to @sudachi@ ("https://github.com/WorksApplications/sudachi.rs"), with a dictionary
-- ("https://github.com/WorksApplications/SudachiDict"); neither comes with this package: you bring both (the dictionary is
-- 200 MB, and has terms of its own: see the README).
--
-- > → <one text, on one line>\n                   (as many lines as there are texts)
-- > ← <word>\t<part of speech>\t<normal form>\t…  (one line for each word)
-- > ← EOS                                         (after the words of each line)
--
-- * One request at a time (a lock).  The process starts with the first request; if it dies, that request fails
--   ('SudachiError'), and the next one starts a new process.  When our end of the pipe closes it leaves by itself.
-- * A text becomes the words it is made of, joined by a space: that is what the khana indexes (with the default analyzer
--   of Datalevin, which cuts at the space), and what a search text is made into.  See 'wordsOfAnalysis' for which words.
-- * A newline would end a text early, so it is a space; a text longer than 'chunkChars' characters goes in pieces (a word
--   cut by a piece is two words, at most once in 'chunkChars' characters).
--
-- Nothing here knows about khanas or facts: it makes a 'Tokenizer', which hosigraph asks for words.
--
-- > tk <- newSudachi (SudachiSpec "/path/to/sudachi" "/path/to/system_core.dic" ModeA)
-- > h  <- openHosiWith defaultConfig { hcKhanaBackend = KhanaDtlvRepl dtlv, hcTokenizer = Just tk } dir schema
--
-- The world owns the tokenizer it is opened with: closing it stops the process (and the same tokenizer starts it again,
-- if it is given to another world).
module Hosigraph.Mikke.Sudachi
  ( SudachiSpec(..), SplitMode(..), SudachiError(..)
  , newSudachi, sudachiMark
    -- * Pure (exposed for the tests)
  , wordsOfAnalysis, lineOfText, chunkChars
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (isAlphaNum, isAscii)
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           System.Directory (doesFileExist, executable, findExecutable, getFileSize, getPermissions)
import           System.IO
import           System.Process
import           System.Timeout (timeout)

import           Hosigraph (HosiError(..), Tokenizer(..))
import           Hosigraph.Sha256 (sha256Hex)

-- | How @sudachi@ cuts: A is the shortest unit (東京都 is 東京 and 都), C the longest (東京都庁 stays whole).  A found the
-- most in the tries we made (a search for 東京 finds 東京都庁 only with A), and it is the one to start with.
data SplitMode = ModeA | ModeB | ModeC deriving (Eq, Show)

modeArg :: SplitMode -> String
modeArg m = case m of ModeA -> "A"; ModeB -> "B"; ModeC -> "C"

-- | What makes the words of a text.  Only the words depend on it: a khana made with one is made again for another.
data SudachiSpec = SudachiSpec
  { sudachiExe  :: FilePath     -- ^ the @sudachi@ executable
  , sudachiDict :: FilePath     -- ^ the system dictionary (@system_core.dic@, or a bigger one)
  , sudachiMode :: SplitMode    -- ^ the way it cuts
  } deriving (Eq, Show)

-- | The process could not be started, died, or stopped answering.
newtype SudachiError = SudachiError String

instance Show SudachiError where show (SudachiError s) = "sudachi: " ++ s

instance Exception SudachiError

data Resident = Resident
  { rSpec  :: SudachiSpec
  , rState :: MVar (Maybe Live)
  }

data Live = Live
  { lProc :: ProcessHandle
  , lIn   :: Handle
  , lOut  :: Handle
  , lErr  :: IORef [Text]       -- ^ what the process wrote to standard error (newest first)
  }

-- | A tokenizer made of one @sudachi@.  The program and the dictionary must be there ('BackendMissing' if not: nothing is
-- started yet, and a dictionary is read when the first text is made into words).  The process comes up with the first request.
newSudachi :: SudachiSpec -> IO Tokenizer
newSudachi spec = do
  found <- runnable (sudachiExe spec)
  unless found $ throwIO (BackendMissing ("Sudachi: no executable sudachi at " ++ show (sudachiExe spec)))
  okDict <- doesFileExist (sudachiDict spec)
  unless okDict $ throwIO (BackendMissing ("Sudachi: no dictionary at " ++ show (sudachiDict spec)))
  mark <- sudachiMark spec
  r <- Resident spec <$> newMVar Nothing
  pure Tokenizer { tkMark = mark, tkWords = tokenizeTexts r, tkClose = stopResident r }

-- | A path to an executable file, or a command name found on the PATH.
runnable :: FilePath -> IO Bool
runnable exe
  | '/' `elem` exe = do e <- doesFileExist exe
                        if e then executable <$> getPermissions exe else pure False
  | otherwise      = (/= Nothing) <$> findExecutable exe

-- | What says that two tokenizers make the same words: the rules of this module, the way it cuts, and which dictionary
-- (its size and the first 4 KB, where it keeps the time it was made and its version).  The path of the file is not in it
-- (a dictionary that moved is the same one), and neither is the version of the @sudachi@ program.
sudachiMark :: SudachiSpec -> IO Text
sudachiMark spec = do
  size <- getFileSize (sudachiDict spec)
  hdr  <- withBinaryFile (sudachiDict spec) ReadMode (\h -> B.hGet h 4096)
  pure (T.pack ("sudachi/1:" ++ modeArg (sudachiMode spec) ++ ":" ++ show size ++ ":" ++ take 16 (sha256Hex hdr)))

-- | The most characters of one text we send in one line (@sudachi@ refuses a line that is too long).
chunkChars :: Int
chunkChars = 3000

-- | The lines one text is sent as: a newline is a space, and a long text is cut into pieces.  (An empty text is one empty line.)
lineOfText :: Text -> [Text]
lineOfText t = case T.chunksOf chunkChars (T.map flat t) of
  [] -> [""]
  cs -> cs
  where flat c = if c == '\n' || c == '\r' || c == '\t' then ' ' else c

-- | The words of one line from what @sudachi -a@ printed for it (the lines before its @EOS@): the normal form of each word,
-- lower-cased, except that a word of ASCII letters and digits stays as it was written (the normal form of @Hello@ is
-- ハロー, and a search for @hello@ is a search for @hello@).  Particles, auxiliary verbs, punctuation and white space are left out,
-- and so is a "word" with no letter or digit in it (@sudachi@ calls the colons of a custom emoji, @:name:@ with a zero-width
-- space in each, nouns).
wordsOfAnalysis :: [Text] -> [Text]
wordsOfAnalysis ls = [ w | l <- ls, Just w <- [wordOf (T.splitOn "\t" l)] ]
  where
    wordOf (surface : pos : norm : _)
      | T.null (T.strip surface)               = Nothing
      | T.takeWhile (/= ',') pos `elem` skipped = Nothing
      | otherwise =
          let w = T.toLower (if T.all isAscii surface || T.null norm then surface else norm)
          in if T.any isAlphaNum w then Just w else Nothing
    wordOf _ = Nothing
    skipped = ["助詞", "助動詞", "補助記号", "空白"]

-- | The words of each text, joined by a space, in the order given.
tokenizeTexts :: Resident -> [Text] -> IO [Text]
tokenizeTexts _ [] = pure []
tokenizeTexts tk texts = do
  let pieces = map lineOfText texts
      flat   = concat pieces
  a <- modifyMVar (rState tk) $ \st -> do
    started <- try (maybe (start tk) pure st)
    case started of
      Left (e :: SudachiError) -> pure (Nothing, Left (show e))
      Right live -> do
        x <- try (timeout (requestSeconds (length flat) * 1000000) (exchange live flat))
        case x of
          Right (Just docs) -> pure (Just live, Right docs)
          Right Nothing     -> do msg <- failure live (toException (SudachiError "no answer in time")); pure (Nothing, Left msg)
          Left (e :: SomeException) -> do msg <- failure live e; pure (Nothing, Left msg)
  docs <- either (throwIO . SudachiError) pure a
  pure (regroup (map length pieces) (map (T.unwords . wordsOfAnalysis) docs))
  where
    regroup [] _ = []
    regroup (n : ns) ws = let (a, b) = splitAt n ws in T.unwords (filter (not . T.null) a) : regroup ns b

-- | How long a request may take: a minute, and a second for each thousand lines.
requestSeconds :: Int -> Int
requestSeconds n = 60 + n `div` 1000

-- | Send the lines (the writing is in a thread of its own: the answers are much bigger than the questions, and a
-- process that waits for us to read cannot read more) and read an answer for each.
exchange :: Live -> [Text] -> IO [[Text]]
exchange live ls = do
  _ <- forkIO $ (B.hPut (lIn live) (B.concat [ TE.encodeUtf8 l <> "\n" | l <- ls ]) >> hFlush (lIn live))
                  `catch` \(_ :: SomeException) -> pure ()
  replicateM (length ls) readDoc
  where
    readDoc = go []
      where
        go acc = do
          l <- BC.hGetLine (lOut live)
          let l' = if "\r" `B.isSuffixOf` l then B.init l else l
          if l' == "EOS" then pure (reverse acc) else go (TE.decodeUtf8With TE.lenientDecode l' : acc)

-- | The process failed a request: put it away, and say what we know.
failure :: Live -> SomeException -> IO String
failure live e = do
  terminateProcess (lProc live) `catch` \(_ :: IOException) -> pure ()
  _ <- timeout 5000000 (waitForProcess (lProc live))
  mapM_ (\h -> hClose h `catch` \(_ :: IOException) -> pure ()) [lIn live, lOut live]
  threadDelay 100000                                  -- let the stderr reader catch up
  errs <- readIORef (lErr live)
  pure ("the process stopped answering (" ++ takeWhile (/= '\n') (displayException e) ++ ")"
        ++ (if null errs then "" else "; its last words: " ++ T.unpack (T.intercalate " | " (reverse (take 6 errs)))))

-- | Close our end of the pipe (the process leaves by itself); if it does not within five seconds, end it.
stopResident :: Resident -> IO ()
stopResident tk = modifyMVar_ (rState tk) $ \st -> do
  forM_ st $ \live -> do
    hClose (lIn live) `catch` \(_ :: IOException) -> pure ()
    gone <- timeout 5000000 (waitForProcess (lProc live))
    when (gone == Nothing) (terminateProcess (lProc live) >> void (waitForProcess (lProc live)))
    hClose (lOut live) `catch` \(_ :: IOException) -> pure ()
  pure Nothing

start :: Resident -> IO Live
start tk = do
  let SudachiSpec exe dict mode = rSpec tk
      args = ["-l", dict, "-m", modeArg mode, "-a", "--split-sentences", "no"]
  x <- try (createProcess (proc exe args) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  (hin, hout, herr, ph) <- case x of
    Right (Just i, Just o, Just e, p) -> pure (i, o, e, p)
    Right _                            -> throwIO (SudachiError "could not open the pipes")
    Left (e :: IOException)            -> throwIO (SudachiError ("cannot start " ++ show exe ++ ": " ++ show e))
  forM_ [hin, hout, herr] $ \h -> hSetBinaryMode h True
  hSetBuffering hin (BlockBuffering Nothing)
  errRef <- newIORef []
  _ <- forkIO $ (forever $ do
         l <- BC.hGetLine herr
         modifyIORef' errRef (take 100 . (TE.decodeUtf8With TE.lenientDecode l :))) `catch` \(_ :: SomeException) -> pure ()
  let live = Live ph hin hout errRef
  -- one word, to find out now whether the dictionary can be read
  hello <- try (timeout (60 * 1000000) (exchange live ["東京"]))
  case hello of
    Right (Just [ws]) | not (null (wordsOfAnalysis ws)) -> pure live
    Right (Just _)    -> abort live "an unexpected first answer"
    Right Nothing     -> abort live "it did not come up within a minute"
    Left (e :: SomeException) -> abort live ("it exited before it was ready (" ++ takeWhile (/= '\n') (displayException e) ++ ")")
  where
    abort live why = do
      msg <- failure live (toException (SudachiError why))
      throwIO (SudachiError ("start: " ++ msg))
