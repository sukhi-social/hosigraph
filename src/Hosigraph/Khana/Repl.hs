-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Khana.Repl
-- Description : One long-lived native @dtlv repl@, spoken to one form at a time.
--
-- The native @dtlv exec@ starts a process for every operation (about 25 ms each).  @dtlv repl@ reads forms from standard
-- input and keeps running, so we start it once and keep it (about 0.1 ms an operation, measured on 1.2.11):
--
-- > → <one Clojure form, any number of lines>\n
-- > ← <what the form printed, on one line>\n
-- > ← user>
--
-- * The prompt @user> @ ends every answer (at the start of a line, or at the very start of the answer when there is none).
--   The form we send prints a vector of a keyword and a /string/ ('Hosigraph.Khana.Dtlv.script'), so what comes back is a
--   single line with its newlines escaped, and the prompt cannot be part of it.
-- * One request at a time (a lock).
-- * The repl says its errors on standard error, and nothing but the prompt on standard output.  A form of ours catches
--   what the database throws and answers @[:err "..."]@; what the interpreter itself refuses (a form it cannot read, a
--   symbol it does not know) has an empty answer, and then the last words of standard error are the answer.
-- * A form that is not closed makes the repl wait for more, and no prompt comes.  We never send one; if a call
--   fails all the same (the process dies), that one call fails ('DtlvReplError'), the process is put away, and the
--   next call starts a fresh one.  (The khana is rebuildable and the events are the truth, so a lost process loses nothing.)
-- * The repl leaves by itself when our end of the pipe closes /only if no database is open in it/ (tried: with a connection
--   open it was still there ten seconds later).  So 'stopDtlvRepl' is called after the connections were dropped
--   ('Hosigraph.Khana.Dtlv.closeDtlv'), and if it is not (our process dies), the repl stays until somebody kills it.
--
-- Nothing here knows about khanas or facts.
module Hosigraph.Khana.Repl
  ( DtlvRepl, DtlvReplError(..)
  , newDtlvRepl, replRequest, replRequestIfRunning, stopDtlvRepl
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           System.IO
import           System.Process
import           System.Timeout (timeout)

-- | The repl could not be started, died, or stopped answering.
newtype DtlvReplError = DtlvReplError String

instance Show DtlvReplError where show (DtlvReplError s) = "dtlv repl: " ++ s

instance Exception DtlvReplError

-- | The @dtlv@ executable, and the process made from it, if one is running.
data DtlvRepl = DtlvRepl
  { drExe   :: FilePath
  , drState :: MVar (Maybe Live)
  }

data Live = Live
  { lProc :: ProcessHandle
  , lIn   :: Handle
  , lOut  :: Handle
  , lErr  :: IORef [Text]       -- ^ the lines the repl wrote to standard error since the request began (newest first)
  }

-- | Nothing is started yet: the repl comes up with the first request.
newDtlvRepl :: FilePath -> IO DtlvRepl
newDtlvRepl exe = DtlvRepl exe <$> newMVar Nothing

startupSeconds :: Int
startupSeconds = 60

prompt :: B.ByteString
prompt = "\nuser> "

-- | What the repl needs once: the connections it keeps (one per directory), so a request does not open and close
-- the database every time.  (Under @exec@ the script opens and closes it; here it stays open until 'Hosigraph.Khana.Dtlv.closeDtlv'.)
setup :: Text
setup = T.unwords
  [ "(do (def conns (atom {}))"
  , "(defn conn! [d s o] (or (get @conns d) (let [x (get-conn d s o)] (swap! conns assoc d x) x)))"
  , "(defn drop! [d] (when-let [x (get @conns d)] (swap! conns dissoc d) (close x)))"
  , ":ready)" ]

-- | Send one form and read what it printed; start the repl first if none is running.
-- (A failure is reported /after/ the lock is given back with the dead process forgotten:
-- 'modifyMVar' would put the old state back when an exception leaves it.)
replRequest :: DtlvRepl -> Text -> IO Text
replRequest r form = do
  a <- modifyMVar (drState r) $ \st -> do
    started <- try (maybe (start r) pure st)
    case started of
      Left (e :: DtlvReplError) -> pure (Nothing, Left (show e))
      Right live -> do
        x <- try (exchange live form)
        case x of
          Right reply -> pure (Just live, Right reply)
          Left (e :: SomeException) -> do
            msg <- failure live e
            pure (Nothing, Left msg)
  either (throwIO . DtlvReplError) pure a

-- | Like 'replRequest', but if no repl is running, do nothing (and start nothing).
replRequestIfRunning :: DtlvRepl -> Text -> IO (Maybe Text)
replRequestIfRunning r form = do
  a <- modifyMVar (drState r) $ \st -> case st of
    Nothing   -> pure (Nothing, Right Nothing)
    Just live -> do
      x <- try (exchange live form)
      case x of
        Right reply -> pure (Just live, Right (Just reply))
        Left (e :: SomeException) -> do
          msg <- failure live e
          pure (Nothing, Left msg)
  either (throwIO . DtlvReplError) pure a

-- | Close our end of the pipe (the repl leaves by itself); if it does not within ten seconds, end it.
stopDtlvRepl :: DtlvRepl -> IO ()
stopDtlvRepl r = modifyMVar_ (drState r) $ \st -> do
  forM_ st $ \live -> do
    mapM_ (\h -> hClose h `catch` \(_ :: IOException) -> pure ()) [lIn live]
    gone <- timeout (10 * 1000000) (waitForProcess (lProc live))
    when (gone == Nothing) (terminateProcess (lProc live) >> void (waitForProcess (lProc live)))
    hClose (lOut live) `catch` \(_ :: IOException) -> pure ()
  pure Nothing

-- | Write one form (a newline ends it), and read to the prompt.  If the answer is empty, what the repl said on
-- standard error is the answer (it is another pipe: a moment is given for it to arrive).
exchange :: Live -> Text -> IO Text
exchange live form = do
  writeIORef (lErr live) []
  B.hPut (lIn live) (TE.encodeUtf8 form <> "\n")
  hFlush (lIn live)
  out <- readToPrompt (lOut live)
  let said = TE.decodeUtf8With TE.lenientDecode out
  if T.null (T.strip said)
    then do
      threadDelay 50000
      errs <- readIORef (lErr live)
      pure (T.intercalate "\n" (take 6 (reverse errs)))     -- the first words: the message comes before the trace
    else pure said

-- | Read until the prompt; return what came before it.
--
-- A prompt that follows /nothing/ has no newline in front of it: when a form is refused, the repl says so on standard
-- error and its standard output is the bare @user> @.  So the read begins as if a newline had just been written (@lf@),
-- and the prompt is @\\nuser> @ at the end of that.
readToPrompt :: Handle -> IO B.ByteString
readToPrompt h = go [lf] lf
  where
    lf = "\n"
    n = B.length prompt
    -- acc: the chunks so far (newest first, with the first newline at the end); tl: the last few bytes of them, in case
    -- the prompt is split between two chunks
    go acc tl = do
      chunk <- B.hGetSome h 65536
      when (B.null chunk) (throwIO (userError "end of output before the prompt"))
      let tl'  = lastN n (tl <> chunk)
          acc' = chunk : acc
      if prompt `B.isSuffixOf` tl'
        then let whole = B.concat (reverse acc') in pure (B.drop 1 (B.take (B.length whole - n) whole))
        else go acc' tl'
    lastN k b = B.drop (B.length b - k) b

-- | The repl failed a call: put it away, and say what we know.
failure :: Live -> SomeException -> IO String
failure live e = do
  terminateProcess (lProc live) `catch` \(_ :: IOException) -> pure ()
  _ <- timeout 5000000 (waitForProcess (lProc live))
  mapM_ (\h -> hClose h `catch` \(_ :: IOException) -> pure ()) [lIn live, lOut live]
  threadDelay 100000                                  -- let the stderr reader catch up
  errs <- readIORef (lErr live)
  pure ("the repl stopped answering (" ++ takeWhile (/= '\n') (displayException e) ++ ")"
        ++ (if null errs then "" else "; its last words: " ++ T.unpack (T.intercalate " | " (reverse (take 8 errs)))))

start :: DtlvRepl -> IO Live
start r = do
  x <- try (createProcess (proc (drExe r) ["repl"]) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  (hin, hout, herr, ph) <- case x of
    Right (Just i, Just o, Just e, p) -> pure (i, o, e, p)
    Right _                            -> throwIO (DtlvReplError "could not open the pipes")
    Left (e :: IOException)            -> throwIO (DtlvReplError ("cannot start " ++ show (drExe r) ++ ": " ++ show e))
  forM_ [hin, hout, herr] $ \h -> hSetBinaryMode h True
  hSetBuffering hin (BlockBuffering Nothing)
  hSetBuffering hout NoBuffering
  errRef <- newIORef []
  _ <- forkIO $ (forever $ do
         l <- BC.hGetLine herr
         modifyIORef' errRef (take 200 . (TE.decodeUtf8With TE.lenientDecode l :))) `catch` \(_ :: SomeException) -> pure ()
  let live = Live ph hin hout errRef
  hello <- try (timeout (startupSeconds * 1000000) (readToPrompt hout >> exchange live setup))
  case hello of
    Right (Just reply) | T.strip reply == ":ready" -> pure live
    Right (Just reply) -> abort live ("an unexpected first answer: " ++ T.unpack (T.take 200 reply))
    Right Nothing      -> abort live ("it did not come up within " ++ show startupSeconds ++ " seconds")
    Left (e :: SomeException) -> abort live ("it exited before it was ready (" ++ takeWhile (/= '\n') (displayException e) ++ ")")
  where
    abort live why = do
      msg <- failure live (toException (DtlvReplError why))
      throwIO (DtlvReplError ("start: " ++ msg))
