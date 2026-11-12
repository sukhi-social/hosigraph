{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Khana.Sidecar
-- Description : One long-lived JVM running Datalevin, spoken to one line at a time.
--
-- The native @dtlv exec@ starts a process for every operation, and (in 1.2.10) resolves the
-- aggregates @max@ / @min@ / @distinct@ to @clojure.core@'s.  The standalone jar on a JVM does
-- neither.  A JVM takes a second or more to start, so we start it once and keep it:
--
-- > → {:dir "/abs/khana-dtlv" :schema "{…}" :body "<Clojure text>" :data <EDN>}   one line
-- > ← {:ok "<what the body returned, as text>"}  or  {:err "<message>"}  one line
-- > → {:op :drop :dir "/abs/khana-dtlv"}     close that database      ← {:ok "dropped"}
-- > → {:op :quit}                            close everything, exit   ← {:ok "bye"}
-- > ← {:ready true}                          once, when it has started
--
-- * One request at a time (a lock); the JVM side is a single loop.
-- * The body sees the open connection as @c@, exactly as under @dtlv exec@, so the bodies of
--   "Hosigraph.Khana.Dtlv" are the same for both.  Big data (a transaction) travels in @:data@
--   and the body sees it as @data@: it is read, not compiled (the JVM refuses to compile a method
--   with thousands of literals).  A list @(keyword "…")@ inside it becomes that keyword.  Whatever the body prints goes to standard
--   error, never into the line protocol.
-- * If the JVM dies or answers nonsense, that one call fails ('SidecarError'), the process is
--   put away, and the next call starts a fresh one.  (The khana is rebuildable, and the events
--   are the truth, so a lost connection loses nothing.)
-- * The JVM leaves by itself when our end of the pipe closes.
--
-- Nothing here knows about khanas or facts.
module Hosigraph.Khana.Sidecar
  ( Sidecar, SidecarError(..)
  , newSidecar, sidecarRequest, sidecarRequestIfRunning, stopSidecar
  , sidecarSource
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO
import           System.Process
import           System.Timeout (timeout)

import           Native.Edn (Edn(..), readEdn)

-- | The JVM could not be started, died, or said something we cannot read.
newtype SidecarError = SidecarError String

instance Show SidecarError where show (SidecarError s) = "sidecar: " ++ s

instance Exception SidecarError

-- | A recipe for a JVM (the @java@ command and the Datalevin standalone jar), and the
-- process made from it, if one is running.
data Sidecar = Sidecar
  { scJava  :: FilePath
  , scJar   :: FilePath
  , scState :: MVar (Maybe Live)
  }

data Live = Live
  { lProc :: ProcessHandle
  , lIn   :: Handle
  , lOut  :: Handle
  , lErr  :: IORef [Text]       -- ^ the last few lines the JVM wrote to standard error (newest first)
  }

-- | Nothing is started yet: the JVM comes up with the first request.
newSidecar :: FilePath -> FilePath -> IO Sidecar
newSidecar java jar = Sidecar java jar <$> newMVar Nothing

jvmFlags :: [String]
jvmFlags =
  [ "--add-opens=java.base/java.nio=ALL-UNNAMED"
  , "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"
  , "--enable-native-access=ALL-UNNAMED"
  , "-Dfile.encoding=UTF-8"
  ]

startupSeconds :: Int
startupSeconds = 90

-- | Send one request line and read one reply line; start the JVM first if none is running.
-- (A failure is reported /after/ the lock is given back with the dead JVM forgotten:
-- 'modifyMVar' would put the old state back when an exception leaves it.)
sidecarRequest :: Sidecar -> Text -> IO Text
sidecarRequest sc line = do
  r <- modifyMVar (scState sc) $ \st -> do
    started <- try (maybe (start sc) pure st)
    case started of
      Left (e :: SidecarError) -> pure (Nothing, Left (show e))
      Right live -> do
        a <- try (exchange live line)
        case a of
          Right reply -> pure (Just live, Right reply)
          Left (e :: SomeException) -> do
            msg <- failure live e
            pure (Nothing, Left msg)
  either (throwIO . SidecarError) pure r

-- | Like 'sidecarRequest', but if no JVM is running, do nothing (and start nothing).
sidecarRequestIfRunning :: Sidecar -> Text -> IO (Maybe Text)
sidecarRequestIfRunning sc line = do
  r <- modifyMVar (scState sc) $ \st -> case st of
    Nothing   -> pure (Nothing, Right Nothing)
    Just live -> do
      a <- try (exchange live line)
      case a of
        Right reply -> pure (Just live, Right (Just reply))
        Left (e :: SomeException) -> do
          msg <- failure live e
          pure (Nothing, Left msg)
  either (throwIO . SidecarError) pure r

-- | Ask the JVM to close everything and leave; if it does not within ten seconds, end it.
stopSidecar :: Sidecar -> IO ()
stopSidecar sc = modifyMVar_ (scState sc) $ \st -> do
  forM_ st $ \live -> do
    _ <- try (exchange live "{:op :quit}") :: IO (Either SomeException Text)
    gone <- timeout (10 * 1000000) (waitForProcess (lProc live))
    when (gone == Nothing) (terminateProcess (lProc live) >> void (waitForProcess (lProc live)))
    mapM_ (\h -> hClose h `catch` \(_ :: IOException) -> pure ()) [lIn live, lOut live]
  pure Nothing

exchange :: Live -> Text -> IO Text
exchange live line = do
  TIO.hPutStrLn (lIn live) line                  -- Text all the way: a big transaction is never turned into a String
  hFlush (lIn live)
  TIO.hGetLine (lOut live)

-- | The JVM failed a call: put it away, and say what we know.
failure :: Live -> SomeException -> IO String
failure live e = do
  terminateProcess (lProc live) `catch` \(_ :: IOException) -> pure ()
  _ <- timeout 5000000 (waitForProcess (lProc live))
  mapM_ (\h -> hClose h `catch` \(_ :: IOException) -> pure ()) [lIn live, lOut live]
  threadDelay 100000                                  -- let the stderr reader catch up
  errs <- readIORef (lErr live)
  pure ("the JVM stopped answering (" ++ takeWhile (/= '\n') (displayException e) ++ ")"
        ++ (if null errs then "" else "; its last words: " ++ T.unpack (T.intercalate " | " (reverse (take 8 errs)))))

start :: Sidecar -> IO Live
start sc = do
  r <- try (createProcess (proc (scJava sc) (jvmFlags ++ ["-cp", scJar sc, "clojure.main", "-e", T.unpack sidecarSource]))
              { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  (hin, hout, herr, ph) <- case r of
    Right (Just i, Just o, Just e, p) -> pure (i, o, e, p)
    Right _                            -> throwIO (SidecarError "could not open the pipes")
    Left (e :: IOException)            -> throwIO (SidecarError ("cannot start " ++ show (scJava sc) ++ ": " ++ show e))
  forM_ [hin, hout, herr] $ \h -> hSetEncoding h utf8
  hSetBuffering hin LineBuffering
  hSetNewlineMode hin noNewlineTranslation
  errRef <- newIORef []
  _ <- forkIO $ (forever $ do
         l <- hGetLine herr
         modifyIORef' errRef (take 30 . (T.pack l :))) `catch` \(_ :: SomeException) -> pure ()
  let live = Live ph hin hout errRef
  hello <- try (timeout (startupSeconds * 1000000) (waitReady hout (20 :: Int)))
  case hello of
    Right (Just Nothing)  -> pure live
    Right (Just (Just l)) -> abort live ("an unexpected first line: " ++ take 200 l)
    Right Nothing         -> abort live ("it did not say it was ready within " ++ show startupSeconds ++ " seconds")
    Left (e :: SomeException) -> abort live ("it exited before it was ready (" ++ takeWhile (/= '\n') (displayException e) ++ ")")
  where
    -- stray lines before the ready line are skipped (up to a few)
    waitReady h n = do
      l <- hGetLine h
      if ready (T.pack l) then pure Nothing
        else if n <= 0 then pure (Just l) else waitReady h (n - 1)
    ready l = case readEdn l of
      Right (EMap kvs) -> lookup (EKw "ready") kvs == Just (EBool True)
      _                -> False
    abort live why = do
      msg <- failure live (toException (SidecarError why))
      throwIO (SidecarError ("start: " ++ msg))

-- ---------------------------------------------------------------------------
-- The Clojure side

-- | The program the JVM runs (given with @clojure.main -e@, so there is no file to leave behind).
sidecarSource :: Text
sidecarSource = T.unlines
  [ "(ns-unmap *ns* 'sync)"
  , "(require '[datalevin.core :refer :all] '[clojure.edn :as edn] 'clojure.walk)"
  , "(do (def ^:dynamic c nil) (def ^:dynamic data nil) nil)"      -- `-e` prints every non-nil value it evaluates: keep stdout for the protocol
  , "(defn- fix [x] (clojure.walk/postwalk (fn [y] (if (and (seq? y) (= 'keyword (first y))) (keyword (second y)) y)) x))"
  , "(let [out   (java.io.PrintWriter. (java.io.OutputStreamWriter. System/out \"UTF-8\") true)"
  , "      conns (atom {})"
  , "      say   (fn [m] (binding [*out* out *print-namespace-maps* false] (println (pr-str m))))"
  , "      open  (fn [dir schema]"
  , "              (or (get @conns dir)"
  , "                  (let [x (get-conn dir (eval (read-string schema)))] (swap! conns assoc dir x) x)))"
  , "      shut  (fn [dir] (when-let [x (get @conns dir)] (swap! conns dissoc dir) (close x)))]"
  , "  (say {:ready true})"
  , "  (loop []"
  , "    (when-let [line (read-line)]"
  , "      (let [req (try (edn/read-string line) (catch Throwable e nil))]"
  , "        (cond"
  , "          (not (map? req)) (do (say {:err \"the request is not an EDN map\"}) (recur))"
  , "          (= :quit (:op req)) (do (doseq [d (keys @conns)] (try (shut d) (catch Throwable e nil)))"
  , "                                  (say {:ok \"bye\"}))"
  , "          (= :drop (:op req)) (do (try (shut (:dir req)) (catch Throwable e nil))"
  , "                                  (say {:ok \"dropped\"})"
  , "                                  (recur))"
  , "          :else (do (say (try (let [conn (open (:dir req) (:schema req))"
  , "                                    v    (binding [c conn data (fix (:data req)) *out* *err* *print-namespace-maps* false]"
  , "                                           (pr-str (eval (read-string (str \"(do \" (:body req) \"\\n)\")))))]"
  , "                                {:ok v})"
  , "                              (catch Throwable e {:err (str (or (ex-message e) (class e)))})))"
  , "                    (recur))))))"
  , "  (System/exit 0))"
  ]
