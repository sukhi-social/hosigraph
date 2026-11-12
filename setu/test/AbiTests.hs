{-# LANGUAGE OverloadedStrings #-}
-- | Tests of "Setu.Abi" (connections and handles) and of what "Setu.Gen.HsExport",
-- "Setu.Gen.CAbi" and "Setu.Gen" write.  No C is compiled here; the C that is written is tried
-- by the conformance test of the package that has a library to load.
module AbiTests (abiTests) where

import           Control.Concurrent
import           Control.Concurrent.STM hiding (check)
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.IORef
import           Data.List (nub, sort)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Unique (hashUnique, newUnique)
import           System.Directory (doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)

import           Harness
import           Setu.Abi
import           Setu.Dispatch
import           Setu.Gen
import           Setu.Gen.CAbi
import           Setu.Gen.CHeader
import           Setu.Gen.HsExport
import           Setu.Spec
import           Setu.Ticket
import           Setu.Wire
import           Takka.Edn (Edn(..))

api :: Api
api = Api
  { apiName = "demo", apiVersion = 2, apiAppErr = "APP", apiAppErrDoc = "the library said no"
  , apiOps = [ Op "slow" "Waits until the test lets it go." TNil TNil ] }

-- | A library whose start does what the test says; it counts its releases, and its @slow@
-- operation waits for 'fxGate'.
data Fixture = Fixture
  { fxLib      :: Library
  , fxReleased :: IORef Int
  , fxGate     :: MVar ()
  , fxEntered  :: MVar ()   -- ^ filled when a @slow@ request has begun to run
  }

fixture :: IO (Either Failure ()) -> IO Fixture
fixture connect = do
  released <- newIORef (0 :: Int)
  gate <- newEmptyMVar
  entered <- newEmptyMVar
  let slow _ = tryPutMVar entered () >> readMVar gate >> pure (Right ENil)
      lib = Library
        { libApi = api, libLimit = 4, libWorkers = 2
        , libConnect = \_uri -> do
            r <- connect
            pure $ case r of
              Left f   -> Left f
              Right () -> case newDispatch api [("slow", slow)] of
                Left ps -> Left (Failure StInternal "bad-api" (T.unwords ps))
                Right d -> Right (d, atomicModifyIORef' released (\n -> (n + 1, ())))
        }
  pure (Fixture lib released gate entered)

goodFixture :: IO Fixture
goodFixture = fixture (pure (Right ()))

-- | The answers heard, in the order they came: (id, status, body).
newtype Heard = Heard (TVar [(Id, Status, ByteString)])

newHeard :: IO Heard
newHeard = Heard <$> newTVarIO []

listen :: Heard -> Done
listen (Heard v) i st b = atomically (modifyTVar' v ((i, st, b) :))

listened :: Heard -> IO [(Id, Status, ByteString)]
listened (Heard v) = reverse <$> readTVarIO v

waitHeard :: Heard -> Int -> IO ()
waitHeard (Heard v) n = patiently ("answers (" ++ show n ++ ")") . atomically $ do
  xs <- readTVar v
  when (length xs < n) retry

req :: Text -> ByteString
req = TE.encodeUtf8

-- | A directory that does not exist yet, under the temporary one.
freshDir :: IO FilePath
freshDir = do
  tmp <- getTemporaryDirectory
  n <- hashUnique <$> newUnique
  let dir = tmp ++ "/setu-gen-test-" ++ show n
  there <- doesDirectoryExist dir
  when there (removeDirectoryRecursive dir)
  pure dir

abiTests :: [Test]
abiTests =
  [ ("abi: a request goes to the handler and comes back as an answer", do
      fx <- goodFixture
      abi <- newAbi (fxLib fx)
      h <- abiOpen abi "anything"
      check "a handle is never 0" (h /= 0)
      heard <- newHeard
      checkEq "accepted" SubmitOk =<< abiSubmit abi h 1 (req "{:op :ping :args [1 2]}") (listen heard)
      waitHeard heard 1
      xs <- listened heard
      case xs of
        [(i, st, body)] -> do
          checkEq "id" 1 i
          checkEq "status" StOk st
          checkEq "body" (Right (ReplyOk (EVec [EInt 1, EInt 2]))) (decodeReply body)
        _ -> throwIO (userError ("expected one answer, heard " ++ show (length xs)))
      abiClose abi h)

  , ("abi: a start that is slow does not hold up open or submit; the request is answered after it", do
      started <- newEmptyMVar
      letGo <- newEmptyMVar
      fx <- fixture (putMVar started () >> readMVar letGo >> pure (Right ()))
      abi <- newAbi (fxLib fx)
      h <- patiently "open to return while the start is still going" (abiOpen abi "slow start")
      takeMVar started
      heard <- newHeard
      checkEq "accepted" SubmitOk =<< abiSubmit abi h 1 (req "{:op :ping :args 7}") (listen heard)
      threadDelay 20000
      checkEq "nothing is answered while the start goes on" [] =<< listened heard
      putMVar letGo ()
      waitHeard heard 1
      [(_, st, body)] <- listened heard
      checkEq "status" StOk st
      checkEq "body" (Right (ReplyOk (EInt 7))) (decodeReply body)
      abiClose abi h)

  , ("abi: a start that fails is the answer of every request that waited for it, and of later ones", do
      letGo <- newEmptyMVar
      fx <- fixture (readMVar letGo >> pure (Left (Failure StIo "no-such-directory" "cannot open /nowhere")))
      abi <- newAbi (fxLib fx)
      h <- abiOpen abi "/nowhere"
      heard <- newHeard
      forM_ [1 .. 3] $ \i -> abiSubmit abi h i (req "{:op :ping}") (listen heard) >>= checkEq "accepted" SubmitOk
      putMVar letGo ()
      waitHeard heard 3
      checkEq "accepted later" SubmitOk =<< abiSubmit abi h 9 (req "{:op :abi}") (listen heard)
      waitHeard heard 4
      xs <- listened heard
      forM_ xs $ \(_, st, body) -> do
        checkEq "status" StIo st
        checkEq "body" (Right (ReplyErr "no-such-directory" "cannot open /nowhere")) (decodeReply body)
      abiClose abi h
      checkEq "nothing to release" 0 =<< readIORef (fxReleased fx))

  , ("abi: a start that throws is an internal error; a uri that is not UTF-8 is a protocol error", do
      fx <- fixture (throwIO (ErrorCall "no JVM"))
      abi <- newAbi (fxLib fx)
      h <- abiOpen abi "x"
      heard <- newHeard
      _ <- abiSubmit abi h 1 (req "{:op :ping}") (listen heard)
      waitHeard heard 1
      [(_, st, body)] <- listened heard
      checkEq "status" StInternal st
      case decodeReply body of
        Right (ReplyErr k m) -> do
          checkEq "kind" "open-exception" k
          check "it says what happened" ("no JVM" `T.isInfixOf` m)
        other -> throwIO (userError ("not an error reply: " ++ show other))
      abiClose abi h
      fx2 <- goodFixture
      abi2 <- newAbi (fxLib fx2)
      h2 <- abiOpen abi2 (B.pack [0xff, 0xfe])
      heard2 <- newHeard
      _ <- abiSubmit abi2 h2 1 (req "{:op :ping}") (listen heard2)
      waitHeard heard2 1
      [(_, st2, body2)] <- listened heard2
      checkEq "status" StProtocol st2
      check "kind" (case decodeReply body2 of Right (ReplyErr "bad-open" _) -> True; _ -> False)
      abiClose abi2 h2)

  , ("abi: a handle that is not open is refused, and closing twice is harmless", do
      fx <- goodFixture
      abi <- newAbi (fxLib fx)
      heard <- newHeard
      checkEq "never given" SubmitBadHandle =<< abiSubmit abi 77 1 (req "{:op :ping}") (listen heard)
      checkEq "cancel, never given" False =<< abiCancel abi 77 1
      h <- abiOpen abi "x"
      checkEq "cancel, open (no such id is fine)" True =<< abiCancel abi h 1
      abiClose abi h
      abiClose abi h
      abiClose abi 77
      checkEq "submit, closed" SubmitBadHandle =<< abiSubmit abi h 1 (req "{:op :ping}") (listen heard)
      checkEq "cancel, closed" False =<< abiCancel abi h 1
      checkEq "released once" 1 =<< readIORef (fxReleased fx)
      checkEq "nothing was answered" [] =<< listened heard)

  , ("abi: the refusals of submit are told apart", do
      fx <- goodFixture
      abi <- newAbi (fxLib fx)
      h <- abiOpen abi "x"
      heard <- newHeard
      -- limit 4: all four are pending (two run, two wait) while the gate is shut
      forM_ [1 .. 4] $ \i -> abiSubmit abi h i (req "{:op :slow}") (listen heard) >>= checkEq "accepted" SubmitOk
      checkEq "duplicate" SubmitDuplicate =<< abiSubmit abi h 3 (req "{:op :ping}") (listen heard)
      checkEq "busy" SubmitBusy =<< abiSubmit abi h 5 (req "{:op :ping}") (listen heard)
      putMVar (fxGate fx) ()
      waitHeard heard 4
      abiClose abi h
      checkEq "bad handle" SubmitBadHandle =<< abiSubmit abi h 6 (req "{:op :ping}") (listen heard))

  , ("abi: cancel and close answer with EDN bodies that say why", do
      fx <- goodFixture
      abi <- newAbi (fxLib fx)
      h <- abiOpen abi "x"
      heard <- newHeard
      forM_ [1 .. 3] $ \i -> abiSubmit abi h i (req "{:op :slow}") (listen heard) >>= checkEq "accepted" SubmitOk
      checkEq "cancel found the handle" True =<< abiCancel abi h 1
      waitHeard heard 1
      -- close answers 2 and 3 and then waits for the running work: let it go once all are answered
      _ <- forkIO (waitHeard heard 3 >> putMVar (fxGate fx) ())
      abiClose abi h
      xs <- listened heard
      let by i = [ (st, decodeReply b) | (j, st, b) <- xs, j == i ]
      checkEq "cancelled" [(StCancelled, Right (ReplyErr "cancelled" "the request was cancelled before it ended"))] (by 1)
      forM_ [2, 3] $ \i ->
        checkEq ("closed " ++ show i) [(StClosed, Right (ReplyErr "closed" "the connection was closed before the request ended"))] (by i))

  , ("abi: close returns only after the callbacks have returned and the running work has ended, then releases once", do
      fx <- goodFixture
      abi <- newAbi (fxLib fx)
      h <- abiOpen abi "x"
      events <- newIORef []
      let ev s = atomicModifyIORef' events (\xs -> (s : xs, ()))
      _ <- abiSubmit abi h 1 (req "{:op :slow}") (\_ _ _ -> threadDelay 30000 >> ev "callback")
      -- a request that has not begun when close comes is not run at all; this one has begun
      patiently "the request to begin" (takeMVar (fxEntered fx))
      _ <- forkIO (threadDelay 60000 >> ev "work-let-go" >> putMVar (fxGate fx) ())
      abiClose abi h
      n <- readIORef (fxReleased fx)
      ev ("released-" ++ show n)
      es <- reverse <$> readIORef events
      checkEq "the release comes last, once" ["released-1"] (drop 2 es)
      checkEq "both ended before it" (sort ["callback", "work-let-go"]) (sort (take 2 es)))

  , ("abi: closing while the start goes on waits for it, then lets go of what it made", do
      letGo <- newEmptyMVar
      fx <- fixture (readMVar letGo >> pure (Right ()))
      abi <- newAbi (fxLib fx)
      h <- abiOpen abi "x"
      _ <- forkIO (threadDelay 50000 >> putMVar letGo ())
      patiently "close" (abiClose abi h)
      checkEq "released" 1 =<< readIORef (fxReleased fx))

  , ("abi: closeAll closes every connection and answers what waits", do
      fx <- goodFixture
      abi <- newAbi (fxLib fx)
      hs <- replicateM 3 (abiOpen abi "x")
      checkEq "handles differ" 3 (length (nub hs))
      heard <- newHeard
      forM_ (zip [1 ..] hs) $ \(i, h) -> abiSubmit abi h i (req "{:op :slow}") (listen heard) >>= checkEq "accepted" SubmitOk
      _ <- forkIO (waitHeard heard 3 >> putMVar (fxGate fx) ())
      abiCloseAll abi
      xs <- listened heard
      checkEq "all closed" [StClosed, StClosed, StClosed] [st | (_, st, _) <- xs]
      checkEq "all released" 3 =<< readIORef (fxReleased fx)
      forM_ hs $ \h -> checkEq "gone" SubmitBadHandle =<< abiSubmit abi h 99 (req "{:op :ping}") (listen heard))

  -- ---- what the generators write -------------------------------------------------

  , ("gen: the Haskell module exports the symbols the C layer calls, and nothing else", do
      let hs = hsExport hsCfg api
          syms = hsSymbols api
      checkEq "symbols" ["demo_hs_open", "demo_hs_close", "demo_hs_submit", "demo_hs_cancel", "demo_hs_close_all"] syms
      forM_ syms $ \s -> check (T.unpack s ++ " exported") (("foreign export ccall \"" <> s <> "\"") `T.isInfixOf` hs)
      checkEq "number of exports" (length syms) (T.count "foreign export ccall" hs)
      check "names the module" ("module Demo.Generated () where" `T.isInfixOf` hs)
      check "names the library" ("import           Demo.Library (library)" `T.isInfixOf` hs))

  , ("gen: the C layer defines every function of the header, and declares and calls every Haskell symbol", do
      let c = cAbi defaultCAbiConfig api
          h = cHeader api
      forM_ ["demo_runtime_init", "demo_runtime_shutdown", "demo_open", "demo_close", "demo_submit", "demo_cancel", "demo_thread_done", "demo_abi_version"] $ \f -> do
        check (T.unpack f ++ " is in the header") ((f <> "(") `T.isInfixOf` h)
        check (T.unpack f ++ " is defined in the C layer") ((f <> "(") `T.isInfixOf` c)
      forM_ (hsSymbols api) $ \s ->
        check (T.unpack s ++ " declared and called") (T.count (s <> "(") c >= 2)
      check "the signals stay with the host" ("--install-signal-handlers=no" `T.isInfixOf` c)
      check "includes its own header" ("#include \"demo.h\"" `T.isInfixOf` c))

  , ("gen: the count of arguments to the runtime is right", do
      let argcOf cfg = [ T.takeWhile (/= ';') (T.drop (T.length "    int argc = ") l)
                       | l <- T.lines (cAbi cfg api), "    int argc = " `T.isPrefixOf` l ]
          -- program name, +RTS, the signal flag, the given flags, -RTS
      checkEq "one flag" ["5"] (argcOf defaultCAbiConfig)
      checkEq "three flags" ["7"] (argcOf (CAbiConfig ["-N4", "-A64m", "-qg"])))

  , ("gen: --check sees a file that is missing or differs, and not one that is as written", do
      let files = [("include/demo.h", cHeader api), ("cbits/demo_abi.c", cAbi defaultCAbiConfig api)]
      dir <- freshDir
      missing <- checkFiles dir files
      checkEq "both missing" 2 (length missing)
      written <- writeFiles dir files
      checkEq "two files written" 2 (length written)
      checkEq "as written" [] =<< checkFiles dir files
      _ <- writeFiles dir [("cbits/demo_abi.c", "/* changed */\n")]
      bad <- checkFiles dir files
      checkEq "one differs" 1 (length bad)
      check "it is the C file" (all (T.isInfixOf "demo_abi.c" . T.pack . fst) bad)
      removeDirectoryRecursive dir)
  ]
  where
    hsCfg = HsExportConfig "Demo.Generated" "Demo.Library" "library"
