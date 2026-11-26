-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- hosigraph Phase 1 tests.  Plain executable (no test framework needed), QuickCheck for the
-- parts where "for all inputs" is the point (MPHF, chunk files), scenarios for the rest.
module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Exception hiding (assert)
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Bits (xor)
import           Data.Char (isAlphaNum, toLower)
import           Data.IORef
import           Data.Int (Int64)
import           Data.List (sort, sortOn, nub, isInfixOf, isPrefixOf, intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word
import           System.Directory
import           System.Environment (getArgs, getExecutablePath, lookupEnv)
import qualified System.IO as SysIO
import           System.Exit
import           System.FilePath ((</>))
import           System.IO
import           System.Process (CreateProcess(..), StdStream(..), callProcess, createProcess, proc, readProcess, terminateProcess, waitForProcess)
import           System.Timeout (timeout)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Hosigraph
import           Hosigraph.Backfill
import           Hosigraph.Bin
import           Hosigraph.BinaryFuse
import           Hosigraph.Chunk
import qualified Hosigraph.Erase as Erase
import           Takka.Edn (Edn(..), readEdn, renderEdn)
import           Takka.Syntax (Amount(..), Binding(..), Clause(..), Dir(..), Pulled(..), Query(..), Term(..), clauseVars, parseQuery)
import           Hosigraph.Id (epochMs, idMs, idRangeBetween, idSeqNo, makeId)
import           Hosigraph.Khana (metaEntity, schemaMark, schemaMarkAttr, khanaSchema, khanaEntity, closeKhana, newKhanaRuntime, closeKhanaRuntime, openKhana, applyFacts, khanaDatoms, khanaDatomPage, compareKhanas)
import           Takka.Types (Datom(..))
import           Hosigraph.Khana.Repl (newDtlvRepl, stopDtlvRepl)
import           Hosigraph.Khana.Dtlv (DtlvOp(..), Runner(..), closeDtlv, dtlvTransact, ednKw, ednStr, ednVal, openDtlv, schemaEdn, tameEdn)
import           Hosigraph.Manifest
import           Hosigraph.Mphf
import           Hosigraph.Probe (confirmSlot)
import           Hosigraph.WriteQueue
import           Hosigraph.ReadCache
import           Hosigraph.Regions
import qualified System.Directory as SD
import           Control.Concurrent (threadDelay)
import           Control.Exception (try, throwIO, ErrorCall(..), SomeException)
import qualified Data.List as L
import qualified Hosigraph.Events as Ev
import           Hosigraph.Sha256
import           Hosigraph.Tail
import qualified Hosigraph.Event as E

-- ---------------------------------------------------------------------------
-- A very small harness

type Test = (String, IO ())

check :: String -> Bool -> IO ()
check msg ok = unless ok (throwIO (userError msg))

checkEq :: (Eq a, Show a) => String -> a -> a -> IO ()
checkEq msg expected actual =
  unless (expected == actual) $
    throwIO (userError (msg ++ "\n      expected: " ++ show expected ++ "\n      actual:   " ++ show actual))

-- | A walk that never ends is a failure, not a hang (a pager that stands still would loop for ever).
endsIn :: String -> IO a -> IO a
endsIn what act = timeout 20000000 act >>= maybe (throwIO (userError (what ++ ": did not end in 20 s"))) pure

prop :: Testable p => Int -> p -> IO ()
prop n p = do
  r <- quickCheckWithResult stdArgs { maxSuccess = n, chatty = False } p
  unless (isSuccess r) (throwIO (userError (output r)))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--hold-world", dir] -> holdWorld dir        -- a helper for the world-lock tests: not a test run
    ["--make-fixture", dir] -> makeFixture dir    -- writes test/fixtures/world-0.1.0 (see "The shape on disk is a promise"); not a test run
    _                     -> runTests

-- | Run again by the world-lock test as a second process: open this world, say so, and hold it until stdin is closed.
-- (If the test kills it instead, it dies holding the world: that is the point.)
holdWorld :: FilePath -> IO ()
holdWorld dir = do
  ev <- Ev.openEvents Ev.defaultEventsConfig dir
  putStrLn "held" >> hFlush stdout
  _ <- try (hGetLine stdin) :: IO (Either IOException String)
  Ev.closeEvents ev

runTests :: IO ()
runTests = do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  -- The Datalevin khana is tried too: through the native `dtlv` when HOSIGRAPH_DTLV names it, and
  -- through a resident JVM when HOSIGRAPH_DATALEVIN_JAR names the standalone jar (HOSIGRAPH_JAVA
  -- names the java command; "java" if unset).
  mexe  <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_DTLV"
  mjar  <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_DATALEVIN_JAR"
  mjava <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_JAVA"
  let java = maybe "java" id mjava
      tag t = map (\(n, a) -> (t ++ " " ++ n, a))
      replRun = case mexe of
        Just exe -> tag "[dtlv-repl]" (hosiTests (KhanaDtlvRepl exe) ++ formatHosiTests (KhanaDtlvRepl exe) ++ dtlvTests (KhanaDtlvRepl exe) ++ wordsTests (KhanaDtlvRepl exe) ++ nativeTests exe ++ replTests exe)
        Nothing  -> []
      jvmRun = case mjar of
        Just jar -> tag "[jvm]" (hosiTests (KhanaDatalevinJvm java jar) ++ formatHosiTests (KhanaDatalevinJvm java jar) ++ dtlvTests (KhanaDatalevinJvm java jar) ++ wordsTests (KhanaDatalevinJvm java jar) ++ jvmTests java jar)
        Nothing  -> []
      mixedRun = case (mexe, mjar) of
        (Just exe, Just jar) -> tag "[dtlv-repl+jvm]" (mixedTests exe java jar)
        _                    -> []
      dtlvRun = replRun ++ jvmRun ++ mixedRun
  when (null replRun) $ putStrLn "  (dtlv repl tests skipped: set HOSIGRAPH_DTLV to the path of the dtlv executable)"
  when (null jvmRun) $ putStrLn "  (jvm tests skipped: set HOSIGRAPH_DATALEVIN_JAR to the Datalevin standalone jar, and HOSIGRAPH_JAVA to java)"
  -- HOSIGRAPH_TEST_ONLY=text runs only the tests whose name has that text in it (for trying one thing; a full run is the one that counts)
  only <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_TEST_ONLY"
  let wanted (name, _) = maybe True (`isInfixOf` name) only
  results <- forM (filter wanted (pureTests ++ readCacheTests ++ worldLockTests ++ formatTests ++ dtlvRun)) $ \(name, act) -> do
    r <- try act
    case r of
      Right () -> putStrLn ("  ok    " ++ name) >> pure True
      Left (e :: SomeException) -> putStrLn ("  FAIL  " ++ name ++ "\n      " ++ show e) >> pure False
  let failed = length (filter not results)
  putStrLn (show (length results - failed) ++ " passed, " ++ show failed ++ " failed")
  when (failed > 0) exitFailure

nonEmpty :: String -> Maybe String
nonEmpty x = if null x then Nothing else Just x

-- ---------------------------------------------------------------------------
-- One opener at a time

-- | Every path under a directory, with the size of each file: what "nothing was touched" compares.
listAll :: FilePath -> IO [(FilePath, Integer)]
listAll root = go ""
  where
    go rel = do
      names <- sort <$> listDirectory (root </> rel)
      fmap concat . forM names $ \n -> do
        let r = rel </> n
        isDir <- doesDirectoryExist (root </> r)
        if isDir then ((r, -1) :) <$> go r else (\sz -> [(r, sz)]) <$> getFileSize (root </> r)

isWorldBusy :: Either HosiError a -> Bool
isWorldBusy r = case r of Left (WorldBusy _) -> True; _ -> False

worldLockTests :: [Test]
worldLockTests =
  [ ("world lock: a second open of an open world is refused (WorldBusy), in the same process, and touches nothing", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg = Ev.defaultEventsConfig { Ev.ecClock = clock }
          put ev n = Ev.withEventsLock ev (Ev.record ev [Create (Tmp 1) "a" (Lit (VInt n))] >>= \rc -> Ev.commit ev rc)
      ev <- Ev.openEvents cfg dir
      put ev 1
      -- a chunk directory the manifest does not list: the owner may be in the middle of a seal.  A second opener that
      -- recovered would sweep it.
      let making = dir </> "chunks" </> "c-in-the-making"
      createDirectoryIfMissing True making
      B.writeFile (making </> "chunk.hosi") "half"
      before <- listAll dir
      r <- try (Ev.openEvents cfg dir)
      check "refused: WorldBusy" (isWorldBusy r)
      case r of Right ev' -> Ev.closeEvents ev'; Left _ -> pure ()
      checkEq "nothing was touched (not the tail, not the unlisted chunk directory)" before =<< listAll dir
      put ev 2                                                  -- the owner goes on
      Ev.closeEvents ev
      -- let go: the next one opens, and finds both writes of the owner
      ev2 <- Ev.openEvents cfg dir
      checkEq "both writes are there" 2 . length =<< Ev.tailBatches ev2
      Ev.closeEvents ev2)

  , ("world lock: another process holds the world, so it is refused; the process dies without closing, so it is free", withTmp $ \dir -> do
      exe <- getExecutablePath
      (Just hin, Just hout, _, ph) <- createProcess (proc exe ["--hold-world", dir]) { std_in = CreatePipe, std_out = CreatePipe }
      (do said <- hGetLine hout
          checkEq "the other process has it" "held" said
          r <- try (Ev.openEvents Ev.defaultEventsConfig dir)
          check "refused: WorldBusy" (isWorldBusy r)
          case r of Right ev' -> Ev.closeEvents ev'; Left _ -> pure ()
          terminateProcess ph                                   -- SIGTERM: it has no time to close anything
          _ <- waitForProcess ph
          ev <- Ev.openEvents Ev.defaultEventsConfig dir        -- the lock file is still there, and means nothing
          Ev.closeEvents ev) `finally` (hClose hin >> terminateProcess ph >> void (waitForProcess ph)))

  , ("world lock: closing lets go, and a world that failed to open does not keep the lock", withTmp $ \dir -> do
      let cfg = Ev.defaultEventsConfig
      ev <- Ev.openEvents cfg dir
      Ev.closeEvents ev
      ev2 <- Ev.openEvents cfg dir
      Ev.closeEvents ev2
      -- a manifest that cannot be read: the open fails after the lock was taken
      B.writeFile (dir </> manifestFile) "this is not a manifest"
      r <- try (Ev.openEvents cfg dir) :: IO (Either SomeException Ev.Events)
      check "the damaged world does not open" (either (const True) (const False) r)
      check "...but it is not WorldBusy either" (case r of Left e | Just (WorldBusy _) <- fromException e -> False; _ -> True)
      removeFile (dir </> manifestFile)
      ev3 <- Ev.openEvents cfg dir
      Ev.closeEvents ev3)

  , ("open: a khana program that is not there is BackendMissing, found before the world is touched", withTmp $ \dir -> do
      r <- try (openHosiWith defaultConfig { hcKhanaBackend = KhanaDtlvRepl "/no/such/dtlv" } (dir </> "w") schema) :: IO (Either HosiError Hosi)
      check "BackendMissing" (case r of Left (BackendMissing _) -> True; _ -> False)
      checkEq "...and no directory was made" False =<< doesDirectoryExist (dir </> "w"))
  ]

-- ---------------------------------------------------------------------------
-- The shape on disk is a promise
--
-- @test/fixtures/world-0.1.0/@ is a world that 0.1.0 wrote (made by @--make-fixture@ below, and kept).  Every later build
-- must open it, show the events it holds, and (with each khana) answer as it was written: that is what "what 0.1.0 wrote,
-- any 0.1.x reads" means.  If a change makes these tests fail, it is a change of the format, and the format is the promise.
-- Do not make the fixture again to make them pass.

fixtureDir :: FilePath
fixtureDir = "test" </> "fixtures" </> "world-0.1.0"

renderValue :: Value -> String
renderValue v = case v of
  VInt n          -> "i:" ++ show n
  VDouble d       -> "d:" ++ show d
  VStr t          -> "s:" ++ show (T.unpack t)
  VBool b         -> "b:" ++ (if b then "t" else "f")
  VKw (Keyword k) -> "k:" ++ T.unpack k
  VTuple _        -> error "renderValue: a tuple is never in an event"

renderEvent :: E.Event -> String
renderEvent e = unwords [ show (E.evSeq e), E.kindName (E.evKind e), show (E.evId e), attrName, renderValue (E.evVal e) ]
  where attrName = case E.evAttr e of Keyword a -> T.unpack a

-- | Every event the world shows, in order: the sealed ones (the erased are not among them), then the tail.
dumpWorld :: Ev.Events -> IO [String]
dumpWorld ev = do
  sealed <- Ev.withEventsLock ev (Ev.eventsAfter ev 0)
  tl     <- Ev.tailBatches ev
  pure (map renderEvent sealed ++ [ "tail " ++ renderEvent e | b <- tl, e <- b ])

-- | An entity as "now" shows it, one line: attributes in order, the values of each in order.
renderEntity :: HosiId -> M.Map Keyword [Value] -> String
renderEntity i m = unwords ("entity" : show i : [ T.unpack k ++ "=" ++ intercalate "," (sort (map renderValue vs)) | (Keyword k, vs) <- M.toList m ])

-- | Make the fixture world: @root/events/@ and @root/expected.txt@.  It is made at the events layer, so no khana program is needed.
-- The world has: chunks of both versions (October was written as version 1, the way 0.0.1.0 did), a cold chunk (November),
-- an erase record (on the version 1 chunk), and a tail that is not sealed.  The @entity@ lines of @expected.txt@ are written by hand here
-- (what each person should be), not read back from the world.
makeFixture :: FilePath -> IO ()
makeFixture root = do
  removePathForcibly root
  let evDir = root </> "events"
  (clk, clock) <- newClock oct18
  let cfg = Ev.defaultEventsConfig { Ev.ecClock = clock }
      put ev ops = Ev.withEventsLock ev (do rc <- Ev.record ev ops; Ev.commit ev rc; pure (Ev.rcTemps rc))
      str = Lit . VStr
  -- October: three people.  Then the chunks are written again as version 1.
  ev1 <- Ev.openEvents cfg evDir
  t1 <- put ev1 [ Create (Tmp 1) "person/name" (str "Ada"), Create (Tmp 1) "person/age" (Lit (VInt 36))
                , Create (Tmp 2) "person/name" (str "Bo"), Create (Tmp 2) "person/score" (Lit (VDouble 1.5))
                , Create (Tmp 2) "person/friend" (RefTo (Tmp 1)), Create (Tmp 2) "person/tag" (str "x"), Create (Tmp 2) "person/tag" (str "y")
                , Create (Tmp 3) "person/name" (str "Cy") ]
  _ <- Ev.seal ev1
  Ev.closeEvents ev1
  toVersion1 evDir (2026, 10)
  -- November: Dan; Ada is a year older (a newer Create wins); Bo loses a tag
  writeIORef clk nov11
  ev2 <- Ev.openEvents cfg evDir
  let (ada, bo, cy) = (t1 M.! 1, t1 M.! 2, t1 M.! 3)
  t2 <- put ev2 [ Create (Tmp 1) "person/name" (str "Dan"), Create (Tmp 1) "person/friend" (RefTo (Id ada))
                , Create (Id ada) "person/age" (Lit (VInt 37)), Delete (Id bo) "person/tag" (str "y") ]
  _ <- Ev.seal ev2
  -- December: Eve; November goes cold; Cy is erased; and a last batch stays in the tail
  writeIORef clk dec05
  t3 <- put ev2 [ Create (Tmp 1) "person/name" (str "Eve") ]
  _ <- Ev.seal ev2
  _ <- Ev.freezeUnit ev2 (KCreate, (2026, 11))
  _ <- Ev.withEventsLock ev2 (Ev.eraseNow ev2 (Erase.fromIds [cy]))
  t4 <- put ev2 [ Create (Tmp 1) "person/name" (str "Fay"), Create (Id bo) "person/tag" (str "z"), Delete (Id bo) "person/score" (Lit (VDouble 1.5)) ]
  Ev.closeEvents ev2
  ev3 <- Ev.openEvents cfg evDir
  events <- dumpWorld ev3
  Ev.closeEvents ev3
  let (dan, eve, fay) = (t2 M.! 1, t3 M.! 1, t4 M.! 1)
      entity i rows = unwords ("entity" : show i : rows)
  writeFile (root </> "expected.txt") $ unlines $
    [ "# What world-0.1.0 shows.  `event` lines: the events, in order (made by --make-fixture, read back).  `entity` lines: what \"now\" must say (written by hand there)." ]
    ++ [ "event " ++ l | l <- events ]
    ++ [ entity ada ["person/age=i:37", "person/name=s:\"Ada\""]
       , entity bo  ["person/friend=i:" ++ show ada, "person/name=s:\"Bo\"", "person/tag=s:\"x\",s:\"z\""]
       , entity cy  []
       , entity dan ["person/friend=i:" ++ show ada, "person/name=s:\"Dan\""]
       , entity eve ["person/name=s:\"Eve\""]
       , entity fay ["person/name=s:\"Fay\""] ]

-- | Write the Create chunks of one month again as version 1 (what 0.0.1.0 wrote), and let the manifest say so.
toVersion1 :: FilePath -> Month -> IO ()
toVersion1 evDir month = do
  mf <- readManifest evDir
  chunks <- forM (mfChunks mf) $ \c ->
    if ceKind c /= KCreate || ceMonth c /= month then pure c else do
      let old = evDir </> "chunks" </> ceName c
          new = old ++ ".v1"
      evs   <- openChunkReader old >>= readChunkEvents
      ci    <- writeChunkVersion 1 new 4 (ceKind c) (ceMonth c) evs
      files <- hashChunkFiles new
      removePathForcibly old
      renameDirectory new old
      pure (entryFromInfo (ceName c) ci [ FileEntry n b h | (n, b, h) <- files ])
  writeManifest evDir mf { mfChunks = chunks }

-- | The fixture, copied to a place of its own (opening a world writes to it).
withFixture :: (FilePath -> IO a) -> IO a
withFixture act = withTmp $ \tmp -> do
  there <- doesDirectoryExist fixtureDir
  unless there $ throwIO (userError (fixtureDir ++ " is not here: run the tests from the package directory"))
  copyDir fixtureDir (tmp </> "w")
  act (tmp </> "w")

expectedLines :: FilePath -> IO [String]
expectedLines dir = filter (\l -> not (null l) && take 1 l /= "#") . lines <$> readFile (dir </> "expected.txt")

formatTests :: [Test]
formatTests =
  [ ("format: the world 0.1.0 wrote is read as it was written: every event in order, the sealed ones then the tail, the erased person gone; and it verifies", withFixture $ \dir -> do
      expected <- expectedLines dir
      ev <- Ev.openEvents Ev.defaultEventsConfig (dir </> "events")
      got <- dumpWorld ev
      checkEq "the events" [ drop 6 l | l <- expected, take 6 l == "event " ] got
      (readable, problems) <- Ev.withEventsLock ev (Ev.verifyEvents ev)
      checkEq "verify: no problems" (True, []) (readable, problems)
      Ev.closeEvents ev)

  , ("format: the fixture holds what it says: chunks of both versions, a cold chunk, an erase record, a tail that is not sealed", withFixture $ \dir -> do
      let evDir = dir </> "events"
      raw <- B.readFile (evDir </> manifestFile)
      check "the manifest is hosi-manifest/2 (it has an erase record)" ("\"hosi-manifest/2\"" `B.isInfixOf` raw)
      mf <- readManifest evDir
      versions <- forM (mfChunks mf) $ \c -> do
        bs <- B.readFile (evDir </> "chunks" </> ceName c </> chunkDataFile)
        pure (B.take 4 (B.drop 8 bs))                                      -- the version, after the 8 bytes of magic
      checkEq "chunk versions in the world" [B.pack [0, 0, 0, 1], B.pack [0, 0, 0, 2]] (sort (nub versions))
      let cold = [ c | c <- mfChunks mf, ceTier c == "cold" ]
      check "a cold chunk" (not (null cold))
      forM_ cold $ \c -> checkEq "...has no mphf.idx" False =<< doesFileExist (evDir </> "chunks" </> ceName c </> chunkMphfFile)
      let records = [ fe | c <- mfChunks mf, Just fe <- [ceErase c] ]
      check "an erase record" (not (null records))
      forM_ records $ \fe -> checkEq "...and its file is there" True =<< doesFileExist (evDir </> "erase" </> feName fe)
      check "a tail that is not sealed" . (> 0) =<< getFileSize (evDir </> tailFile))

  , ("format: a world written under a newer format is refused, and left as it was (a tail that begins with the reserved HOSI; a manifest of another format)", do
      forM_ [("tail", newerTail), ("manifest", newerManifest)] $ \(what, mutate) -> withFixture $ \dir -> do
        let evDir = dir </> "events"
        mutate evDir
        r1 <- try (Ev.openEvents Ev.defaultEventsConfig evDir)
        check (what ++ ": refused as Corrupt, not opened and not cut") (case r1 of Left (Corrupt _) -> True; _ -> False)
        case r1 of Right ev' -> Ev.closeEvents ev'; Left _ -> pure ()
        before <- filter ((/= "lock") . fst) <$> listAll evDir
        tailBefore <- B.readFile (evDir </> tailFile)
        r2 <- try (Ev.openEvents Ev.defaultEventsConfig evDir)
        check (what ++ ": and again") (case r2 of Left (Corrupt _) -> True; _ -> False)
        case r2 of Right ev' -> Ev.closeEvents ev'; Left _ -> pure ()
        checkEq (what ++ ": every file is as it was") before . filter ((/= "lock") . fst) =<< listAll evDir
        checkEq (what ++ ": the tail is as it was") tailBefore =<< B.readFile (evDir </> tailFile))

  , ("tail: a bad record with a whole one after it is damage in the middle, and is refused (Corrupt) and not cut; a bad last record cannot be told from a torn end, and is cut", withTmp $ \dir -> do
      let ev n = E.Event n KCreate 10 "a" (VInt (fromIntegral n))
          (b1, b2) = ([ev 1], [ev 2])
          path = dir </> tailFile
      tl <- openTail dir False
      starts <- appendBatches tl [b1, b2, [ev 3]]                    -- where each of the three begins
      closeTail tl
      whole <- B.readFile path
      let at2 = fromIntegral (starts !! 1) :: Int
          at3 = fromIntegral (starts !! 2) :: Int
          flipAt n bs = B.concat [B.take n bs, B.singleton (B.index bs n `xor` 0xff), B.drop (n + 1) bs]
          readIt = try (readTail dir) :: IO (Either HosiError ([[E.Event]], Integer))
          kept r = either (const Nothing) Just r
          damaged what n from = do
            B.writeFile path (flipAt n whole)
            r <- readIt
            check (what ++ ": refused as Corrupt, and it says where the bad record begins")
              (case r of Left (Corrupt m) -> ("the record at byte " ++ show from ++ " ") `isInfixOf` m; _ -> False)
            checkEq (what ++ ": the file is as it was") (flipAt n whole) =<< B.readFile path
      damaged "the payload of the second of three" (at2 + 10) at2
      damaged "the CRC of the second of three" (at2 + 4) at2
      damaged "the length of the second of three (it says something huge)" at2 at2
      damaged "the first of three" 10 (0 :: Int)
      -- the last one: nothing follows it, so a flipped byte looks like a crash that left half a record
      B.writeFile path (flipAt (at3 + 10) whole)
      checkEq "the last of three damaged: the first two are kept, the good part ends where it begins" (Just ([b1, b2], fromIntegral at3)) . kept =<< readIt
      B.writeFile path (B.take (B.length whole - 3) whole)
      checkEq "the last of three torn: the same" (Just ([b1, b2], fromIntegral at3)) . kept =<< readIt
      -- through the open: refused, nothing cut, and the lock is let go (the second try is Corrupt too, not WorldBusy)
      B.writeFile path (flipAt (at2 + 10) whole)
      forM_ [1, 2 :: Int] $ \_ -> do
        r <- try (Ev.openEvents Ev.defaultEventsConfig dir)
        check "openEvents: refused as Corrupt" (case r of Left (Corrupt _) -> True; _ -> False)
        case r of Right ev' -> Ev.closeEvents ev'; Left _ -> pure ()
      checkEq "...and the tail is as it was" (flipAt (at2 + 10) whole) =<< B.readFile path)
  ]
  where
    -- a log of a later format: a mark that begins with HOSI, then whatever it likes
    newerTail evDir = do
      t <- B.readFile (evDir </> tailFile)
      B.writeFile (evDir </> tailFile) (B.concat ["HOSITAIL", B.pack [0, 2], t])
    -- the same manifest, said to be of format 9, and signed again so that only the format is wrong
    newerManifest evDir = do
      raw <- B.readFile (evDir </> manifestFile)
      let body  = fst (BC.break (== '\n') raw)
          body' = BC.pack (replaceOnce "hosi-manifest/2" "hosi-manifest/9" (BC.unpack body))
      B.writeFile (evDir </> manifestFile) (B.concat [body', "\nsha256:", BC.pack (sha256Hex body'), "\n"])
    replaceOnce from to s = case [ (a, b) | n <- [0 .. length s], let (a, b) = splitAt n s, from `isPrefixOf` b ] of
      ((a, b) : _) -> a ++ to ++ drop (length from) b
      []           -> error ("replaceOnce: no " ++ from)

-- | The same, with a khana: Hosi opens what 0.1.0 wrote, makes the khana from the events, and answers as written;
-- and new writes go on top of the old world.
formatHosiTests :: KhanaBackend -> [Test]
formatHosiTests be =
  [ ("format: Hosi opens the world 0.1.0 wrote, says what is written for each person, takes new writes on top, and says the same after a reopen", withFixture $ \dir -> do
      expected <- expectedLines dir
      let people = [ (read (words l !! 1) :: HosiId, l) | l <- expected, take 7 l == "entity " ]
          sayAll h = forM_ people $ \(i, l) -> do m <- getEntity h i; checkEq ("entity " ++ show i) l (renderEntity i m)
      h <- openHosiWith (backendCfg be) dir schema
      sayAll h
      checkEq "verify: no problems" [] =<< verify h
      gus <- person h "Gus" 30
      _ <- seal h
      sayAll h
      check "the new one is there" . M.member "person/name" =<< getEntity h gus
      closeHosi h
      h2 <- openHosiWith (backendCfg be) dir schema
      sayAll h2
      m <- getEntity h2 gus
      check "...and Gus is still there after the reopen" (M.member "person/name" m)
      closeHosi h2)
  ]

-- ---------------------------------------------------------------------------
-- Fixtures

schema :: Schema
schema = M.fromList
  [ ("person/name",   identity (attr TString))
  , ("person/age",    attr TLong)
  , ("person/score",  attr TDouble)
  , ("person/friend", many ref)
  , ("person/tag",    many (attr TString)) ]

-- | A fresh directory, removed afterwards.
withTmp :: (FilePath -> IO a) -> IO a
withTmp act = do
  base <- getTemporaryDirectory
  t <- getPOSIXTime
  let dir = base </> ("hosigraph-test-" ++ show (floor (t * 1000000) :: Integer))
  removePathForcibly dir
  createDirectoryIfMissing True dir
  act dir `finally` removePathForcibly dir

-- | A clock we can move by hand.  @day n@ is n days after 2026-01-01 00:00 UTC.
day :: Integer -> Integer
day n = epochMs + n * 86400000

oct18, nov11, dec05 :: Integer
oct18 = day 290
nov11 = day 314
dec05 = day 338

newClock :: Integer -> IO (IORef Integer, Clock)
newClock t0 = do
  r <- newIORef t0
  pure (r, readIORef r)

backendCfg :: KhanaBackend -> HosiConfig
backendCfg be = defaultConfig { hcKhanaBackend = be }

name :: T.Text -> Op
name n = Create (Tmp 1) "person/name" (Lit (VStr n))

-- | Create one person; returns the id.
person :: Hosi -> T.Text -> Integer -> IO HosiId
person h n a = do
  r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr n)), Create (Tmp 1) "person/age" (Lit (VInt (fromIntegral a))) ]
  pure (wrTemps r M.! 1)

-- | Create one thing that points at its owner by @person/friend@, with marker @n@ for its name and @n@ for its age (three facts); returns the id.
ownedThing :: Hosi -> HosiId -> Int -> IO HosiId
ownedThing h who n = do
  r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr (T.pack (marker n)))), Create (Tmp 1) "person/age" (Lit (VInt (fromIntegral n)))
               , Create (Tmp 1) "person/friend" (RefTo (Id who)) ]
  pure (wrTemps r M.! 1)

ageOf :: Hosi -> HosiId -> IO (Maybe [Value])
ageOf h i = M.lookup "person/age" <$> getEntity h i

reopen :: HosiConfig -> FilePath -> Hosi -> IO Hosi
reopen cfg dir h = closeHosi h >> openHosiWith cfg dir schema

-- ---------------------------------------------------------------------------
-- The tests

-- | A range of small ids, so that ranges overlap and touch often; a negative length makes an upside-down pair.
genEraseRange :: Gen (HosiId, HosiId)
genEraseRange = do
  a <- choose (0, 60)
  len <- choose (-1, 9)
  pure (a, a + len)

-- | A range anywhere in the whole of an Int64: the extremes are on purpose.
genWideRange :: Gen (HosiId, HosiId)
genWideRange = do
  let one = frequency [ (4, arbitrary), (2, elements [minBound, minBound + 1, -1, 0, 1, 2, maxBound - 1, maxBound]), (2, choose (0, 1000)) ]
  a <- one
  b <- one
  pure (min a b, max a b)

-- | A chunk entry and an erase record entry to put into manifests (nothing here is read from disk).
manifestChunk :: ChunkEntry
manifestChunk =
  let files = [FileEntry "chunk.hosi" 100 (sha256Hex "a")]
  in ChunkEntry { ceName = "c000001-create-2026-11", ceTier = "hot", ceKind = KCreate, ceMonth = (2026, 11)
                , ceEvents = 4, ceIds = 2, ceMinId = 10, ceMaxId = 20, ceMinSeq = 1, ceMaxSeq = 4
                , ceFiles = files, ceHash = chunkEntryHash files, ceErase = Nothing }

manifestRecord :: FileEntry
manifestRecord = FileEntry "c000001-create-2026-11-1.bin" 40 (sha256Hex "x")

-- | Replace the first @from@ in the bytes by @to@ (it must be there).
swapBytes :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
swapBytes from to bs = case B.breakSubstring from bs of
  (a, b) | B.null b  -> error ("swapBytes: no " ++ show from)
         | otherwise -> a <> to <> B.drop (B.length from) b

-- | Change the body of the manifest file and sign it again, as a program that edited it would.
resignManifest :: FilePath -> (B.ByteString -> B.ByteString) -> IO ()
resignManifest dir f = do
  bs <- B.readFile (dir </> manifestFile)
  let body = f (BC.takeWhile (/= '\n') bs)
  B.writeFile (dir </> manifestFile) (B.concat [body, "\n", "sha256:", BC.pack (sha256Hex body), "\n"])

-- | Give every chunk of the world in @dir@ (closed) an erase record that holds these ids: what the writer of
-- an Erase will do, by hand.  The record is the same for every chunk and not cut to the chunk's range; the
-- reader cuts it.
writeRecordsByHand :: FilePath -> Erase.EraseSet -> IO ()
writeRecordsByHand dir set = do
  mf <- readManifest dir
  createDirectoryIfMissing True (dir </> "erase")
  let bytes = Erase.encode set
  chunks <- forM (mfChunks mf) $ \c -> do
    let fe = FileEntry (ceName c ++ "-1.bin") (B.length bytes) (sha256Hex bytes)
    B.writeFile (dir </> "erase" </> feName fe) bytes
    pure c { ceErase = Just fe }
  writeManifest dir mf { mfChunks = chunks, mfEraseGen = 1 }

-- | A small world on disk, closed: three entities a < b < c made together, and a Delete of one fact of b, sealed
-- into two chunks (Create and Delete).  Every chunk has a record that erases a..b, so c is the one that stays.
erasedWorld :: FilePath -> IO (HosiId, HosiId, HosiId)
erasedWorld dir = do
  (_, clock) <- newClock nov11
  ev <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir
  r <- Ev.withEventsLock ev (Ev.record ev [ Create (Tmp n) "x" (Lit (VInt (fromIntegral n))) | n <- [1, 2, 3] ])
  Ev.withEventsLock ev (Ev.commit ev r)
  let a = Ev.rcTemps r M.! 1
      b = Ev.rcTemps r M.! 2
      c = Ev.rcTemps r M.! 3
  r2 <- Ev.withEventsLock ev (Ev.record ev [Delete (Id b) "x" (Lit (VInt 2))])
  Ev.withEventsLock ev (Ev.commit ev r2)
  _ <- Ev.seal ev
  Ev.closeEvents ev
  writeRecordsByHand dir (Erase.fromRanges [(a, b)])
  pure (a, b, c)

-- | An events world (no khana): three entities made in November, two in December, then one of the November ones
-- deleted; all sealed.  Three chunks: the creations of November (ids n1..n3), its deletion (just n2), the creations of December.
-- Gives the events, left open, and the ids.
twoMonths :: FilePath -> IO (Ev.Events, [HosiId], [HosiId])
twoMonths dir = do
  (clk, clock) <- newClock nov11
  ev <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir
  let put ops = Ev.withEventsLock ev (Ev.record ev ops >>= \rc -> Ev.commit ev rc >> pure rc)
  r1 <- put [ Create (Tmp n) "x" (Lit (VInt (fromIntegral n))) | n <- [1, 2, 3] ]
  writeIORef clk dec05
  r2 <- put [ Create (Tmp n) "x" (Lit (VInt (fromIntegral n))) | n <- [1, 2] ]
  let ns = M.elems (Ev.rcTemps r1)
  _ <- put [Delete (Id (ns !! 1)) "x" (Lit (VInt 2))]
  _ <- Ev.seal ev
  pure (ev, ns, M.elems (Ev.rcTemps r2))

-- | How many times these bytes stand in the files under a directory (all of them, however deep).  The tail log is
-- left out when it is open (GHC does not let one read a file another handle of the same process has open
-- for writing): the tests that count with an open world check that its tail is empty.
markerCount :: B.ByteString -> FilePath -> IO Int
markerCount marker root = do
  names <- filter (/= "tail.log") <$> listDirectory root
  fmap sum . forM names $ \n -> do
    let p = root </> n
    isDir <- doesDirectoryExist p
    if isDir then markerCount marker p else do
      countFile p
  where
    -- read in blocks of 4 MB: on Linux Datalevin's data.mdb is a sparse file of 1 GB, and B.readFile took it whole
    countFile p = SysIO.withBinaryFile p SysIO.ReadMode (\hd -> blocks hd 0 B.empty)
    keep = B.length marker - 1
    blocks hd n carry = do
      blk <- B.hGet hd (4 * 1024 * 1024)
      if B.null blk then pure n else do
        let hay = carry <> blk
        let n' = n + countIn 0 hay
        n' `seq` blocks hd n' (B.copy (B.drop (B.length hay - keep) hay))   -- strict, and a copy: the next round must not hold this block
    countIn n hay =
      let rest = snd (B.breakSubstring marker hay)
      in if B.null rest then n else countIn (n + 1 :: Int) (B.drop (B.length marker) rest)

-- | Copy a directory of plain files.
copyFlat :: FilePath -> FilePath -> IO ()
copyFlat from to = do
  createDirectoryIfMissing True to
  listDirectory from >>= mapM_ (\n -> copyFile (from </> n) (to </> n))

-- | An events world with one chunk of Create events whose values carry a marker text each (the first is the one
-- to keep, the others are for erasing), made at one clock time.  Left open.
markedWorld :: FilePath -> Int -> IO (Ev.Events, [HosiId])
markedWorld dir n = do
  (_, clock) <- newClock nov11
  ev <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir
  r <- Ev.withEventsLock ev (Ev.record ev [ Create (Tmp i) "x" (Lit (VStr (T.pack (marker i)))) | i <- [1 .. n] ] >>= \rc -> Ev.commit ev rc >> pure rc)
  _ <- Ev.seal ev
  pure (ev, [ Ev.rcTemps r M.! i | i <- [1 .. n] ])

marker :: Int -> String
marker i = "marker-" ++ show i ++ "-do-not-leave-me-on-the-disk"

pad6 :: Int -> String
pad6 n = let s = show n in replicate (6 - length s) '0' ++ s

-- | Tests that need no khana at all.
-- | Folding forward, as the khana does: a Create puts a value in (and, for cardinality one, takes the old one out);
-- a Delete takes out that very value, if it is there.  In the backfill tests "t/many" is the one attribute of
-- cardinality many; "t/one" and "t/loose" (which the schema does not know) are cardinality one.
forwardFacts :: [Event] -> S.Set Fact
forwardFacts = foldl' step S.empty
  where
    step s e = case evKind e of
      KCreate | a == "t/many" -> S.insert f s
              | otherwise     -> S.insert f (S.filter (\(i', a', _) -> (i', a') /= (i, a)) s)
      KDelete                 -> S.delete f s
      where f@(i, a, _) = (evId e, evAttr e, evVal e)

backfillSchema :: Schema
backfillSchema = M.fromList [("t/one", attr TLong), ("t/many", many (attr TLong))]

-- | The events newest first, in steps; what each step gives to put in.
backSteps :: [[Event]] -> [[Fact]]
backSteps = go emptyBack
  where go _ []         = []
        go b (st : sts) = let (fs, b') = backStep backfillSchema b st in fs : go b' sts

-- | Cut a list into pieces of the given sizes (the rest is one more piece).
splitBy :: [Int] -> [a] -> [[a]]
splitBy _ [] = []
splitBy [] xs = [xs]
splitBy (n : ns) xs = let (a, b) = splitAt (max 1 n) xs in a : splitBy ns b

genBackEvents :: Gen [Event]
genBackEvents = do
  n <- choose (0, 40)
  forM [1 .. n] $ \s -> do
    k <- frequency [(3, pure KCreate), (2, pure KDelete)]
    i <- elements [10, 11, 12]
    a <- elements ["t/one", "t/many", "t/loose"]
    v <- VInt <$> choose (0, 2)             -- three values only, so that the same fact comes round again and again
    pure (Event (fromIntegral (s :: Int)) k i a v)

readCacheTests :: [Test]
readCacheTests =
  [ ("read cache: the same question is asked of the source once while the answer is fresh; another scope or key is another question", do
      rc <- newReadCache defaultReadCacheConfig
      calls <- newIORef (0 :: Int)
      let ask sc k = cached rc sc k (modifyIORef' calls (+ 1) >> readIORef calls)
      a <- ask "room:1" "newest"
      b <- ask "room:1" "newest"
      c <- ask "room:2" "newest"
      d <- ask "room:1" "count"
      checkEq "the second is the first's answer" a b
      checkEq "two scopes, two questions" 3 =<< readIORef calls
      check "...with different answers" (c /= a && d /= a && c /= d)
      st <- cacheStats rc
      checkEq "one hit" 1 (csHits st)
      checkEq "three misses" 3 (csMisses st)
      checkEq "three kept" 3 (csSize st))
  , ("read cache: invalidate forgets one scope and leaves the others; the next ask goes to the source", do
      rc <- newReadCache defaultReadCacheConfig
      calls <- newIORef (0 :: Int)
      let ask sc = cached rc sc "newest" (modifyIORef' calls (+ 1) >> readIORef calls)
      _ <- ask "room:1"; _ <- ask "room:2"
      invalidate rc "room:1"
      a <- ask "room:1"; b <- ask "room:2"
      checkEq "room 1 was asked again" 3 a
      checkEq "room 2 still has its first answer" 2 b
      st <- cacheStats rc
      checkEq "one forgotten" 1 (csForgotten st)
      invalidateAll rc
      _ <- ask "room:2"
      checkEq "everything was forgotten" 4 =<< readIORef calls)
  , ("read cache: many ask at the same moment and the source is asked once; they all get that answer", do
      rc <- newReadCache defaultReadCacheConfig
      calls <- newIORef (0 :: Int)
      gate <- newEmptyMVar
      let slow = do modifyIORef' calls (+ 1); readMVar gate; pure (42 :: Int)
      vs <- forM [1 .. 20 :: Int] $ \_ -> do
        v <- newEmptyMVar
        _ <- forkIO (cached rc "room:1" "newest" slow >>= putMVar v)
        pure v
      threadDelay 100000                         -- they are all waiting for the one that went
      putMVar gate ()
      rs <- mapM takeMVar vs
      checkEq "all got the answer" (replicate 20 42) rs
      checkEq "the source was asked once" 1 =<< readIORef calls
      st <- cacheStats rc
      checkEq "one miss" 1 (csMisses st)
      checkEq "nineteen shared it" 19 (csShared st))
  , ("read cache: an answer fetched while a write invalidated its scope is not kept (it may predate the write)", do
      rc <- newReadCache defaultReadCacheConfig
      started <- newEmptyMVar
      release <- newEmptyMVar
      out <- newEmptyMVar
      _ <- forkIO (cached rc "room:1" "newest" (putMVar started () >> readMVar release >> pure (1 :: Int)) >>= putMVar out)
      takeMVar started
      invalidate rc "room:1"                     -- the write comes while the answer is on its way
      putMVar release ()
      checkEq "the asker still gets what it fetched" 1 =<< takeMVar out
      st <- cacheStats rc
      checkEq "...but it was not kept" 0 (csSize st)
      checkEq "...and it is counted" 1 (csStaleDropped st)
      r <- cached rc "room:1" "newest" (pure (2 :: Int))
      checkEq "the next ask fetches again" 2 r)
  , ("read cache: the time limit makes an old answer be fetched again; with no limit it stays until invalidated", do
      rc <- newReadCache defaultReadCacheConfig { rcTtlMs = Just 30 }
      calls <- newIORef (0 :: Int)
      let ask = cached rc "r" "k" (modifyIORef' calls (+ 1) >> readIORef calls)
      _ <- ask; _ <- ask
      checkEq "fresh: once" 1 =<< readIORef calls
      threadDelay 80000
      _ <- ask
      checkEq "old: fetched again" 2 =<< readIORef calls
      checkEq "counted as expired" 1 . csExpired =<< cacheStats rc
      rc2 <- newReadCache defaultReadCacheConfig { rcTtlMs = Nothing }
      calls2 <- newIORef (0 :: Int)
      let ask2 = cached rc2 "r" "k" (modifyIORef' calls2 (+ 1) >> readIORef calls2)
      _ <- ask2; threadDelay 60000; _ <- ask2
      checkEq "no limit: still once" 1 =<< readIORef calls2)
  , ("read cache: it is bounded (the oldest answers go first), and the newest stay", do
      rc <- newReadCache defaultReadCacheConfig { rcMaxEntries = 20 }
      forM_ [1 .. 60 :: Int] $ \i -> do
        _ <- cached rc "s" (T.pack (show i)) (pure i)
        threadDelay 1000
      st <- cacheStats rc
      check "never more than the limit" (csSize st <= 20)
      check "some were dropped for room" (csEvictions st > 0)
      calls <- newIORef (0 :: Int)
      _ <- cached rc "s" "60" (modifyIORef' calls (+ 1) >> pure (0 :: Int))
      checkEq "the newest is still there" 0 =<< readIORef calls)
  , ("read cache: a question that fails fails for everyone who asked it, is not kept, and the next ask tries again", do
      rc <- newReadCache defaultReadCacheConfig
      gate <- newEmptyMVar
      let boom = readMVar gate >> throwIO (ErrorCall "khana is away") :: IO Int
      vs <- forM [1 .. 5 :: Int] $ \_ -> do
        v <- newEmptyMVar
        _ <- forkIO (try (cached rc "r" "k" boom) >>= putMVar v)
        pure v
      threadDelay 50000
      putMVar gate ()
      rs <- mapM takeMVar vs
      check "every asker got the failure" (all (either (\e -> "khana is away" `L.isInfixOf` show (e :: SomeException)) (const False)) rs)
      checkEq "nothing was kept" 0 . csSize =<< cacheStats rc
      v <- cached rc "r" "k" (pure (7 :: Int))
      checkEq "the next ask is answered" 7 v)
  , ("read cache: the advice says what the counts suggest, and nothing when all is well", do
      let cfg = defaultReadCacheConfig { rcMaxEntries = 10 }
      rc <- newReadCache cfg
      forM_ [1 .. 40 :: Int] $ \i -> cached rc "s" (T.pack (show i)) (pure i)
      st <- cacheStats rc
      let adv = cacheAdvice cfg st
      check "it tells of the dropped answers" (any ("dropped for room" `L.isInfixOf`) adv)
      rc2 <- newReadCache defaultReadCacheConfig
      forM_ [1 .. 300 :: Int] $ \_ -> cached rc2 "s" "k" (pure (1 :: Int))
      st2 <- cacheStats rc2
      checkEq "nothing to say when it works" [] (cacheAdvice defaultReadCacheConfig st2)
      rc3 <- newReadCache defaultReadCacheConfig
      forM_ [1 .. 300 :: Int] $ \i -> cached rc3 "s" (T.pack (show i)) (pure i)
      st3 <- cacheStats rc3
      check "it tells that nothing repeats" (any ("caching gains little" `L.isInfixOf`) (cacheAdvice defaultReadCacheConfig st3)))
  ]

pureTests :: [Test]
pureTests =
  [ ("sha256: known vectors", do
      checkEq "empty" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (sha256Hex B.empty)
      checkEq "abc" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" (sha256Hex "abc")
      checkEq "two blocks" "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
        (sha256Hex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
      checkEq "10000 patterned bytes" "c3d5cea5e36f10537bb7aeb14e6740a973a9dd5b55c22347d465693f1ded4ced" (sha256Hex (B.pack [ fromIntegral (i * 7 `mod` 251 :: Int) | i <- [0 .. 9999] ])))
  , ("crc32: standard check value", checkEq "crc32 123456789" 0xCBF43926 (crc32 "123456789"))

  , ("backfill: reading newest first, in any steps, gives what folding oldest first gives, and no fact comes twice (QuickCheck)", prop 3000 $
      forAll genBackEvents $ \evs -> forAll (listOf (choose (1, 6))) $ \sizes ->
        let flat = concat (backSteps (splitBy sizes (reverse evs)))
        in conjoin [ length flat === S.size (S.fromList flat)       -- a fact is put in once, and never taken out: what a step gives is final
                   , S.fromList flat === forwardFacts evs ])

  , ("backfill: an older event does not override a newer one; a Delete holds back the older Create; one that takes out nothing does nothing", do
      let e s k a v = Event s k 10 a (VInt v)
          one = "t/one"
          many' = "t/many"
      -- an update before its Create: the update is held, the Create adds nothing for that attribute
      checkEq "update first" [[(10, one, VInt 2)], [(10, "t/loose", VInt 7)]]
        (backSteps [[e 3 KCreate one 2], [e 2 KCreate one 1, e 1 KCreate "t/loose" 7]])
      -- Delete first (the newest word): the Create that comes later is settled as taken out
      checkEq "delete first" [[], []] (backSteps [[e 2 KDelete one 1], [e 1 KCreate one 1]])
      -- a Delete of a value that is not there: the Create still counts
      checkEq "delete of another value" [[], [(10, one, VInt 1)]] (backSteps [[e 2 KDelete one 2], [e 1 KCreate one 1]])
      -- a value put in again after a Delete: the newest word wins
      checkEq "create, delete, create" [[(10, one, VInt 1)], [], []]
        (backSteps [[e 3 KCreate one 1], [e 2 KDelete one 1], [e 1 KCreate one 1]])
      -- cardinality many: each value goes by its own newest word
      checkEq "many" [[(10, many', VInt 2)], [(10, many', VInt 1)]]
        (backSteps [[e 4 KCreate many' 2, e 3 KDelete many' 3], [e 2 KCreate many' 1, e 1 KCreate many' 3]]))

  , ("mphf: any set of distinct keys maps onto [0,n) exactly once (QuickCheck)", prop 300 $
      forAll (resize 400 (listOf arbitrary)) $ \(ks0 :: [Word64]) ->
        let ks = S.toList (S.fromList ks0)
            mp = buildMphf ks
            slots = map (mphfSlot mp) ks
        in sort slots === [0 .. length ks - 1])
  , ("mphf: survives serialization", prop 100 $
      forAll (resize 300 (listOf arbitrary)) $ \(ks0 :: [Word64]) ->
        let ks = S.toList (S.fromList ks0)
            mp = buildMphf ks
        in case runGet getMphf (build (putMphf mp)) 0 of
             Left e    -> counterexample e False
             Right mp' -> map (mphfSlot mp) ks === map (mphfSlot mp') ks)
  , ("mphf: 50,000 keys, including consecutive ids like real ones", do
      let ks = [ fromIntegral (makeId (5 + m) s) | m <- [0 .. 49], s <- [0 .. 999] ] :: [Word64]
          mp = buildMphf ks
      checkEq "bijection" (50000 :: Int) (S.size (S.fromList (map (mphfSlot mp) ks))))

  , ("binary fuse: every key it was built from is a member (QuickCheck); a key given twice counts once", prop 300 $
      forAll (resize 400 (listOf arbitrary)) $ \(ks :: [Word64]) ->
        let f = buildBinaryFuse (ks ++ ks)
        in conjoin [ counterexample ("lost " ++ show k) (bfMember f k) | k <- ks ]
             .&&. bfKeys f === S.size (S.fromList ks))
  , ("binary fuse: survives serialization; the empty filter says no to everything", do
      r <- quickCheckWithResult stdArgs { maxSuccess = 100, chatty = False } $
        forAll (resize 300 (listOf arbitrary)) $ \(ks :: [Word64]) (probes :: [Word64]) ->
          let f = buildBinaryFuse ks
          in case runGet getBinaryFuse (build (putBinaryFuse f)) 0 of
               Left e   -> counterexample e False
               Right f' -> map (bfMember f) (ks ++ probes) === map (bfMember f') (ks ++ probes)
      unless (isSuccess r) (throwIO (userError (output r)))
      let none = buildBinaryFuse []
      check "empty: no" (not (any (bfMember none) [0, 1, 12345, maxBound]))
      check "empty: round trip" (either (const False) (not . flip bfMember 7) (runGet getBinaryFuse (build (putBinaryFuse none)) 0)))
  , ("binary fuse: 50,000 ids like real ones: none lost, about 1 stranger in 65,536 let through, about 19-20 bits a key", do
      let ks = [ fromIntegral (makeId (5 + m) s) | m <- [0 .. 49], s <- [0 .. 999] ] :: [Word64]
          f  = buildBinaryFuse ks
          -- strangers: ids of other months (the same shape), and a million made-up 64-bit numbers
          sameShape = [ fromIntegral (makeId (60 + m) s) | m <- [0 .. 49], s <- [0 .. 999] ] :: [Word64]
          random64  = [ mix64 (i + 0x1234567) | i <- [0 .. 999999] ]
          fp1 = length (filter (bfMember f) sameShape)
          fp2 = length (filter (bfMember f) random64)
      checkEq "none lost" True (all (bfMember f) ks)
      -- expected 0.8 and 15; the limits are many standard deviations away, and the keys are fixed, so this never flickers
      check ("same-shape strangers let through: " ++ show fp1) (fp1 <= 10)
      check ("random strangers let through: " ++ show fp2) (fp2 <= 60)
      let bitsPerKey = fromIntegral (bfBytes f * 8) / 50000 :: Double
      check ("bits per key: " ++ show bitsPerKey) (bitsPerKey < 21))
  , ("binary fuse: small sets of every size from 1 to 40 build, and lose nothing", do
      forM_ [1 .. 40 :: Int] $ \n -> do
        let ks = [ mix64 (fromIntegral i) | i <- [1 .. n] ]
            f  = buildBinaryFuse ks
        check ("n = " ++ show n) (all (bfMember f) ks))
  , ("binary fuse: a cut or odd file is an error, not a filter", do
      let good = build (putBinaryFuse (buildBinaryFuse [mix64 i | i <- [1 .. 100]]))
          bad  = [ ("cut short", B.take (B.length good - 1) good)
                 , ("segment length not a power of two", B.concat [B.take 12 good, be32 12, B.drop 16 good])
                 , ("no segments for keys", B.concat [B.take 16 good, be32 0, B.drop 20 good]) ]
      forM_ bad $ \(what, bs) ->
        check what (either (const True) (const False) (runGet getBinaryFuse bs 0)))

  , ("erase set: members, ranges and the normal form agree with a plain Set of ids (QuickCheck)", prop 500 $
      forAll (listOf genEraseRange) $ \rs ->
        let s = Erase.fromRanges rs
            naive = S.fromList [ i | (a, b) <- rs, i <- [a .. b] ]
            out = Erase.toRanges s
            sorted = and [ a <= b | (a, b) <- out ] && and (zipWith (\(_, b) (c, _) -> b + 1 < c) out (drop 1 out))
        in counterexample ("ranges: " ++ show out) sorted
           .&&. conjoin [ Erase.member i s === S.member i naive | i <- [-3 .. 80] ]
           .&&. S.fromList [ i | (a, b) <- out, i <- [a .. b] ] === naive
           .&&. Erase.fromRanges (reverse (rs ++ rs)) === s            -- order and repeats do not matter
           .&&. Erase.null s === S.null naive)
  , ("erase set: ranges that overlap or touch become one, at the edges of the number line too", do
      checkEq "touching" [(1, 6)] (Erase.toRanges (Erase.fromRanges [(4, 6), (1, 3)]))
      checkEq "overlapping" [(1, 9)] (Erase.toRanges (Erase.fromRanges [(1, 5), (3, 9), (2, 4)]))
      checkEq "one apart stays two" [(1, 3), (5, 6)] (Erase.toRanges (Erase.fromRanges [(1, 3), (5, 6)]))
      checkEq "inside another" [(1, 9)] (Erase.toRanges (Erase.fromRanges [(1, 9), (4, 5)]))
      checkEq "an upside-down pair holds nothing" [] (Erase.toRanges (Erase.fromRanges [(5, 1)]))
      checkEq "at the top (maxBound + 1 would wrap)" [(maxBound - 1, maxBound)]
        (Erase.toRanges (Erase.fromRanges [(maxBound, maxBound), (maxBound - 1, maxBound)]))
      checkEq "at the bottom" [(minBound, 5)] (Erase.toRanges (Erase.fromRanges [(minBound + 2, 5), (minBound, minBound + 1)]))
      check "the largest id is a member" (Erase.member maxBound (Erase.fromIds [maxBound]))
      check "...and its neighbour is not" (not (Erase.member (maxBound - 1) (Erase.fromIds [maxBound]))))
  , ("erase set: union is associative, commutative, idempotent, and has the empty set for zero (QuickCheck)", prop 300 $
      forAll (listOf genEraseRange) $ \xs -> forAll (listOf genEraseRange) $ \ys -> forAll (listOf genEraseRange) $ \zs ->
        let a = Erase.fromRanges xs; b = Erase.fromRanges ys; c = Erase.fromRanges zs
        in Erase.union (Erase.union a b) c === Erase.union a (Erase.union b c)
           .&&. Erase.union a b === Erase.union b a
           .&&. Erase.union a a === a
           .&&. Erase.union a Erase.empty === a
           .&&. Erase.union a b === Erase.fromRanges (xs ++ ys)
           .&&. conjoin [ Erase.member i (Erase.union a b) === (Erase.member i a || Erase.member i b) | i <- [-3 .. 80] ])
  , ("erase set: restrictTo and overlaps agree with a plain Set (QuickCheck)", prop 500 $
      forAll (listOf genEraseRange) $ \rs -> forAll (choose (-3, 70)) $ \lo -> forAll (choose (-3, 14)) $ \len ->
        let s = Erase.fromRanges rs
            hi = lo + len                                                 -- len < 0 asks about an upside-down window
            naive = S.fromList [ i | (a, b) <- rs, i <- [a .. b] ]
            inWindow = S.filter (\i -> lo <= i && i <= hi) naive
            r = Erase.restrictTo (lo, hi) s
        in S.fromList [ i | (a, b) <- Erase.toRanges r, i <- [a .. b] ] === inWindow
           .&&. r === Erase.fromRanges [ (i, i) | i <- S.toList inWindow ]    -- in normal form too
           .&&. Erase.overlaps (lo, hi) s === not (S.null inWindow))
  , ("erase file: any set round-trips, whatever the numbers are, even the whole range of an Int64 (QuickCheck)", prop 500 $
      forAll (listOf genWideRange) $ \rs ->
        let s = Erase.fromRanges rs
        in Erase.decode (Erase.encode s) === Right s)
  , ("erase file: HOSIERS1 is the magic, the numbers are counted from the one before, and a CRC-32 closes it", do
      let body = "HOSIERS1" <> B.pack [2, 5, 0, 4, 2]                       -- two ranges: 5..5, then 4 further on: 10..12
          file = Erase.encode (Erase.fromRanges [(5, 5), (10, 12)])
      checkEq "the bytes" (body <> be32 (crc32 body)) file
      checkEq "the empty set" (Right Erase.empty) (Erase.decode (Erase.encode Erase.empty))
      checkEq "...is 13 bytes" 13 (B.length (Erase.encode Erase.empty)))
  , ("erase file: a changed byte, a cut or longer file, a foreign file, a number that does not fit are all refused", do
      let s = Erase.fromRanges [(2, 2), (300, 70000), (maxBound - 5, maxBound), (minBound, minBound + 3)]
          good = Erase.encode s
          refused = either (const True) (const False) . Erase.decode
          sealed body = body <> be32 (crc32 body)                           -- a file with the right CRC, to reach the other checks
      checkEq "the good one reads" (Right s) (Erase.decode good)
      forM_ [0 .. B.length good - 1] $ \k -> forM_ [0x01, 0xff] $ \m ->
        check ("changing byte " ++ show k ++ " by xor " ++ show m)
          (refused (B.take k good <> B.singleton (B.index good k `xor` m) <> B.drop (k + 1) good))
      forM_ [0 .. B.length good - 1] $ \k -> check ("cut to " ++ show k ++ " bytes") (refused (B.take k good))
      check "one byte more" (refused (good <> B.singleton 0))
      check "another magic" (refused (sealed ("HOSIERS2" <> build (putVar 0))))
      check "a count with nothing behind it" (refused (sealed ("HOSIERS1" <> build (putVar 5))))
      check "a count too large to be true" (refused (sealed ("HOSIERS1" <> build (putVar (2 ^ (40 :: Int))))))
      check "bytes after the last range" (refused (sealed ("HOSIERS1" <> build (putVar 0) <> B.singleton 7)))
      check "a range that touches the one before it"
        (refused (sealed ("HOSIERS1" <> build (putVar 2 <> putVar 5 <> putVar 0 <> putVar 0 <> putVar 0))))
      check "a range past the largest id"
        (refused (sealed ("HOSIERS1" <> build (putVar 1 <> putVar (fromIntegral (maxBound :: Int64)) <> putVar 1))))
      check "a second range past the largest id"
        (refused (sealed ("HOSIERS1" <> build (putVar 2 <> putVar 1 <> putVar 0 <> putVar (fromIntegral (maxBound :: Int64)) <> putVar 0))))
      checkEq "the largest id alone is fine" (Right (Erase.fromIds [maxBound]))
        (Erase.decode (sealed ("HOSIERS1" <> build (putVar 1 <> putVar (fromIntegral (maxBound :: Int64)) <> putVar 0)))))

  , ("manifest: with no erase record nothing changes (hosi-manifest/1, no new key); a record makes it /2; both read back as they were", withTmp $ \dir -> do
      let plain   = Manifest 3 7 4 [manifestChunk] 0
          withRec = plain { mfChunks = [manifestChunk { ceErase = Just manifestRecord }], mfEraseGen = 1 }
          emptied = plain { mfEraseGen = 1 }                   -- the record is gone again, the counter is not
          onDisk m = writeManifest dir m >> B.readFile (dir </> manifestFile)
          has needle = (needle `B.isInfixOf`)
      bs1 <- onDisk plain
      check "a world that never had a record is /1" (has "\"format\":\"hosi-manifest/1\"" bs1)
      check "and shows no trace of erase at all" (not (has "erase" bs1 || has "Erase" bs1))
      checkEq "reads back" plain =<< readManifest dir
      bs2 <- onDisk withRec
      check "a world with a record is /2" (has "\"format\":\"hosi-manifest/2\"" bs2)
      check "with the counter, and the record on its chunk" (has "\"eraseGen\":1" bs2 && has "\"erase\":{" bs2)
      checkEq "reads back" withRec =<< readManifest dir
      bs3 <- onDisk emptied
      check "when the last record is gone the counter keeps the world /2 (so memory and disk say the same)" (has "hosi-manifest/2" bs3)
      checkEq "reads back" emptied =<< readManifest dir)

  , ("manifest: a /1 that lists a record or a counter, a /2 without its counter, and a format from the future are refused", withTmp $ \dir -> do
      let withRec = Manifest 3 7 4 [manifestChunk { ceErase = Just manifestRecord }] 1
          emptied = Manifest 3 7 4 [manifestChunk] 1
          refused what m edit = do
            writeManifest dir m
            resignManifest dir edit
            r <- try (readManifest dir)
            case r of
              Left (Corrupt _) -> pure ()
              Left e           -> throwIO (userError (what ++ ": the wrong error: " ++ show e))
              Right _          -> throwIO (userError (what ++ ": was accepted"))
          toV1 = swapBytes "hosi-manifest/2" "hosi-manifest/1"
      refused "a /1 with a record" withRec toV1
      refused "a /1 with a counter" emptied toV1
      refused "a /2 without its counter" withRec (swapBytes "eraseGen" "eraseGeX")
      -- the same refusal is what a build that knows only /1 does with a /2: it reads the format first
      refused "a format from the future" withRec (swapBytes "hosi-manifest/2" "hosi-manifest/3")
      -- and a change nobody signed is still refused, as before
      writeManifest dir withRec
      bs <- B.readFile (dir </> manifestFile)
      B.writeFile (dir </> manifestFile) (swapBytes "\"eraseGen\":1" "\"eraseGen\":2" bs)
      r <- try (readManifest dir)
      check "an unsigned change to the counter is refused" (case r of Left (Corrupt _) -> True; _ -> False))

  , ("events: the manifest lists a chunk's erase record; open sweeps the files it does not list; verify checks the listed one", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg = Ev.defaultEventsConfig { Ev.ecClock = clock }
      ev <- Ev.openEvents cfg dir
      r <- Ev.withEventsLock ev (Ev.record ev [Create (Tmp 1) "a" (Lit (VInt 1))])
      Ev.withEventsLock ev (Ev.commit ev r)
      Just sr <- Ev.seal ev
      Ev.closeEvents ev
      checkEq "a world that never erased has no erase directory" False =<< doesDirectoryExist (dir </> "erase")
      -- a record for the one chunk, written by hand: the id it holds is gone
      let c     = head (Ev.srChunks sr)
          bytes = Erase.encode (Erase.fromIds [Ev.rcTemps r M.! 1])
          fe    = FileEntry (ceName c ++ "-1.bin") (B.length bytes) (sha256Hex bytes)
          path  = dir </> "erase" </> feName fe
          list m g = writeManifest dir m { mfChunks = [ x { ceErase = Just g } | x <- mfChunks m ], mfEraseGen = 1 }
          verify ev' = snd <$> Ev.withEventsLock ev' (Ev.verifyEvents ev')
      createDirectoryIfMissing True (dir </> "erase")
      B.writeFile path bytes
      readManifest dir >>= \m -> list m fe
      B.writeFile (dir </> "erase" </> "stray-2.bin") bytes                      -- an Erase that never reached the manifest
      B.writeFile (path ++ ".tmp") bytes
      ev2 <- Ev.openEvents cfg dir
      checkEq "the listed record stays; the stray and the temp file go" [feName fe] =<< listDirectory (dir </> "erase")
      checkEq "verify: clean (the chunk's own hash does not mind the record)" [] =<< verify ev2
      -- a byte changed
      let flipAt n b = B.take n b <> B.singleton (B.index b n `xor` 1) <> B.drop (n + 1) b
      B.writeFile path (flipAt 10 bytes)
      ps1 <- verify ev2
      check "a changed byte: the hash does not match" (any ("does not match its recorded hash" `isInfixOf`) ps1)
      check "and it is not an erase record any more (its CRC)" (any ("is not an erase record" `isInfixOf`) ps1)
      B.writeFile path bytes
      checkEq "put back: clean again" [] =<< verify ev2
      -- gone
      removeFile path
      ps2 <- verify ev2
      check "a missing record is reported" (any ("is missing" `isInfixOf`) ps2)
      Ev.closeEvents ev2)

  , ("erase, read side: history and eventsAfter leave out an erased id, its chunks are not even asked, and the other ids stay", withTmp $ \dir -> do
      (a, b, c) <- erasedWorld dir
      ev <- Ev.openEvents Ev.defaultEventsConfig dir
      -- b has a Create and a Delete, in two chunks; both chunks' records have it
      (esB, trB) <- Ev.historyTrace ev b
      checkEq "b: nothing" [] esB
      checkEq "b: both chunks turn it away by their record, before any filter or reader" (2, 0, [])
        (Ev.ltSkippedByErase trB, Ev.ltSkippedByFilter trB, map fst (Ev.ltProbed trB))
      (esA, trA) <- Ev.historyTrace ev a
      checkEq "a: nothing" [] esA
      checkEq "a: only the Create chunk's range holds it, and that chunk's record has it" (1, 0) (Ev.ltSkippedByErase trA, length (Ev.ltProbed trA))
      checkEq "no chunk reader was opened for any of this" 0 . Ev.xsReadersOpen =<< Ev.eventsStats ev
      -- c was not erased
      (esC, trC) <- Ev.historyTrace ev c
      checkEq "c: found" 1 (length esC)
      checkEq "c: nothing was skipped on account of an erase" 0 (Ev.ltSkippedByErase trC)
      -- the events a khana is folded from
      evs <- Ev.eventsAfter ev 0
      checkEq "eventsAfter: only c's event is left" [c] (map evId evs)
      evs' <- Ev.eventsAfter ev (maximum (map evSeq evs) - 1)
      checkEq "and with a seq to start from, the same filter" [c] (map evId evs')
      Ev.closeEvents ev)

  , ("erase, read side: a write to an erased entity is refused, and nothing is written; any other write, even one that only points at it, goes in", withTmp $ \dir -> do
      (a, b, c) <- erasedWorld dir
      (_, clock) <- newClock nov11
      ev <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir
      let attempt ops = try (Ev.withEventsLock ev (Ev.record ev ops >>= \rc -> Ev.commit ev rc >> pure rc)) :: IO (Either HosiError Ev.Recorded)
          wasErased = either (\e -> case e of BadArgument m -> "was erased" `isInfixOf` m; _ -> False) (const False)
          wentIn    = either (const False) (const True)
      forM_ [ ("a Create on an erased entity", [Create (Id a) "x" (Lit (VInt 9))])
            , ("a Delete on one", [Delete (Id b) "x" (Lit (VInt 2))])
            , ("one erased operation among good ones", [Create (Id c) "x" (Lit (VInt 9)), Create (Id b) "x" (Lit (VInt 9))]) ] $ \(what, ops) ->
        check (what ++ " is refused") . wasErased =<< attempt ops
      checkEq "the refused batches wrote nothing" 0 . Ev.xsTailEvents =<< Ev.eventsStats ev
      check "a write to c, which was not erased, goes in" . wentIn =<< attempt [Create (Id c) "x" (Lit (VInt 9))]
      check "a new entity goes in" . wentIn =<< attempt [Create (Tmp 1) "x" (Lit (VInt 1))]
      check "and so does one that only points at an erased entity: a number may stay" . wentIn =<< attempt [Create (Tmp 1) "ref" (RefTo (Id b))]
      checkEq "three events in the tail" 3 . Ev.xsTailEvents =<< Ev.eventsStats ev
      -- recordMany: the erased batch is a Left in its own place and the next one still goes in
      rs <- Ev.withEventsLock ev (Ev.recordMany ev [[Create (Id a) "x" (Lit (VInt 9))], [Create (Tmp 1) "x" (Lit (VInt 5))]])
      check "recordMany: Left, then Right" (case rs of [Left (BadArgument _), Right _] -> True; _ -> False)
      forM_ (take 1 [ r | Right r <- rs ]) (Ev.withEventsLock ev . Ev.rollback ev)
      Ev.closeEvents ev)

  , ("erase, read side: open refuses a world whose listed erase record is missing, changed, or not an erase record", withTmp $ \dir -> do
      _ <- erasedWorld dir
      mf <- readManifest dir
      let c0   = head (mfChunks mf)
          fe   = maybe (error "erasedWorld lists a record on every chunk") id (ceErase c0)
          path = dir </> "erase" </> feName fe
          relist g = writeManifest dir mf { mfChunks = c0 { ceErase = Just g } : tail (mfChunks mf) }
          refused what = do
            r <- try (Ev.openEvents Ev.defaultEventsConfig dir)
            case r of
              Left (Corrupt msg) -> check (what ++ ": the message names the file (" ++ msg ++ ")") (feName fe `isInfixOf` msg)
              Left e             -> throwIO (userError (what ++ ": the wrong error: " ++ show e))
              Right ev           -> Ev.closeEvents ev >> throwIO (userError (what ++ ": the world opened without its record"))
      bytes <- B.readFile path
      removeFile path
      refused "missing"
      B.writeFile path (B.take 10 bytes <> B.singleton (B.index bytes 10 `xor` 1) <> B.drop 11 bytes)
      refused "a byte changed"
      let junk = "this is not an erase record at all" :: B.ByteString
      B.writeFile path junk
      relist fe { feBytes = B.length junk, feSha256 = sha256Hex junk }
      refused "signed by the manifest, but not an erase record"
      -- put back as it was: it opens again
      B.writeFile path bytes
      writeManifest dir mf
      ev <- Ev.openEvents Ev.defaultEventsConfig dir
      Ev.closeEvents ev)

  , ("idRangeBetween: a span of time is one range of ids, from the first of its first millisecond to the last before its end", do
      let t0 = UTCTime (fromGregorian 2026 11 1) 0
          t1 = UTCTime (fromGregorian 2026 12 1) 0
      case idRangeBetween t0 t1 of
        Nothing       -> throwIO (userError "November has ids")
        Just (lo, hi) -> do
          checkEq "the first id is in November, at sequence 0" ((2026, 11), 0) (idMonth lo, idSeqNo lo)
          checkEq "the last is still November" (2026, 11) (idMonth hi)
          checkEq "the next one is December" (2026, 12) (idMonth (hi + 1))
      checkEq "a span that ends where it starts has no ids" Nothing (idRangeBetween t0 t0)
      checkEq "nor one that ends before it starts" Nothing (idRangeBetween t1 t0)
      checkEq "before the first id the generator gives, it starts at the first" (Just (makeId 1 0))
        (fst <$> idRangeBetween (UTCTime (fromGregorian 2000 1 1) 0) t0))

  , ("erase, write side: a record for part of a chunk, the chunk dropped when everything it could hold is covered; each step swaps the manifest once and tidies the files", withTmp $ \dir -> do
      (ev, ns, ds) <- twoMonths dir
      let [n1, n2, n3] = ns
          [d1, _]      = ds
          eraseSet s   = Ev.withEventsLock ev (Ev.eraseNow ev s)
          files d      = sort <$> listDirectory (dir </> d)
          history1 i   = length <$> Ev.history ev i
      -- one id in the middle of the November creations (a record), which is also the whole of the deletion chunk (dropped)
      done1 <- eraseSet (Erase.fromIds [n2])
      checkEq "one chunk dropped, one recorded, generation 1" (1, 1, 1) (length (Ev.edDropped done1), length (Ev.edRecorded done1), Ev.edGeneration done1)
      checkEq "n2 is gone, the others are not" [0, 1, 1, 1] =<< mapM history1 [n2, n1, n3, d1]
      mf1 <- readManifest dir
      checkEq "two chunks left, generation 1" (2, 1) (length (mfChunks mf1), mfEraseGen mf1)
      checkEq "the dropped chunk's directory is gone" 2 . length =<< files "chunks"
      checkEq "one record, named for the chunk and the generation" [ceName c ++ "-1.bin" | c <- mfChunks mf1, ceErase c /= Nothing] =<< files "erase"
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev (Ev.verifyEvents ev)
      -- the same again: nothing to do, nothing written
      m1 <- B.readFile (dir </> manifestFile)
      done2 <- eraseSet (Erase.fromIds [n2])
      checkEq "nothing new: nothing dropped, nothing recorded, still generation 1" ([], [], 1) (Ev.edDropped done2, Ev.edRecorded done2, Ev.edGeneration done2)
      checkEq "and the manifest is the same bytes" m1 =<< B.readFile (dir </> manifestFile)
      -- a second id: the chunk's record is replaced by a bigger one in the new generation
      done3 <- eraseSet (Erase.fromIds [n1])
      checkEq "recorded again, generation 2" (1, 2) (length (Ev.edRecorded done3), Ev.edGeneration done3)
      mf3 <- readManifest dir
      checkEq "only the new record is left" [ceName c ++ "-2.bin" | c <- mfChunks mf3, ceErase c /= Nothing] =<< files "erase"
      -- all of November's creations: the chunk is covered whole, so it goes, and its record with it
      done4 <- eraseSet (Erase.fromRanges [(n1, n3)])
      checkEq "dropped, nothing recorded, generation 3" (1, 0, 3) (length (Ev.edDropped done4), length (Ev.edRecorded done4), Ev.edGeneration done4)
      checkEq "no record is left" [] =<< files "erase"
      mf4 <- readManifest dir
      checkEq "December is all that is left" (1, [ (2026, 12) ]) (length (mfChunks mf4), map ceMonth (mfChunks mf4))
      checkEq "December is untouched" 1 =<< history1 d1
      -- and the same on a world opened again
      Ev.closeEvents ev
      ev2 <- Ev.openEvents Ev.defaultEventsConfig dir
      checkEq "after reopen: November gone, December there" [0, 0, 0, 1] =<< mapM (fmap length . Ev.history ev2) [n1, n2, n3, d1]
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev2 (Ev.verifyEvents ev2)
      Ev.closeEvents ev2)

  , ("erase, write side: the tail is sealed first; a world that only dropped chunks has no erase/", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      ev <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir
      r <- Ev.withEventsLock ev (Ev.record ev [ Create (Tmp n) "x" (Lit (VInt 1)) | n <- [1, 2] ] >>= \rc -> Ev.commit ev rc >> pure rc)
      let [x, y] = M.elems (Ev.rcTemps r)
      checkEq "two events wait in the tail" 2 . Ev.xsTailEvents =<< Ev.eventsStats ev
      done <- Ev.withEventsLock ev (Ev.eraseNow ev (Erase.fromRanges [(0, maxBound)]))
      check "the tail was sealed first" (maybe False (const True) (Ev.edSealed done))
      checkEq "the tail is empty" 0 . Ev.xsTailEvents =<< Ev.eventsStats ev
      checkEq "the one chunk was covered whole: dropped, no record" (1, 0, 1) (length (Ev.edDropped done), length (Ev.edRecorded done), Ev.edGeneration done)
      checkEq "x and y are gone" [0, 0] =<< mapM (fmap length . Ev.history ev) [x, y]
      checkEq "there was never a record, so there is no erase/" False =<< doesDirectoryExist (dir </> "erase")
      mf <- readManifest dir
      checkEq "the world is /2 all the same, with its counter" 1 (mfEraseGen mf)
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev (Ev.verifyEvents ev)
      Ev.closeEvents ev)

  , ("ownedBy: the entities that name an owner in a Create, in any month and even if deleted since; only the attributes asked; the window is on the creation time; the tail is sealed first; nothing is erased", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      ev <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir
      let put ops = Ev.withEventsLock ev (Ev.record ev ops >>= \rc -> Ev.commit ev rc >> pure rc)
          idOf rc n = Ev.rcTemps rc M.! n
          owned attrs owners rng = Ev.withEventsLock ev (Ev.ownedBy ev attrs owners rng)
          everything = (minBound, maxBound)
          authored = ["post/author", "media/owner"]
      who <- put [Create (Tmp 1) "name" (Lit (VStr "A")), Create (Tmp 2) "name" (Lit (VStr "B"))]
      let (a, b) = (idOf who 1, idOf who 2)
      nov <- put [ Create (Tmp 1) "post/author" (RefTo (Id a)), Create (Tmp 1) "post/text" (Lit (VStr "p1"))     -- a's post
                 , Create (Tmp 2) "post/author" (RefTo (Id b))                                                   -- b's post
                 , Create (Tmp 3) "post/author" (RefTo (Id a)), Create (Tmp 3) "post/text" (Lit (VStr "gone"))   -- a's post, deleted below
                 , Create (Tmp 4) "media/owner" (RefTo (Id a))                                                   -- a's media
                 , Create (Tmp 5) "post/mention" (RefTo (Id a)) ]                                                -- mentions a, is not a's
      let [p1, p2, p3, m1, p5] = map (idOf nov) [1 .. 5]
      _ <- put [Delete (Id p3) "post/author" (RefTo (Id a)), Delete (Id p3) "post/text" (Lit (VStr "gone"))]
      writeIORef clk (day 324)
      late <- put [Create (Tmp 1) "post/author" (RefTo (Id a))]                                                  -- a's post of the 21st: the same chunk as the others
      let p6 = idOf late 1
      _ <- Ev.seal ev
      writeIORef clk dec05
      dec <- put [Create (Tmp 1) "post/author" (RefTo (Id a))]                                                   -- a's post of December, still in the tail
      let pd = idOf dec 1
      checkEq "before: one event waits in the tail" 1 . Ev.xsTailEvents =<< Ev.eventsStats ev
      checkEq "a's posts and media, the deleted one too, the December one from the tail; not b's, not the mention"
        (sort [p1, p3, m1, p6, pd]) =<< owned authored [a] everything
      checkEq "...and the tail is sealed now" 0 . Ev.xsTailEvents =<< Ev.eventsStats ev
      let Just novRange = idRangeBetween (UTCTime (fromGregorian 2026 11 1) 0) (UTCTime (fromGregorian 2026 12 1) 0)
          Just decRange = idRangeBetween (UTCTime (fromGregorian 2026 12 1) 0) (UTCTime (fromGregorian 2027 1 1) 0)
          Just firstHalf  = idRangeBetween (UTCTime (fromGregorian 2026 11 1) 0) (UTCTime (fromGregorian 2026 11 15) 0)
          Just secondHalf = idRangeBetween (UTCTime (fromGregorian 2026 11 15) 0) (UTCTime (fromGregorian 2026 12 1) 0)
      checkEq "the window of November" (sort [p1, p3, m1, p6]) =<< owned authored [a] novRange
      checkEq "the window of December" [pd] =<< owned authored [a] decRange
      -- the first half and the second half of November are in one chunk: the window cuts inside it
      checkEq "the first half of November" (sort [p1, p3, m1]) =<< owned authored [a] firstHalf
      checkEq "the second half of November" [p6] =<< owned authored [a] secondHalf
      checkEq "only the attribute asked" [m1] =<< owned ["media/owner"] [a] everything
      checkEq "a mention is found when it is the attribute asked" [p5] =<< owned ["post/mention"] [a] everything
      checkEq "the other owner" [p2] =<< owned authored [b] everything
      checkEq "both owners, in order, once each" (sort [p1, p2, p3, m1, p6, pd]) =<< owned authored [a, b] everything
      checkEq "no attributes: nothing" [] =<< owned [] [a] everything
      checkEq "no owners: nothing" [] =<< owned authored [] everything
      checkEq "an owner nobody names: nothing" [] =<< owned authored [12345] everything
      checkEq "a value that is not a reference to the owner does not count" [] =<< owned ["name"] [a] everything
      checkEq "none of this erased anything" 0 =<< Ev.eraseGeneration ev
      Ev.closeEvents ev)

  , ("rewrite: the chunk with a record is written again without what was erased; the old chunk and the record go; the marker is in no file", withTmp $ \dir -> do
      (ev, [a, b, c]) <- markedWorld dir 3
      let m i = BC.pack (marker i)
          erasing s = Ev.withEventsLock ev (Ev.eraseNow ev s)
          files d = sort <$> listDirectory (dir </> d)
      checkEq "the tail is empty (the chunk was sealed), so the files below are all there is" 0 . Ev.xsTailEvents =<< Ev.eventsStats ev
      checkEq "the three markers are in the chunk" [1, 1, 1] =<< mapM (\i -> markerCount (m i) dir) [1, 2, 3]
      _ <- erasing (Erase.fromIds [c])
      checkEq "erased, not yet rewritten: the bytes are still in the file" 1 =<< markerCount (m 3) dir
      checkEq "c is gone from history all the same" 0 . length =<< Ev.history ev c
      mf0 <- readManifest dir
      let [old] = mfChunks mf0
      -- nothing to rewrite when nothing has a record: a call that does nothing writes nothing
      done <- Ev.rewriteErased ev 10
      checkEq "one chunk rewritten, into the next number" [(ceName old, Just ("c" ++ pad6 (mfNextChunk mf0) ++ drop 7 (ceName old)))] (Ev.rwRewritten done)
      checkEq "nothing skipped, nothing left" (0 :: Int, [] :: [String]) (Ev.rwLeft done, Ev.rwSkipped done)
      mf1 <- readManifest dir
      let [new] = mfChunks mf1
      checkEq "the new chunk holds a and b only: a tighter zone map" (2, 2, a, b) (ceEvents new, ceIds new, ceMinId new, ceMaxId new)
      checkEq "no record any more; the counter stays" (Nothing, 1) (ceErase new, mfEraseGen mf1)
      check "the generation moved" (mfGeneration mf1 > mfGeneration mf0)
      checkEq "chunks/ holds the new chunk only" [ceName new] =<< files "chunks"
      checkEq "erase/ holds nothing" [] =<< files "erase"
      checkEq "the marker of c is in no file; the other two are" [1, 1, 0] =<< mapM (\i -> markerCount (m i) dir) [1, 2, 3]
      checkEq "a and b are found, c is not" [1, 1, 0] =<< mapM (fmap length . Ev.history ev) [a, b, c]
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev (Ev.verifyEvents ev)
      m1 <- B.readFile (dir </> manifestFile)
      done2 <- Ev.rewriteErased ev 10
      checkEq "again: nothing to do" ([] :: [String], [] :: [String], 0 :: Int) (map fst (Ev.rwRewritten done2), Ev.rwSkipped done2, Ev.rwLeft done2)
      checkEq "and the manifest is the same bytes" m1 =<< B.readFile (dir </> manifestFile)
      -- the guard went with the record: the id is a bare number, and a write to it is let in again
      r <- Ev.withEventsLock ev (Ev.record ev [Create (Id c) "x" (Lit (VInt 9))])
      Ev.withEventsLock ev (Ev.commit ev r)
      Ev.closeEvents ev
      ev2 <- Ev.openEvents Ev.defaultEventsConfig dir
      checkEq "after reopen: a and b as before; c has only the new write" [1, 1, 1] =<< mapM (fmap length . Ev.history ev2) [a, b, c]
      Ev.closeEvents ev2)

  , ("rewrite: a chunk none of whose events is left is taken out; a limit rewrites a few at a time", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      ev <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir
      let put ops = Ev.withEventsLock ev (Ev.record ev ops >>= \rc -> Ev.commit ev rc >> pure rc)
          erasing s = Ev.withEventsLock ev (Ev.eraseNow ev s)
      -- x and y a second apart in November: the ids between them are nobody's, so erasing x and y is not "the whole zone map"
      rx <- put [Create (Tmp 1) "x" (Lit (VInt 1))]
      modifyIORef' clk (+ 1000)
      ry <- put [Create (Tmp 1) "x" (Lit (VInt 2))]
      writeIORef clk dec05
      rz <- put [Create (Tmp 1) "x" (Lit (VInt 3))]
      _ <- Ev.seal ev
      let [x] = M.elems (Ev.rcTemps rx); [y] = M.elems (Ev.rcTemps ry); [z] = M.elems (Ev.rcTemps rz)
      done1 <- erasing (Erase.fromIds [x, y, z])
      checkEq "the November chunk has a record (a gap), the December one went whole" (1, 1) (length (Ev.edRecorded done1), length (Ev.edDropped done1))
      mf0 <- readManifest dir
      checkEq "one chunk, with a record" 1 (length [ () | c <- mfChunks mf0, ceErase c /= Nothing ])
      done <- Ev.rewriteErased ev 10
      checkEq "nothing was left of it: no chunk takes its place" 1 (length [ () | (_, Nothing) <- Ev.rwRewritten done ])
      mf1 <- readManifest dir
      checkEq "the world has no chunk" 0 (length (mfChunks mf1))
      checkEq "no directory is left in chunks/" [] =<< listDirectory (dir </> "chunks")
      checkEq "the counter for names did not move for a chunk that was not made" (mfNextChunk mf0) (mfNextChunk mf1)
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev (Ev.verifyEvents ev)
      Ev.closeEvents ev
      -- a limit: two chunks with records, one at a time
      withTmp $ \dir2 -> do
        (ev2, ns, ds) <- twoMonths dir2
        let [_, _, n3] = ns
            [_, d2] = ds
        _ <- Ev.withEventsLock ev2 (Ev.eraseNow ev2 (Erase.fromIds [n3, d2]))
        r1 <- Ev.rewriteErased ev2 1
        checkEq "one at a time: one done, one left" (1, 1) (length (Ev.rwRewritten r1), Ev.rwLeft r1)
        r2 <- Ev.rewriteErased ev2 1
        checkEq "the next call does the other" (1, 0) (length (Ev.rwRewritten r2), Ev.rwLeft r2)
        checkEq "n3 and d2 are gone, the rest is there" [0, 0, 1, 1] =<< mapM (fmap length . Ev.history ev2) [n3, d2, head ns, head ds]
        checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev2 (Ev.verifyEvents ev2)
        Ev.closeEvents ev2)

  , ("rewrite: where it can stop; a directory made, a swap done and the old chunk not yet removed: open sweeps them, and the world is the one the manifest names", withTmp $ \dir -> do
      (ev, [a, b, c]) <- markedWorld dir 3
      _ <- Ev.withEventsLock ev (Ev.eraseNow ev (Erase.fromIds [c]))
      mf0 <- readManifest dir
      let [old] = mfChunks mf0
          Just fe = ceErase old
      -- what the old world's files are, kept aside
      copyFlat (dir </> "chunks" </> ceName old) (dir </> "aside-chunk")
      copyFlat (dir </> "erase") (dir </> "aside-erase")
      -- 1. died after writing the new directory, before the swap: a scratch directory and a named one nobody lists
      Ev.closeEvents ev
      copyFlat (dir </> "aside-chunk") (dir </> "chunks" </> ".rewrite-someone")
      copyFlat (dir </> "aside-chunk") (dir </> "chunks" </> "c000099-create-2026-11")
      ev1 <- Ev.openEvents Ev.defaultEventsConfig dir
      checkEq "open swept both: the old chunk is all that is in chunks/" [ceName old] =<< listDirectory (dir </> "chunks")
      checkEq "the world is the old one: a and b, not c" [1, 1, 0] =<< mapM (fmap length . Ev.history ev1) [a, b, c]
      -- 2. the swap done, the old chunk and its record not yet removed
      done <- Ev.rewriteErased ev1 10
      checkEq "rewritten" 1 (length (Ev.rwRewritten done))
      mf1 <- readManifest dir
      Ev.closeEvents ev1
      copyFlat (dir </> "aside-chunk") (dir </> "chunks" </> ceName old)
      copyFlat (dir </> "aside-erase") (dir </> "erase")
      ev2 <- Ev.openEvents Ev.defaultEventsConfig dir
      checkEq "open swept the old chunk and its record" (map ceName (mfChunks mf1)) =<< listDirectory (dir </> "chunks")
      checkEq "no record is left" False . elem (feName fe) =<< listDirectory (dir </> "erase")
      checkEq "the world is the new one" [1, 1, 0] =<< mapM (fmap length . Ev.history ev2) [a, b, c]
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev2 (Ev.verifyEvents ev2)
      Ev.closeEvents ev2)

  , ("freeze rule: cold is no use for ecColdAfterMs and a hot chunk; a read warms the month; the stamps reach the disk and survive a reopen; a lost or unreadable access.json makes every unit just used; with Nothing nothing is kept", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      let days n = n * 86400000
          cfg = Ev.defaultEventsConfig { Ev.ecClock = clock, Ev.ecColdBlockEvents = 4, Ev.ecColdAfterMs = Just (days 10) }
          nov = (KCreate, (2026, 11))
          dec = (KCreate, (2026, 12))
      ev <- Ev.openEvents cfg dir
      let put ops = Ev.withEventsLock ev (Ev.record ev ops >>= \rc -> Ev.commit ev rc >> pure rc)
      r1 <- put [Create (Tmp 1) "x" (Lit (VInt 1))]
      _ <- Ev.seal ev
      writeIORef clk dec05
      _ <- put [Create (Tmp 1) "x" (Lit (VInt 2))]
      _ <- Ev.seal ev
      let n1 = Ev.rcTemps r1 M.! 1
      checkEq "a seal stamps the write of each unit it made a chunk of" (M.fromList [(nov, Ev.Stamp 0 nov11), (dec, Ev.Stamp 0 dec05)]) =<< Ev.stampsOf ev
      checkEq "it is on the disk already" True =<< doesFileExist (dir </> "access.json")
      writeIORef clk (dec05 + days 1)
      checkEq "a day after December's write: November has been quiet for 25 days" [nov] =<< Ev.coldUnits ev
      _ <- Ev.history ev n1
      checkEq "asking about a November entity warms November (both kinds of it)" [(nov, Ev.Stamp (dec05 + days 1) nov11), ((KDelete, (2026, 11)), Ev.Stamp (dec05 + days 1) 0)] . filter (\((_, m), _) -> m == (2026, 11)) . M.toList =<< Ev.stampsOf ev
      checkEq "...so nothing is cold" [] =<< Ev.coldUnits ev
      writeIORef clk (dec05 + days 11 - 1)
      checkEq "one millisecond short of 10 days from the read: November is warm, December (11 days) is cold" [dec] =<< Ev.coldUnits ev
      writeIORef clk (dec05 + days 11)
      checkEq "exactly 10 days from the read: November is cold too; in order" [nov, dec] =<< Ev.coldUnits ev
      -- the first one only: November, and it has no hot chunk afterwards
      done <- Ev.freezeCold ev 1
      checkEq "freezeCold 1 froze the first unit" [nov] (map Ev.fzUnit done)
      checkEq "the other is left" [dec] =<< Ev.coldUnits ev
      _ <- Ev.history ev n1                                    -- a read after the last write of the file: only a close can bring it to the disk
      before <- Ev.stampsOf ev
      Ev.closeEvents ev
      B.writeFile (dir </> "access.json.tmp") "half a file"      -- a write that died before its rename
      ev2 <- Ev.openEvents cfg dir
      checkEq "open swept the temp file" False =<< doesFileExist (dir </> "access.json.tmp")
      checkEq "after reopen: the same stamps for the units that have a chunk (the Delete one has none, and is left out)" (M.filterWithKey (\(kd, _) _ -> kd == KCreate) before) =<< Ev.stampsOf ev2
      checkEq "and December is still cold: the stamps were not forgotten" [dec] =<< Ev.coldUnits ev2
      Ev.closeEvents ev2
      removeFile (dir </> "access.json")
      ev3 <- Ev.openEvents cfg dir
      checkEq "access.json lost: every unit is just used" (M.fromList [(nov, Ev.Stamp (dec05 + days 11) (dec05 + days 11)), (dec, Ev.Stamp (dec05 + days 11) (dec05 + days 11))]) =<< Ev.stampsOf ev3
      checkEq "...so nothing is cold" [] =<< Ev.coldUnits ev3
      Ev.closeEvents ev3
      B.writeFile (dir </> "access.json") "{ not json"
      ev4 <- Ev.openEvents cfg dir
      checkEq "access.json unreadable: the same" [] =<< Ev.coldUnits ev4
      writeIORef clk (dec05 + days 40)
      checkEq "and it goes cold with time as before" [dec] =<< Ev.coldUnits ev4
      Ev.closeEvents ev4
      -- without ecColdAfterMs: nothing is kept, nothing is cold, nothing is written
      withTmp $ \dir2 -> do
        (_, clock2) <- newClock nov11
        let cfg2 = Ev.defaultEventsConfig { Ev.ecClock = clock2 }
        evN <- Ev.openEvents cfg2 dir2
        rN <- Ev.withEventsLock evN (Ev.record evN [Create (Tmp 1) "x" (Lit (VInt 1))] >>= \rc -> Ev.commit evN rc >> pure rc)
        _ <- Ev.seal evN
        _ <- Ev.history evN (Ev.rcTemps rN M.! 1)
        checkEq "no stamps; no unit is cold; freezeCold does nothing" (M.empty, [], 0 :: Int) =<< ((,,) <$> Ev.stampsOf evN <*> Ev.coldUnits evN <*> (length <$> Ev.freezeCold evN 10))
        Ev.closeEvents evN
        checkEq "no access.json on the disk" False =<< doesFileExist (dir2 </> "access.json"))

  , ("freeze: the hot chunks of one unit become one cold chunk with no MPHF; the other units stay; history and eventsAfter answer the same; a late write makes a hot chunk, the next freeze a second cold one", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      let cfg = Ev.defaultEventsConfig { Ev.ecClock = clock, Ev.ecColdBlockEvents = 4 }
      ev <- Ev.openEvents cfg dir
      let put ops = Ev.withEventsLock ev (Ev.record ev ops >>= \rc -> Ev.commit ev rc >> pure rc)
          idOf rc n = Ev.rcTemps rc M.! n
          unit = (KCreate, (2026, 11))
          isCold c = ceTier c == "cold"
      -- November, sealed twice (two hot Create chunks, and one Delete chunk), and December once
      r1 <- put [ Create (Tmp n) "x" (Lit (VInt (fromIntegral n))) | n <- [1 .. 6] ]
      let ids1 = [ idOf r1 n | n <- [1 .. 6] ]
      _ <- Ev.seal ev
      modifyIORef' clk (+ 1000)
      r2 <- put ([ Create (Id i) "y" (Lit (VStr "more")) | i <- take 3 ids1 ] ++ [ Create (Tmp 1) "x" (Lit (VInt 7)), Delete (Id (head ids1)) "x" (Lit (VInt 1)) ])
      let n7 = idOf r2 1
      _ <- Ev.seal ev
      writeIORef clk dec05
      r3 <- put [ Create (Tmp n) "x" (Lit (VInt 100)) | n <- [1 .. 3] ]
      _ <- Ev.seal ev
      let everyone = ids1 ++ [n7] ++ [ idOf r3 n | n <- [1 .. 3] ]
      mf0 <- readManifest dir
      let hots = [ ceName c | c <- mfChunks mf0, ceKind c == KCreate, ceMonth c == (2026, 11) ]
      checkEq "before: two hot Create chunks in November, four chunks in all" (2, 4) (length hots, length (mfChunks mf0))
      hist0  <- mapM (Ev.history ev) everyone
      after0 <- Ev.withEventsLock ev (Ev.eventsAfter ev 0)
      fz <- Ev.freezeUnit ev unit
      checkEq "the two were put together, into one" (hots, 1, False) (Ev.fzMerged fz, length (Ev.fzInto fz), Ev.fzSkipped fz)
      mf1 <- readManifest dir
      let [c0] = filter isCold (mfChunks mf1)
      checkEq "one cold chunk; the Delete chunk and December's are still hot; three chunks in all" (2, 3) (length (filter (not . isCold) (mfChunks mf1)), length (mfChunks mf1))
      checkEq "the cold chunk holds all of November's Creates (10 events, 7 ids), and only chunk.hosi and filter.bin" (10, 7, [chunkDataFile, chunkFilterFile]) (ceEvents c0, ceIds c0, sort (map feName (ceFiles c0)))
      checkEq "chunks/ holds what the manifest lists, and no scratch" (sort (map ceName (mfChunks mf1))) . sort =<< listDirectory (dir </> "chunks")
      check "the generation moved" (mfGeneration mf1 > mfGeneration mf0)
      checkEq "history is the same for everyone" hist0 =<< mapM (Ev.history ev) everyone
      checkEq "and so is eventsAfter" after0 =<< Ev.withEventsLock ev (Ev.eventsAfter ev 0)
      groupsRef <- newIORef []
      Ev.withEventsLock ev (Ev.eventsAfterEach ev 0 (\g -> modifyIORef groupsRef (g :)))
      groups <- reverse <$> readIORef groupsRef
      checkEq "eventsAfterEach hands over the same events in the same order, one group at a time" after0 (concat groups)
      check "and each group is sorted, and the groups follow each other" (all (\g -> map evSeq g == sort (map evSeq g)) groups
        && and (zipWith (\a b -> maximum (map evSeq a) < minimum (map evSeq b)) groups (drop 1 groups)))
      (_, tr) <- Ev.historyTrace ev (head ids1)
      checkEq "history asks the cold chunk and finds it there" True (ceName c0 `elem` [ n | (n, Star _) <- Ev.ltProbed tr ])
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev (Ev.verifyEvents ev)
      -- again: no hot chunk is left in the unit
      m1 <- B.readFile (dir </> manifestFile)
      fz2 <- Ev.freezeUnit ev unit
      checkEq "again: nothing to do" ([] :: [String], 0 :: Int, False) (Ev.fzMerged fz2, length (Ev.fzInto fz2), Ev.fzSkipped fz2)
      checkEq "and the manifest is the same bytes" m1 =<< B.readFile (dir </> manifestFile)
      -- a late write to a November entity: a new hot chunk in the frozen month; the next freeze puts it in a second cold chunk
      writeIORef clk (day 345)
      _ <- put [Create (Id (ids1 !! 1)) "z" (Lit (VInt 5))]
      _ <- Ev.seal ev
      mf2 <- readManifest dir
      checkEq "a hot chunk again in the frozen month" 1 (length [ () | c <- mfChunks mf2, ceKind c == KCreate, ceMonth c == (2026, 11), not (isCold c) ])
      fz3 <- Ev.freezeUnit ev unit
      mf3 <- readManifest dir
      checkEq "a second cold chunk; the first one is the same entry" (2, True) (length (filter isCold (mfChunks mf3)), c0 `elem` mfChunks mf3)
      checkEq "the entity has its new event too, from the other cold chunk" (length (hist0 !! 1) + 1) . length =<< Ev.history ev (ids1 !! 1)
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev (Ev.verifyEvents ev)
      Ev.closeEvents ev
      ev2 <- Ev.openEvents cfg dir
      checkEq "after reopen: the same answers" (hist0 !! 0 : [], True) . (\hs -> (take 1 hs, length (hs !! 1) == length (hist0 !! 1) + 1)) =<< mapM (Ev.history ev2) everyone
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev2 (Ev.verifyEvents ev2)
      Ev.closeEvents ev2)

  , ("freeze: it reads through the erase records: what was erased is in no file afterwards and the records go; a unit with nothing left makes no cold chunk", withTmp $ \dir -> do
      (ev, [a, b, c]) <- markedWorld dir 3
      let m i = BC.pack (marker i)
      _ <- Ev.withEventsLock ev (Ev.eraseNow ev (Erase.fromIds [c]))
      checkEq "erased, still hot: the bytes of c are in the chunk" 1 =<< markerCount (m 3) dir
      fz <- Ev.freezeUnit ev (KCreate, (2026, 11))
      checkEq "one hot chunk put into one cold chunk" (1, 1) (length (Ev.fzMerged fz), length (Ev.fzInto fz))
      checkEq "the marker of c is in no file; the other two are" [1, 1, 0] =<< mapM (\i -> markerCount (m i) dir) [1, 2, 3]
      checkEq "erase/ holds nothing" [] =<< listDirectory (dir </> "erase")
      mf <- readManifest dir
      let [cold] = mfChunks mf
      checkEq "cold, no record, a and b only" ("cold", Nothing, 2) (ceTier cold, ceErase cold, ceEvents cold)
      checkEq "a and b are found, c is not" [1, 1, 0] =<< mapM (fmap length . Ev.history ev) [a, b, c]
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev (Ev.verifyEvents ev)
      Ev.closeEvents ev
      -- two entities a second apart: erasing both leaves a gap in the zone map, so it is a record and not a drop; and nothing is left of the chunk
      withTmp $ \dir2 -> do
        (clk, clock) <- newClock nov11
        ev2 <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir2
        let put ops = Ev.withEventsLock ev2 (Ev.record ev2 ops >>= \rc -> Ev.commit ev2 rc >> pure rc)
        rx <- put [Create (Tmp 1) "x" (Lit (VInt 1))]
        modifyIORef' clk (+ 1000)
        ry <- put [Create (Tmp 1) "x" (Lit (VInt 2))]
        _ <- Ev.seal ev2
        let [x] = M.elems (Ev.rcTemps rx); [y] = M.elems (Ev.rcTemps ry)
        done <- Ev.withEventsLock ev2 (Ev.eraseNow ev2 (Erase.fromIds [x, y]))
        checkEq "a record, not a drop" (1, 0) (length (Ev.edRecorded done), length (Ev.edDropped done))
        mf0 <- readManifest dir2
        fz2 <- Ev.freezeUnit ev2 (KCreate, (2026, 11))
        checkEq "the hot chunk is merged, and no cold chunk takes its place" (1, 0) (length (Ev.fzMerged fz2), length (Ev.fzInto fz2))
        mf1 <- readManifest dir2
        checkEq "the world has no chunk; the name counter did not move for a chunk that was not made" (0, mfNextChunk mf0) (length (mfChunks mf1), mfNextChunk mf1)
        checkEq "nothing in chunks/ or erase/" ([], []) =<< ((,) <$> listDirectory (dir2 </> "chunks") <*> listDirectory (dir2 </> "erase"))
        checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev2 (Ev.verifyEvents ev2)
        Ev.closeEvents ev2)

  , ("freeze: where it can stop; a scratch directory nobody lists, or the swap done with the hot chunk not yet removed: open sweeps them, and the world is the one the manifest names", withTmp $ \dir -> do
      (ev, ids) <- markedWorld dir 3
      mf0 <- readManifest dir
      let [hot] = mfChunks mf0
      copyFlat (dir </> "chunks" </> ceName hot) (dir </> "aside-chunk")
      Ev.closeEvents ev
      -- 1. died while the cold chunk was being written: a scratch directory, and a named one, that nobody lists
      copyFlat (dir </> "aside-chunk") (dir </> "chunks" </> ".freeze-create-2026-11")
      copyFlat (dir </> "aside-chunk") (dir </> "chunks" </> "c000099-create-2026-11")
      ev1 <- Ev.openEvents Ev.defaultEventsConfig dir
      checkEq "open swept both: the hot chunk is all that is in chunks/" [ceName hot] =<< listDirectory (dir </> "chunks")
      checkEq "the world is the old one" [1, 1, 1] =<< mapM (fmap length . Ev.history ev1) ids
      -- 2. the swap done, the hot chunk not yet removed
      _ <- Ev.freezeUnit ev1 (KCreate, (2026, 11))
      mf1 <- readManifest dir
      Ev.closeEvents ev1
      copyFlat (dir </> "aside-chunk") (dir </> "chunks" </> ceName hot)
      ev2 <- Ev.openEvents Ev.defaultEventsConfig dir
      checkEq "open swept the hot chunk: the cold one is all that is in chunks/" (map ceName (mfChunks mf1)) =<< listDirectory (dir </> "chunks")
      checkEq "the world is the cold one" ([1, 1, 1], ["cold"]) =<< ((,) <$> mapM (fmap length . Ev.history ev2) ids <*> pure (map ceTier (mfChunks mf1)))
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev2 (Ev.verifyEvents ev2)
      Ev.closeEvents ev2)

  , ("freeze: an erase and a rewrite of a cold chunk leave it cold: no MPHF comes back, and the marker is gone", withTmp $ \dir -> do
      (ev, [a, b, c]) <- markedWorld dir 3
      let m i = BC.pack (marker i)
      _ <- Ev.freezeUnit ev (KCreate, (2026, 11))
      _ <- Ev.withEventsLock ev (Ev.eraseNow ev (Erase.fromIds [c]))
      mfE <- readManifest dir
      check "the cold chunk has a record now" (all (\x -> ceErase x /= Nothing) (mfChunks mfE))
      done <- Ev.rewriteErased ev 10
      checkEq "rewritten" 1 (length (Ev.rwRewritten done))
      mf <- readManifest dir
      let [new] = mfChunks mf
      checkEq "the new chunk is cold, with chunk.hosi and filter.bin only" ("cold", [chunkDataFile, chunkFilterFile]) (ceTier new, sort (map feName (ceFiles new)))
      checkEq "and no mphf.idx on the disk" False =<< doesFileExist (dir </> "chunks" </> ceName new </> chunkMphfFile)
      checkEq "the marker of c is in no file; the other two are" [1, 1, 0] =<< mapM (\i -> markerCount (m i) dir) [1, 2, 3]
      checkEq "a and b are found, c is not" [1, 1, 0] =<< mapM (fmap length . Ev.history ev) [a, b, c]
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev (Ev.verifyEvents ev)
      Ev.closeEvents ev)

  , ("rewrite: a history that runs while the chunk is being replaced always finds its events", withTmp $ \dir -> do
      (ev, xs) <- markedWorld dir 40
      let x = head xs
          rounds = drop 1 xs
      running <- newIORef True
      problems <- newIORef ([] :: [String])
      looked <- newIORef (0 :: Int)
      finished <- newEmptyMVar
      _ <- forkIO $ do
        let loop = do
              go <- readIORef running
              when go $ do
                r <- try (length <$> Ev.history ev x)
                case r of
                  Right 1 -> pure ()
                  Right n -> modifyIORef' problems (("found " ++ show n ++ " events") :)
                  Left (e :: SomeException) -> modifyIORef' problems (show e :)
                modifyIORef' looked (+ 1)
                loop
        loop
        putMVar finished ()
      forM_ rounds $ \y -> do
        _ <- Ev.withEventsLock ev (Ev.eraseNow ev (Erase.fromIds [y]))
        void (Ev.rewriteErased ev 10)
      writeIORef running False
      takeMVar finished
      checkEq "no history failed or came back wrong" [] =<< readIORef problems
      check "it did look, many times" . (> 20) =<< readIORef looked
      checkEq "what is left is x" [1] =<< mapM (fmap length . Ev.history ev) [x]
      checkEq "verify clean" (True, []) =<< Ev.withEventsLock ev (Ev.verifyEvents ev)
      Ev.closeEvents ev
      -- the same, but made to happen: the chunk is replaced after history has taken its view and before it opens anything.
      -- (The loop above only sometimes lands there; this lands there every time.)  Warm the caches first, so the
      -- reader and the filter of the old chunk are the ones the view points at.
      withTmp $ \dir2 -> do
        (ev2, [a, _, c]) <- markedWorld dir2 3
        _ <- Ev.history ev2 a
        _ <- Ev.withEventsLock ev2 (Ev.eraseNow ev2 (Erase.fromIds [c]))
        (es, _) <- Ev.historyTraceAfter (void (Ev.rewriteErased ev2 10)) ev2 a
        checkEq "a is found, in the chunk that took the old one's place" 1 (length es)
        checkEq "the old chunk is gone from the disk" 1 . length =<< listDirectory (dir2 </> "chunks")
        Ev.closeEvents ev2)

  , ("probe (星かな？): a stranger is turned away by the fingerprint without reading anything", do
      reads_ <- newIORef (0 :: Int)
      let slotBody r = modifyIORef' reads_ (+ 1) >> pure r :: IO (Maybe String)
      a <- confirmSlot 1 2 (slotBody (Just "x"))
      checkEq "different fingerprints: not a star" NotStar a
      checkEq "...and the body was never read" 0 =<< readIORef reads_
      b <- confirmSlot 7 7 (slotBody Nothing)
      checkEq "same fingerprint, other key: looked like one" LookAlike b
      checkEq "...the body was read once" 1 =<< readIORef reads_
      c <- confirmSlot 7 7 (slotBody (Just "evidence"))
      checkEq "same fingerprint, same key: a star" (Star "evidence") c)

  , ("chunk: write then read back; every id finds exactly its events (QuickCheck)", prop 60 $
      forAll genEvents $ \evs -> monadicIO $ do
        ok <- run $ withTmp $ \dir -> do
          let cdir = dir </> "c"
          _ <- writeChunk cdir 7 KCreate (2026, 11) evs
          cr <- openChunkReader cdir
          back <- readChunkEvents cr
          let want = sortOn (\e -> (evId e, evAttr e, evSeq e)) evs
              byId = M.fromListWith (flip (++)) [ (evId e, [e]) | e <- want ]
          found <- forM (M.toList byId) $ \(i, es) -> do
            o <- lookupInChunk cr i
            pure (o == Star es)
          absent <- forM [ 1, 2, 3, 999999 ] $ \i -> do
            o <- lookupInChunk cr i
            pure (case o of Star _ -> False; _ -> True)
          pure (back == want && and found && and absent)
        assert ok)
  , ("chunk: a corrupted file is reported, not misread", withTmp $ \dir -> do
      let cdir = dir </> "c"
      _ <- writeChunk cdir 4 KCreate (2026, 11) [ E.Event s KCreate (fromIntegral s * 3) "a" (VInt 1) | s <- [1 .. 20] ]
      bs <- B.readFile (cdir </> chunkDataFile)
      B.writeFile (cdir </> chunkDataFile) (B.take 20 bs)          -- cut short
      r <- try (openChunkReader cdir)
      check "expected Corrupt" (case r of Left (Corrupt _) -> True; _ -> False))
  , ("chunk: the filter lets every id in; a stranger is never taken for a star, with the filter or without it", withTmp $ \dir -> do
      let ids      = [ makeId (5 + m) s | m <- [0 .. 19], s <- [0 .. 99] ]            -- 2,000 ids of the real shape
          strangers = [ makeId (40 + m) s | m <- [0 .. 19], s <- [0 .. 999] ]        -- 20,000 ids of other months
          cdir     = dir </> "c"
      _ <- writeChunk cdir 64 KCreate (2026, 1) [ E.Event q KCreate i "a" (VInt 1) | (q, i) <- zip [1 ..] ids ]
      cr   <- openChunkReader cdir
      filt <- readChunkFilter cdir (length ids)
      check "there is a filter" (not (null filt))
      check "every id passes the filter" (all (filterMayHave filt) ids)
      checkEq "and finds its star" True . and =<< forM ids (\i -> (\o -> case o of Star _ -> True; _ -> False) <$> lookupInChunk cr i)
      let letThrough = length (filter (filterMayHave filt) strangers)
      check ("strangers let through by the filter: " ++ show letThrough ++ " of 20,000 (about 0.3 expected)") (letThrough <= 5)
      -- 'lookupInChunk' does not look at the filter: the MPHF fingerprint alone must also keep strangers out
      stars <- length . filter (\o -> case o of Star _ -> True; _ -> False) <$> mapM (lookupInChunk cr) strangers
      checkEq "no stranger is a star" 0 stars)
  , ("chunk: a chunk without filter.bin (an older one) still answers; a cut or foreign filter is Corrupt", withTmp $ \dir -> do
      let evs n  = [ E.Event q KCreate (makeId 5 (fromIntegral q)) "a" (VInt 1) | q <- [1 .. n] ]
          cdir   = dir </> "c"
          corrupt' what act = do
            r <- try act
            check what (case r of Left (Corrupt _) -> True; Right _ -> False; Left _ -> False)
      _ <- writeChunk cdir 4 KCreate (2026, 1) (evs 20)
      removeFile (cdir </> chunkFilterFile)
      filt <- readChunkFilter cdir 20
      check "no filter file: no filter" (null filt)
      check "so it may have anything" (filterMayHave filt (makeId 77 7))
      cr <- openChunkReader cdir
      o <- lookupInChunk cr (makeId 5 3)
      check "and the lookup still finds a real id" (case o of Star _ -> True; _ -> False)
      -- a cut filter
      _ <- writeChunk cdir 4 KCreate (2026, 1) (evs 20)
      fb <- B.readFile (cdir </> chunkFilterFile)
      B.writeFile (cdir </> chunkFilterFile) (B.take (B.length fb - 1) fb)
      corrupt' "a cut filter is Corrupt" (readChunkFilter cdir 20)
      -- the filter of a chunk with another number of ids
      _ <- writeChunk (dir </> "d") 4 KCreate (2026, 1) (evs 30)
      copyFile (dir </> "d" </> chunkFilterFile) (cdir </> chunkFilterFile)
      corrupt' "another chunk's filter is Corrupt" (readChunkFilter cdir 20))

  , ("deep path: with no MPHF index, the footer's block table gives the same answers as the MPHF (QuickCheck, versions 1 and 2)", prop 60 $
      forAll genEvents $ \evs -> monadicIO $ do
        ok <- run $ withTmp $ \dir -> do
          rs <- forM [1, 2] $ \v -> do
            let cdir = dir </> show v
            _ <- writeChunkVersion v cdir 7 KCreate (2026, 11) evs
            fast <- openChunkReader cdir
            deep <- openChunkReaderWith (withoutMphf (localSource cdir))
            let want = M.fromListWith (flip (++)) [ (evId e, [e]) | e <- sortOn (\e -> (evId e, evAttr e, evSeq e)) evs ]
            -- every id from before the first to after the last: present ones, and absent ones inside and outside the ranges
            same <- forM [90 .. 130] $ \i -> do
              a <- lookupInChunk fast i
              b <- lookupInChunk deep i
              pure (case (M.lookup i want, a, b) of
                      (Just es, Star x, Star y) -> x == es && y == es
                      (Nothing, x, y)           -> not (isStar x) && not (isStar y)
                      _                         -> False)
            back <- readChunkEvents deep
            pure (and same && back == concat (M.elems want))
          pure (and rs)
        assert ok)
  , ("cold chunk: the same events as a hot one, answered by the deep path; no mphf.idx, so the files are fewer and smaller; the manifest lists what is there (QuickCheck)", prop 60 $
      forAll genEvents $ \evs -> monadicIO $ do
        ok <- run $ withTmp $ \dir -> do
          let hot = dir </> "hot"; cold = dir </> "cold"
              sizeOf d = fmap sum . mapM (getFileSize . (d </>)) =<< listDirectory d
          _ <- writeChunk hot 7 KCreate (2026, 11) evs
          info <- writeColdChunk cold 64 KCreate (2026, 11) evs
          hotInfo <- crInfo <$> openChunkReader hot
          files <- listDirectory cold
          listed <- map (\(n, _, _) -> n) <$> hashChunkFiles cold
          hr <- openChunkReader hot
          cr <- openChunkReader cold
          hotEvs <- readChunkEvents hr
          coldEvs <- readChunkEvents cr
          let want = M.fromListWith (flip (++)) [ (evId e, [e]) | e <- sortOn (\e -> (evId e, evAttr e, evSeq e)) evs ]
          found <- forM [90 .. 130] $ \i -> do
            o <- lookupInChunk cr i
            pure (case (M.lookup i want, o) of
                    (Just es, Star x) -> x == es
                    (Nothing, x)      -> not (isStar x)
                    _                 -> False)
          filt <- readChunkFilter cold (ciIds info)
          hs <- sizeOf hot
          cs <- sizeOf cold
          pure ( info == hotInfo, coldEvs == hotEvs, and found, sort files == sort [chunkDataFile, chunkFilterFile]
               , sort listed == sort files, not (null filt) && all (filterMayHave filt) (M.keys want), cs < hs )
        assert (ok == (True, True, True, True, True, True, True)))
  , ("deep path: opening reads four ranges; a lookup reads one block, or nothing when the footer already says no", withTmp $ \dir -> do
      let ids = [ makeId 5 (2 * s) | s <- [0 .. 1999] ]                           -- even seq numbers only
          inside   = [ makeId 5 (2 * s + 1) | s <- [0 .. 1998] ]                  -- odd ones: inside the ranges, not in the chunk
          outside  = [ makeId 6 s | s <- [0 .. 99] ]                              -- past the last block
          cdir = dir </> "c"
      _ <- writeChunk cdir 64 KCreate (2026, 1) [ E.Event (fromIntegral q) KCreate i "a" (VInt (fromIntegral q)) | (q, i) <- zip [1 :: Int ..] ids ]
      size <- fromIntegral . B.length <$> B.readFile (cdir </> chunkDataFile)
      calls <- newIORef (0 :: Int); bytes <- newIORef (0 :: Int)
      let counted = (withoutMphf (localSource cdir))
            { csRange = \f off len -> do
                b <- csRange (localSource cdir) f off len
                modifyIORef' calls (+ 1) >> modifyIORef' bytes (+ B.length b)
                pure b }
          measure act = do
            c0 <- readIORef calls; b0 <- readIORef bytes
            r <- act
            c1 <- readIORef calls; b1 <- readIORef bytes
            pure (r, c1 - c0, b1 - b0)
      (cr, openCalls, openBytes) <- measure (openChunkReaderWith counted)
      checkEq "open: fixed part, header, trailer, footer" 4 openCalls
      check ("open reads far less than the chunk (" ++ show openBytes ++ " of " ++ show size ++ ")") (openBytes * 4 < size)
      -- every present id: one range, one block (a small part of the file), and the right answer
      forM_ (take 200 (cycle' 17 ids)) $ \i -> do
        (o, c, b) <- measure (lookupInChunk cr i)
        checkEq "one range" 1 c
        check ("one block is a small part of the chunk (" ++ show b ++ " of " ++ show size ++ ")") (b * 10 < size)
        check "found" (isStar o)
      -- inside the ranges but not in the chunk: the block is read for nothing, and the answer says so
      (o1, c1, _) <- measure (lookupInChunk cr (inside !! 500))
      checkEq "looked like a star: one range" (LookAlike, 1) (o1, c1)
      -- past the last block: the footer's table has already answered
      forM_ outside $ \i -> do
        (o, c, b) <- measure (lookupInChunk cr i)
        checkEq "nothing read" (NotStar, 0, 0) (o, c, b))
  , ("deep path: a chunk the filter turns away is never opened", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      let cfg = Ev.defaultEventsConfig { Ev.ecClock = clock }
      ev <- Ev.openEvents cfg dir
      let write1 n = do
            r <- Ev.withEventsLock ev (Ev.record ev [Create (Tmp 1) "a" (Lit (VInt n))])
            Ev.withEventsLock ev (Ev.commit ev r)
            pure (Ev.rcTemps r M.! 1)
      a <- write1 1
      writeIORef clk (nov11 + 3600000)
      _ <- write1 2
      _ <- Ev.seal ev
      Ev.closeEvents ev
      ev2 <- Ev.openEvents cfg dir                                       -- nothing is open yet
      let ghost = makeId (idMs a + 1800000) 0                            -- inside the chunk's id range, never written
      (es, tr) <- Ev.historyTrace ev2 ghost
      checkEq "nothing found" [] es
      checkEq "turned away by the filter, with no reader opened" (1, 0) . (\s -> (Ev.ltSkippedByFilter tr, Ev.xsReadersOpen s)) =<< Ev.eventsStats ev2
      (esA, trA) <- Ev.historyTrace ev2 a
      checkEq "a real id passes the filter" 0 (Ev.ltSkippedByFilter trA)
      checkEq "and its chunk is opened only now" 1 . Ev.xsReadersOpen =<< Ev.eventsStats ev2
      check "and it finds the event" (length esA == 1)
      Ev.closeEvents ev2)

  , ("chunk: the value dictionary loses nothing; versions 1 and 2 read back the same (QuickCheck)", prop 60 $
      forAll genRepetitiveEvents $ \evs -> monadicIO $ do
        ok <- run $ withTmp $ \dir -> do
          let want = sortOn (\e -> (evId e, evAttr e, evSeq e)) evs
              same v = do
                _ <- writeChunkVersion v (dir </> show v) 7 KCreate (2026, 11) evs
                cr <- openChunkReader (dir </> show v)
                back <- readChunkEvents cr
                -- the sign of zero is part of the value: (==) cannot see it, so look at the bits
                let bits = map (\e -> case evVal e of VDouble d -> Just (isNegativeZero d); _ -> Nothing)
                found <- forM (S.toList (S.fromList (map evId evs))) $ \i -> do
                  o <- lookupInChunk cr i
                  pure (case o of Star es -> es == [ e | e <- want, evId e == i ] && bits es == bits [ e | e <- want, evId e == i ]; _ -> False)
                pure (back == want && bits back == bits want && and found)
          rs <- mapM same [1, 2]
          pure (and rs)
        assert ok)
  , ("chunk: the dictionary makes a chunk with repeated values smaller, by exactly what it saves", withTmp $ \dir -> do
      let sizeOf v name evs = do
            _ <- writeChunkVersion v (dir </> name) 50 KCreate (2026, 11) evs
            B.length <$> B.readFile (dir </> name </> chunkDataFile)
          kw t = VKw (Keyword t)
          repeated = [ E.Event s KCreate (fromIntegral s) "type" (kw "http://schema.org/Person") | s <- [1 .. 200] ]
          -- the same length (24 characters) but every one different, so nothing goes in the dictionary
          varied   = [ E.Event s KCreate (fromIntegral s) "type" (kw ("http://schema.org/P" <> T.justifyRight 5 '0' (T.pack (show s)))) | s <- [1 .. 200] ]
          distinct = [ E.Event s KCreate (fromIntegral s) "n" (VInt (fromIntegral s * 1000003)) | s <- [1 .. 200] ]
      rep2 <- sizeOf 2 "rep2" repeated; var2 <- sizeOf 2 "var2" varied
      -- the keyword is 1 (tag) + 1 (length) + 24 (text) = 26 bytes; a reference is 2 (tag, number 0).
      -- Each of the 200 saves 26 - 2, and the header pays for the entry once (26)
      checkEq "repeated values: exactly the bytes the references save" (200 * (26 - 2) - 26) (var2 - rep2)
      -- and version 2 spells numbers short: it must be clearly smaller than version 1 on all of them
      forM_ [("repeated", repeated), ("varied", varied), ("distinct", distinct)] $ \(name, evs) -> do
        a <- sizeOf 1 ("a" ++ name) evs; b <- sizeOf 2 ("b" ++ name) evs
        check ("version 2 is smaller than version 1 on " ++ name ++ " (" ++ show b ++ " < " ++ show a ++ ")") (b < a))
  , ("chunk: version 2 reads back the extremes: edge numbers, huge seqs, ids far apart, empty and long text", withTmp $ \dir -> do
      let ints = [minBound, minBound + 1, -65, -64, -1, 0, 1, 63, 64, 8191, 8192, maxBound - 1, maxBound]
          ev s i a v = E.Event s KCreate i a v
          evs = [ ev (fromIntegral n) (5 + fromIntegral n) "n" (VInt x) | (n, x) <- zip [1 :: Int ..] ints ]
             ++ [ ev maxBound (maxBound - 1) "s" (VStr "") , ev (maxBound - 1) (maxBound - 1) "s" (VStr (T.replicate 3000 "長い文字列 "))
                , ev 2 1 "k" (VKw (Keyword "")), ev 3 1 "d" (VDouble (-0.0)), ev 4 1 "d" (VDouble (1 / 0)), ev 5 1 "b" (VBool True) ]
      _ <- writeChunk (dir </> "c") 4 KCreate (2026, 11) evs
      cr <- openChunkReader (dir </> "c")
      back <- readChunkEvents cr
      checkEq "every event comes back" (sortOn (\e -> (evId e, evAttr e, evSeq e)) evs) back
      found <- forM (S.toList (S.fromList (map evId evs))) $ \i -> (\o -> o == Star [ e | e <- back, evId e == i ]) <$> lookupInChunk cr i
      check "and every id finds its run" (and found))
  , ("varint: any number round-trips in at most 10 bytes; zigzag round-trips (QuickCheck)", prop 500 $
      forAll (frequency [(3, arbitrary), (1, elements [0, 127, 128, 16383, 16384, maxBound, maxBound - 1])]) $ \(w :: Word64) ->
        let bs = build (putVar w)
        in B.length bs <= 10 .&&. runGet getVar bs 0 === Right w
           .&&. runGet (unzigzag <$> getVar) (build (putVar (zigzag (fromIntegral w)))) 0 === Right (fromIntegral w))
  , ("varint: a number that does not fit in 64 bits, or is cut short, is an error", do
      check "11 bytes" (either (const True) (const False) (runGet getVar (B.replicate 10 0xff <> B.pack [0x01]) 0))
      check "the 10th byte holds more than 1 bit" (either (const True) (const False) (runGet getVar (B.replicate 9 0xff <> B.pack [0x02]) 0))
      check "cut short" (either (const True) (const False) (runGet getVar (B.pack [0x80, 0x80]) 0))
      checkEq "the largest number is fine" (Right maxBound) (runGet getVar (build (putVar maxBound)) 0))
  , ("chunk: 0.0 and -0.0 are two values in the dictionary, and a few repeats are not worth an entry", withTmp $ \dir -> do
      let cdir = dir </> "c"
          zeros = [ E.Event s KCreate 7 "z" (VDouble (if even s then 0.0 else -0.0)) | s <- [1 .. 40] ]
      _ <- writeChunk cdir 8 KCreate (2026, 11) zeros
      back <- readChunkEvents =<< openChunkReader cdir
      checkEq "the signs come back as they went" [ even s | s <- [1 .. 40 :: Int] ]
        [ case evVal e of VDouble d -> not (isNegativeZero d); _ -> False | e <- back ]
      -- a 3-byte value seen 6 times: 6 * (3 - 4) < 3 → not worth it, so no entry
      _ <- writeChunk (dir </> "few") 8 KCreate (2026, 11) [ E.Event s KCreate 7 "z" (VStr "a") | s <- [1 .. 6] ]
      bs <- B.readFile (dir </> "few" </> chunkDataFile)
      check "no dictionary reference was written" (not (B.pack [5, 0] `B.isInfixOf` bs)))
  , ("chunk: a dictionary number that points nowhere is reported, not misread", withTmp $ \dir -> do
      -- (the reference "dictionary entry 0" is the bytes below; we look in the body only, not the header)
      forM_ [(2, B.pack [5, 0], B.pack [5, 9])] $ \(v, ref, broken) -> do
        let cdir = dir </> show v
        _ <- writeChunkVersion v cdir 8 KCreate (2026, 11) [ E.Event s KCreate (fromIntegral s) "t" (VStr "a long enough string") | s <- [1 .. 20] ]
        bs <- B.readFile (cdir </> chunkDataFile)
        let bodyAt = fromIntegral (B.foldl' (\a b -> a * 256 + fromIntegral b) (0 :: Word64) (B.take 8 (B.drop 18 bs)))
            (before, from) = B.breakSubstring ref (B.drop bodyAt bs)
        check ("version " ++ show v ++ ": a reference is there to break") (not (B.null from))
        B.writeFile (cdir </> chunkDataFile) (B.take bodyAt bs <> before <> broken <> B.drop (B.length ref) from)
        cr <- openChunkReader cdir
        r <- try (readChunkEvents cr)
        check ("version " ++ show v ++ ": expected Corrupt") (case r of Left (Corrupt _) -> True; _ -> False))

  , ("tail: batches round-trip; a torn last record is noticed and cut", withTmp $ \dir -> do
      tl <- openTail dir False
      let b1 = [ E.Event 1 KCreate 10 "a" (VStr "x"), E.Event 2 KCreate 10 "b" (VInt 2) ]
          b2 = [ E.Event 3 KDelete 10 "a" (VStr "x") ]
      _ <- appendBatch tl b1
      s2 <- appendBatch tl b2
      closeTail tl
      (bs, good) <- readTail dir
      checkEq "both batches" [b1, b2] bs
      size <- getFileSize (dir </> tailFile)
      checkEq "good size = file size" size (fromIntegral good)
      B.appendFile (dir </> tailFile) "\0\0\0\200\1\2"              -- the start of a record that never finished
      (bs', good') <- readTail dir
      checkEq "still both" [b1, b2] bs'
      checkEq "good part ends where it did" size (fromIntegral good')
      _ <- pure s2
      pure ())

  , ("takka edn: renderEdn keeps the characters of a string, escapes the rest, and is read back", do
      checkEq "non-ASCII stays as it is" "\"日本語 é 😀\"" (renderEdn (EStr "日本語 é 😀"))
      checkEq "quote, backslash and control characters" "\"a\\\"b\\\\c\\nd\\u0001\"" (renderEdn (EStr "a\"b\\c\nd\SOH"))
      checkEq "read back" (Right (EStr "日本語 é 😀\n\"\\\SOH")) (readEdn (renderEdn (EStr "日本語 é 😀\n\"\\\SOH")))
      checkEq "a map has a comma between its pairs" "{:a 1, :b [2 3]}"
        (renderEdn (EMap [(EKw "a", EInt 1), (EKw "b", EVec [EInt 2, EInt 3])]))
      checkEq "a float that is not finite" "##NaN" (renderEdn (EFloat (0 / 0))))
  , ("dtlv edn: a string is written so that a reader gives the same text back (QuickCheck)", prop 300 $
      forAll (listOf (frequency [(8, arbitrary), (2, elements ("\"\\\n\r\t\0\DEL\f\b é日本😀" :: String))])) $ \s ->
        let t = T.pack s in readEdn (ednStr t) === Right (EStr t))
  , ("dtlv edn: tameEdn spells \\f and \\b as \\u escapes and leaves \\\\f alone", do
      checkEq "f, b and an escaped backslash before f" (Right (EStr "x\fy\bz\\f")) (readEdn (tameEdn "\"x\\fy\\bz\\\\f\""))
      checkEq "other escapes are untouched" (Right (EStr "a\nb\"c")) (readEdn (tameEdn "\"a\\nb\\\"c\"")))
  , ("schemaMark: a schema with no tuple and no fulltext has the mark it had before they were (a world made then is not made again); each of them changes it", do
      checkEq "the mark of the test schema, as 0.1.0 made it" "hosi-schema/1:ccbb256ed4236b3f24acc25834ec25fc5c4f0f7d4cfb2bd4db33cb998cd7ee50" (schemaMark schema)
      let with a = schemaMark (M.insert "person/pair" a schema)
          plain  = with (attr TString)
      check "a tuple makes another mark" (with (tuple ["person/name", "person/age"]) /= plain)
      check "its parts are in it" (with (tuple ["person/name", "person/age"]) /= with (tuple ["person/age", "person/name"]))
      check "fulltext makes another mark" (with (fulltext (attr TString)) /= plain))

  , ("takka syntax: :order-by, :limit and :offset are read in both forms; a form Datalevin would take no notice of is refused", do
      let parse t = readEdn t >>= parseQuery
          seen t = fmap (\q -> (qOrderBy q, qLimit q, qOffset q)) (parse t)
          refused t = either (const True) (const False) (parse t)
          rows = ":where [?e :person/age ?a] [?e :person/name ?n]"
      checkEq "vector form; a direction is optional"
        (Right ([("?a", Desc), ("?n", Asc), ("?e", Asc)], Just (AInt 20), Just (AInt 40)))
        (seen ("[:find ?a ?n ?e " <> rows <> " :order-by [?a :desc ?n ?e] :limit 20 :offset 40]"))
      checkEq "map form is a vector of pairs"
        (Right ([("?a", Desc), ("?n", Asc)], Just (AInt 5), Just (AInt 0)))
        (seen ("{:find [?a ?n] :where [[?e :person/age ?a] [?e :person/name ?n]] :order-by [[?a :desc] [?n]] :limit 5 :offset 0}"))
      checkEq "nothing asked: nothing in the query"
        (Right ([], Nothing, Nothing)) (seen ("[:find ?a ?n " <> rows <> "]"))
      checkEq "a limit may be a scalar of :in"
        (Right ([], Just (AVar "?k"), Just (AVar "?o")))
        (seen ("[:find ?a ?n :in $ ?k ?o " <> rows <> " :limit ?k :offset ?o]"))
      -- the shapes Datalevin 1.2.10 answers whole, without a word
      check "a scalar find" (refused ("[:find ?a . " <> rows <> " :order-by [?a]]"))
      check "a collection find" (refused ("[:find [?a ...] " <> rows <> " :limit 3]"))
      check "...and :offset" (refused ("[:find ?a . " <> rows <> " :offset 3]"))
      check "map form, flat order-by (Datalevin reads it as ascending whatever it says)"
        (refused "{:find [?a ?n] :where [[?e :person/age ?a] [?e :person/name ?n]] :order-by [?a :desc]}")
      check "vector form, pairs" (refused ("[:find ?a ?n " <> rows <> " :order-by [[?a :desc]]]"))
      check "a direction that is not one" (refused ("[:find ?a ?n " <> rows <> " :order-by [?a :up]]"))
      check "empty order-by" (refused ("[:find ?a ?n " <> rows <> " :order-by []]"))
      check "limit 0 (Datalevin: error)" (refused ("[:find ?a ?n " <> rows <> " :limit 0]"))
      check "limit -1 (Datalevin: no limit at all)" (refused ("[:find ?a ?n " <> rows <> " :limit -1]"))
      check "offset -1" (refused ("[:find ?a ?n " <> rows <> " :offset -1]"))
      check "limit a string" (refused ("[:find ?a ?n " <> rows <> " :limit \"3\"]"))
      check "limit two numbers" (refused ("[:find ?a ?n " <> rows <> " :limit 3 4]"))
      check "a variable that :in does not give" (refused ("[:find ?a ?n " <> rows <> " :limit ?k]"))
      check "a variable that is not a scalar" (refused ("[:find ?a ?n :in $ [?k ...] " <> rows <> " :limit ?k]"))
      check "limit 1 is fine" (not (refused ("[:find ?a ?n " <> rows <> " :limit 1]"))))
  , ("takka syntax: a function puts its answer in a binding (scalar, tuple, collection, relation, _); $ and an option map are arguments; a vector of values is a tuple", do
      let parse t = readEdn t >>= parseQuery
          ok t = either (const False) (const True) (parse t)
          clauses t = fmap qWhere (parse t)
      checkEq "fulltext, with the attribute, a relation binding and options"
        (Right [CFn "fulltext" [TSrc "$", TConst (VKw "note/body"), TVar "?q", TOpts [("top", VInt 5)]] (BRel ["?e", "?a", "?v"])])
        (clauses "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q {:top 5}) [[?e ?a ?v]]]]")
      checkEq "fulltext in every attribute, _ keeps nothing"
        (Right [CFn "fulltext" [TSrc "$", TVar "?q"] (BRel ["?e", "_", "?v"])])
        (clauses "[:find ?e :in $ ?q :where [(fulltext $ ?q) [[?e _ ?v]]]]")
      checkEq "a tuple" (Right [CFn "f" [TVar "?x"] (BTuple ["?a", "?b"])]) (clauses "[:find ?a :in $ ?x :where [(f ?x) [?a ?b]]]")
      checkEq "a collection" (Right [CFn "f" [TVar "?x"] (BColl "?a")]) (clauses "[:find ?a :in $ ?x :where [(f ?x) [?a ...]]]")
      checkEq "a scalar" (Right [CFn "+" [TVar "?x", TConst (VInt 1)] (BScalar "?y")]) (clauses "[:find ?y :in $ ?x :where [(+ ?x 1) ?y]]")
      checkEq "a tuple of values in a pattern, nil for a part that is not there"
        (Right [CPattern (TVar "?e") (TConst (VKw "follow/pair")) (TConst (VTuple [Just (VInt 1), Nothing]))])
        (clauses "[:find ?e :where [?e :follow/pair [1 nil]]]")
      check "a binding that is not one" (not (ok "[:find ?a :in $ ?x :where [(f ?x) 5]]"))
      check "a binding with a name that is not a variable" (not (ok "[:find ?a :in $ ?x :where [(f ?x) [a b]]]"))
      check "a tuple does not hold a tuple" (not (ok "[:find ?e :where [?e :follow/pair [[1 2] 3]]]"))
      check "an option map takes keywords" (not (ok "[:find ?e :in $ ?q :where [(fulltext $ ?q {1 2}) [[?e ?a ?v]]]]"))
      checkEq "the variables of a binding are the variables of the clause, and _ is none" (Right ["?a", "?e", "?q"])
        (fmap (S.toList . S.fromList . concatMap clauseVars) (clauses "[:find ?e :in $ ?q :where [(f ?q) [?a _]] [?e :x/a ?a]]")))
  , ("dtlv edn: numbers, keywords and the schema", do
      checkEq "small double" (Right "1.0e-2") (ednVal (VDouble 0.01))
      checkEq "smallest long" (Right "-9223372036854775808") (ednVal (VInt minBound))
      check "NaN has no spelling" (either (const True) (const False) (ednVal (VDouble (0 / 0))))
      checkEq "plain keyword" ":person/name" (ednKw "person/name")
      checkEq "an IRI goes through (keyword ...)" "(keyword \"http://schema.org/name\")" (ednKw "http://schema.org/name")
      checkEq "schema"
        "{:a/f {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many} :a/n {:db/valueType :db.type/string :db/unique :db.unique/identity} :a/x {}}"
        (schemaEdn (M.fromList [("a/n", identity (attr TString)), ("a/f", many ref), ("a/x", attr TAny)])))
  , ("dtlv edn: a finite double is written so that a reader gives the same number back (QuickCheck)", prop 300 $
      forAll (arbitrary :: Gen Double) $ \d ->
        not (isNaN d || isInfinite d) ==> (ednVal (VDouble d) >>= readEdn) === Right (EFloat d))
  ]

-- | Tests that tell a story through a whole hosigraph, against the khana of the given backend.
hosiTests :: KhanaBackend -> [Test]
hosiTests be =
  [ ("events alone: record, commit, rollback, seal, history, reopen (no khana at all)", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg = Ev.defaultEventsConfig { Ev.ecClock = clock }
      ev <- Ev.openEvents cfg dir
      rec1 <- Ev.withEventsLock ev (Ev.record ev [Create (Tmp 1) "a" (Lit (VInt 1)), Create (Tmp 1) "b" (Lit (VInt 2))])
      Ev.withEventsLock ev (Ev.commit ev rec1)
      let i = Ev.rcTemps rec1 M.! 1
      rec2 <- Ev.withEventsLock ev (Ev.record ev [Create (Id i) "a" (Lit (VInt 9))])
      Ev.withEventsLock ev (Ev.rollback ev rec2)                      -- this one is taken back: it never happened
      checkEq "next seq is not burned by the rollback" 3 . Ev.xsNextSeq =<< Ev.eventsStats ev
      Just sr <- Ev.seal ev
      checkEq "sealed the two committed events" 2 (Ev.srEvents sr)
      Ev.closeEvents ev
      ev2 <- Ev.openEvents cfg dir
      es <- Ev.history ev2 i
      checkEq "history after reopen" [VInt 1, VInt 2] (map evVal es)
      checkEq "last seq" 2 =<< Ev.lastSeq ev2
      Ev.closeEvents ev2)

  , ("one opener at a time: a second open of an open world is WorldBusy and touches nothing, khana included; the first goes on; it opens again after close", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg = (backendCfg be) { hcClock = clock }
      h <- openHosiWith cfg dir schema
      _ <- person h "Frege" 70
      before <- listAll dir
      r <- try (openHosiWith cfg dir schema) :: IO (Either HosiError Hosi)
      check "the second open is WorldBusy" (isWorldBusy r)
      case r of Right h2 -> closeHosi h2; Left _ -> pure ()
      checkEq "nothing under the world was touched (events, khana)" before =<< listAll dir
      _ <- person h "Peirce" 80                                    -- the first goes on
      closeHosi h
      h3 <- openHosiWith cfg dir schema                            -- closed: free again, and nothing was lost
      rows <- query h3 "[:find ?n :where [?e :person/name ?n]]" []
      checkEq "both people are there" 2 (length rows)
      closeHosi h3)

  , ("events alone: the chunk reader cache keeps within its limit and still answers", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      -- three seals = three chunks, each with one entity
      let fill ev = forM [1 .. 3 :: Int] $ \n -> do
            r <- Ev.withEventsLock ev (Ev.record ev [Create (Tmp 1) "a" (Lit (VInt (fromIntegral n)))])
            Ev.withEventsLock ev (Ev.commit ev r)
            _ <- Ev.seal ev
            pure (Ev.rcTemps r M.! 1, VInt (fromIntegral n))
          askAll ev ids = forM_ ids $ \(i, v) -> do
            es <- Ev.history ev i
            checkEq "the history is right" [v] (map evVal es)
      ev0 <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir
      ids <- fill ev0
      Ev.closeEvents ev0
      -- room for everything: all three readers stay open
      evBig <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } dir
      askAll evBig ids
      sBig <- Ev.eventsStats evBig
      checkEq "three chunks, three readers" 3 (Ev.xsReadersOpen sBig)
      Ev.closeEvents evBig
      -- no room: the newest reader is kept, the others are let go, and every answer is the same
      evTiny <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock, Ev.ecReaderCacheBytes = 0 } dir
      askAll evTiny (ids ++ reverse ids)
      sTiny <- Ev.eventsStats evTiny
      checkEq "one reader is always kept" 1 (Ev.xsReadersOpen sTiny)
      check "and its bytes are counted" (Ev.xsReaderBytes sTiny > 0)
      Ev.closeEvents evTiny)

  , ("write, read now, seal, reopen: the same answers", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "Frege")), Create (Tmp 1) "person/age" (Lit (VInt 70))
                   , Create (Tmp 2) "person/name" (Lit (VStr "Peirce")), Create (Tmp 1) "person/friend" (RefTo (Tmp 2)) ]
      let frege = wrTemps r M.! 1; peirce = wrTemps r M.! 2
      before <- getEntity h frege
      checkEq "friend is a real id" (Just [VInt peirce]) (M.lookup "person/friend" before)
      qBefore <- query h "[:find ?n ?f :where [?e :person/name ?n] [?e :person/friend ?x] [?x :person/name ?f]]" []
      checkEq "datalog join over a ref" [[VStr "Frege", VStr "Peirce"]] qBefore
      Just sr <- seal h
      checkEq "sealed 4 events" 4 (srEvents sr)
      s1 <- stats h
      checkEq "tail is empty after seal" 0 (stTailEvents s1)
      checkEq "one chunk (all Create, same month)" 1 (stChunks s1)
      afterSeal <- getEntity h frege
      checkEq "same entity after seal" before afterSeal
      h2 <- reopen (cfgWith clock) dir h
      afterReopen <- getEntity h2 frege
      checkEq "same entity after reopen" before afterReopen
      qAfter <- query h2 "[:find ?n ?f :where [?e :person/name ?n] [?e :person/friend ?x] [?x :person/name ?f]]" []
      checkEq "same datalog answer after reopen" qBefore qAfter
      problems <- verify h2
      checkEq "verify is clean" [] problems
      closeHosi h2)

  , ("unsealed writes survive a restart (they are in the tail)", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      i <- person h "Boole" 49
      h2 <- reopen (cfgWith clock) dir h
      checkEq "age still there" (Just [VInt 49]) =<< ageOf h2 i
      s <- stats h2
      checkEq "still in the tail" 2 (stTailEvents s)
      ev <- history h2 i
      checkEq "history sees tail events" 2 (length ev)
      closeHosi h2)

  , ("update: a newer Create wins now; both stay in history", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg (cfgWith clock) dir $ \h -> do
        i <- person h "Frege" 70
        _ <- write h [Create (Id i) "person/age" (Lit (VInt 71))]
        checkEq "now" (Just [VInt 71]) =<< ageOf h i
        _ <- seal h
        ev <- history h i
        checkEq "history keeps both ages" [VInt 70, VInt 71] [ evVal e | e <- ev, evAttr e == "person/age" ])

  , ("delete: gone from now, still in history; tail and chunks merge by seq", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg (cfgWith clock) dir $ \h -> do
        i <- person h "Frege" 70
        _ <- seal h
        _ <- write h [Delete (Id i) "person/age" (Lit (VInt 70))]
        checkEq "age gone now" Nothing =<< ageOf h i
        ev <- history h i
        checkEq "create, create, delete in order" [KCreate, KCreate, KDelete] (map evKind ev)
        checkEq "seqs ascending" (sort (map evSeq ev)) (map evSeq ev))

  , ("delete of a many-valued attribute removes just that value", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg (cfgWith clock) dir $ \h -> do
        r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "X"))
                     , Create (Tmp 1) "person/tag" (Lit (VStr "logic")), Create (Tmp 1) "person/tag" (Lit (VStr "math")) ]
        let i = wrTemps r M.! 1
        _ <- write h [Delete (Id i) "person/tag" (Lit (VStr "logic"))]
        e <- getEntity h i
        checkEq "one tag left" (Just [VStr "math"]) (M.lookup "person/tag" e))

  , ("a Delete lives in the chunk of the entity's creation month, not the deletion month", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      withHosiCfg (cfgWith clock) dir $ \h -> do
        i <- person h "Frege" 70
        _ <- seal h
        writeIORef clk dec05
        _ <- write h [Delete (Id i) "person/age" (Lit (VInt 70))]
        Just sr <- seal h
        checkEq "delete chunk is named for Nov" [("delete", (2026, 11))] [ (E.kindName (ceKind c), ceMonth c) | c <- srChunks sr ]
        (ev, tr) <- historyTrace h i
        checkEq "found create + delete" 3 (length ev)
        checkEq "two chunks probed, both hits" 2 (length [ () | (_, Star _) <- ltProbed tr ]))

  , ("zone maps: a lookup skips chunks of other months and non-overlapping id ranges", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      withHosiCfg (cfgWith clock) dir $ \h -> do
        a <- person h "A" 1
        writeIORef clk (nov11 + 3600000)
        a2 <- person h "A2" 1
        _ <- seal h                                    -- chunk 1: Nov, ids a .. a2
        writeIORef clk (nov11 + 7200000)
        b <- person h "B" 2
        _ <- seal h                                    -- chunk 2: Nov, later ids
        writeIORef clk dec05
        c <- person h "C" 3
        _ <- seal h                                    -- chunk 3: Dec
        s <- stats h
        checkEq "three chunks" 3 (stChunks s)
        (ea, ta) <- historyTrace h a
        checkEq "A: 2 events" 2 (length ea)
        checkEq "A: Dec chunk skipped by month" 1 (ltSkippedByMonth ta)
        checkEq "A: later Nov chunk skipped by id range" 1 (ltSkippedByRange ta)
        checkEq "A: exactly one chunk read" 1 (length (ltProbed ta))
        (_, tc) <- historyTrace h c
        checkEq "C: both Nov chunks skipped by month" 2 (ltSkippedByMonth tc)
        (_, tb) <- historyTrace h b
        checkEq "B: month of its id" (2026, 11) (ltMonth tb)
        -- an id inside chunk 1's range that was never written: the zone map cannot tell, the filter can
        let ghost = makeId (idMs a + 1800000) 0
        check "ghost id is inside chunk 1's range" (ghost > a && ghost < a2)
        (eg, tg) <- historyTrace h ghost
        checkEq "no events" [] eg
        checkEq "the filter turned the chunk away (no MPHF question)" (1, []) (ltSkippedByFilter tg, map snd (ltProbed tg))
        checkEq "and a real id passes the filter" 0 (ltSkippedByFilter ta))

  , ("a rejected write leaves no trace, in the log or in numbering", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      i <- person h "Frege" 70
      s0 <- stats h
      r <- try (write h [ Create (Id i) "person/age" (Lit (VStr "seventy")) ])      -- wrong type
      check "write was rejected" (case r of Left (WriteRejected _) -> True; _ -> False)
      r2 <- try (write h [ Create (Tmp 1) "person/name" (Lit (VStr "Frege")) ])      -- unique name taken by another entity
      check "unique violation rejected" (case r2 of Left (WriteRejected _) -> True; _ -> False)
      s1 <- stats h
      checkEq "tail bytes unchanged" (stTailBytes s0) (stTailBytes s1)
      checkEq "next seq unchanged" (stNextSeq s0) (stNextSeq s1)
      checkEq "age untouched" (Just [VInt 70]) =<< ageOf h i
      h2 <- reopen (cfgWith clock) dir h
      checkEq "after reopen too" (Just [VInt 70]) =<< ageOf h2 i
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("crash after the manifest swap, before the tail was emptied: nothing doubled", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      i <- person h "Frege" 70
      closeHosi h
      copyFile (dir </> "events" </> tailFile) (dir </> "events" </> "tail.saved")             -- the tail as it was before sealing
      h1 <- openHosiWith (cfgWith clock) dir schema
      _ <- seal h1
      closeHosi h1
      copyFile (dir </> "events" </> "tail.saved") (dir </> "events" </> tailFile)             -- as if the truncate never happened
      h2 <- openHosiWith (cfgWith clock) dir schema
      s <- stats h2
      checkEq "tail records already sealed are ignored" 0 (stTailEvents s)
      ev <- history h2 i
      checkEq "history has each event once" 2 (length ev)
      checkEq "age" (Just [VInt 70]) =<< ageOf h2 i
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("crash after writing chunks, before the manifest swap: the old world remains", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      i <- person h "Frege" 70
      closeHosi h
      copyFile (dir </> "events" </> tailFile) (dir </> "events" </> "tail.saved")
      h1 <- openHosiWith (cfgWith clock) dir schema
      _ <- seal h1
      closeHosi h1
      -- put back the world as it was before the swap: old (empty) manifest, full tail; the new chunk dir stays on disk
      removeFile (dir </> "events" </> manifestFile)
      copyFile (dir </> "events" </> "tail.saved") (dir </> "events" </> tailFile)
      orphans <- listDirectory (dir </> "events" </> "chunks")
      checkEq "an orphan chunk is on disk" 1 (length orphans)
      h2 <- openHosiWith (cfgWith clock) dir schema
      orphans' <- listDirectory (dir </> "events" </> "chunks")
      checkEq "orphan was swept" [] orphans'
      s <- stats h2
      checkEq "events are back in the tail" 2 (stTailEvents s)
      checkEq "age" (Just [VInt 70]) =<< ageOf h2 i
      Just _ <- seal h2                                             -- and sealing again works
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("a torn tail record is cut; the khana (which had seen it) is rebuilt to match the log", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      s1 <- stats h
      b <- person h "Peirce" 75
      closeHosi h
      -- cut the second record in half: the crash happened while it was being written
      size <- getFileSize (dir </> "events" </> tailFile)
      withFile (dir </> "events" </> tailFile) ReadWriteMode $ \f -> hSetFileSize f (size - 9)
      h2 <- openHosiWith (cfgWith clock) dir schema
      s2 <- stats h2
      checkEq "log is back to the first batch" (stTailBytes s1) (stTailBytes s2)
      checkEq "first batch is there" (Just [VInt 70]) =<< ageOf h2 a
      checkEq "second batch is not" Nothing =<< ageOf h2 b
      checkEq "khana agrees with the log" [] =<< verify h2
      c <- person h2 "Boole" 49                                       -- and writing carries on
      check "new id is later than old ones" (c > a)
      closeHosi h2)

  , ("the khana can be thrown away: lost, or rebuilt live, same answers", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      i <- person h "Frege" 70
      _ <- seal h
      j <- person h "Peirce" 75
      _ <- write h [Delete (Id i) "person/age" (Lit (VInt 70)), Create (Id j) "person/age" (Lit (VInt 76))]
      let snapshot hh = (,,) <$> getEntity hh i <*> getEntity hh j <*> query hh "[:find ?n :where [?e :person/name ?n]]" []
      want <- snapshot h
      rebuildKhana h
      checkEq "after rebuildKhana" want =<< snapshot h
      closeHosi h
      removePathForcibly (dir </> kd)
      h2 <- openHosiWith (cfgWith clock) dir schema
      checkEq "after the khana directory was deleted" want =<< snapshot h2
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("a khana that is ahead of the log (it saw a write the log lost) is rebuilt", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      s1 <- stats h
      _ <- write h [Create (Id a) "person/age" (Lit (VInt 99))]
      closeHosi h
      withFile (dir </> "events" </> tailFile) ReadWriteMode $ \f -> hSetFileSize f (stTailBytes s1)
      h2 <- openHosiWith (cfgWith clock) dir schema
      checkEq "the lost write is not in now" (Just [VInt 70]) =<< ageOf h2 a
      closeHosi h2)

  , ("an old khana (it never saw the newest writes) catches up from chunks and tail at open", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      closeHosi h
      copyDir (dir </> kd) (dir </> "khana.saved")                      -- a khana that only knows Frege
      h1 <- openHosiWith (cfgWith clock) dir schema
      b <- person h1 "Peirce" 75
      _ <- seal h1                                                    -- Peirce is in a chunk
      c <- person h1 "Boole" 49                                       -- Boole is only in the tail
      closeHosi h1
      removePathForcibly (dir </> kd)
      renameDirectory (dir </> "khana.saved") (dir </> kd)             -- the crash: the khana's last commits never reached the disk
      h2 <- openHosiWith (cfgWith clock) dir schema
      checkEq "Frege" (Just [VInt 70]) =<< ageOf h2 a
      checkEq "Peirce (from the chunk)" (Just [VInt 75]) =<< ageOf h2 b
      checkEq "Boole (from the tail)" (Just [VInt 49]) =<< ageOf h2 c
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("a logged batch the khana refuses (crash between log and fold) is dropped at open", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      s <- stats h
      closeHosi h
      -- as if the process died right after the log append of a write the khana would have refused
      tl <- openTail (dir </> "events") False
      _ <- appendBatch tl [E.Event (stNextSeq s) KCreate a "person/age" (VStr "seventy")]
      closeTail tl
      h2 <- openHosiWith (cfgWith clock) dir schema
      checkEq "age is untouched" (Just [VInt 70]) =<< ageOf h2 a
      s2 <- stats h2
      checkEq "the refused batch is out of the log" (stTailBytes s) (stTailBytes s2)
      b <- person h2 "Peirce" 75                                      -- and the seq number is reused cleanly
      checkEq "seqs continue" (stNextSeq s + 2) . stNextSeq =<< stats h2
      _ <- pure b
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("a changed schema is noticed at open: the khana is made again under it, and the old answers do not linger", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let schemaMany = M.insert "person/age" (many (attr TLong)) schema
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      _ <- write h [Create (Id a) "person/age" (Lit (VInt 71))]       -- cardinality one: 71 replaces 70
      checkEq "as cardinality one" (Just [VInt 71]) =<< ageOf h a
      closeHosi h
      markKhana dir
      h1 <- openHosiWith (cfgWith clock) dir schema
      check "the same schema: the khana is kept" =<< isOldKhana dir
      closeHosi h1
      h2 <- openHosiWith (cfgWith clock) dir schemaMany
      check "a different schema: the khana was made again" . not =<< isOldKhana dir
      checkEq "as cardinality many" (Just [VInt 70, VInt 71]) . fmap sort =<< ageOf h2 a
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2
      markKhana dir
      h2b <- openHosiWith (cfgWith clock) dir schemaMany               -- the khana that was made says which schema it was made under
      check "the same schema again: the khana that was made is kept" =<< isOldKhana dir
      closeHosi h2b
      h3 <- openHosiWith (cfgWith clock) dir schema                    -- and back again
      checkEq "as cardinality one again" (Just [VInt 71]) =<< ageOf h3 a
      checkEq "verify clean" [] =<< verify h3
      closeHosi h3)

  , ("a schema the sealed events cannot be folded under is refused at open; the khana and the log are as they were", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let schemaLong = M.insert "person/name" (attr TLong) schema       -- the names are strings: a number it cannot be
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      _ <- seal h
      s1 <- stats h
      closeHosi h
      markKhana dir
      msg <- openRefused (cfgWith clock) dir schemaLong
      check "the message says what to do" ("open it again with the schema" `isInfixOf` msg)
      check "the old khana was not replaced" =<< isOldKhana dir
      check "no half-made khana is left" . not =<< doesDirectoryExist (dir </> (kd ++ ".new"))
      h2 <- openHosiWith (cfgWith clock) dir schema
      check "still not replaced after the old schema is back" =<< isOldKhana dir
      checkEq "Frege" (Just [VInt 70]) =<< ageOf h2 a
      s2 <- stats h2
      checkEq "the log is as it was" (stSealedSeq s1, stTailBytes s1) (stSealedSeq s2, stTailBytes s2)
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("a schema the unsealed log cannot be folded under is refused at open: nothing acknowledged is cut from the log", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let schemaLong   = M.insert "person/name" (attr TLong) schema       -- the names are strings: a number it cannot be
          schemaUnique = M.insert "person/age" (uniqueValue (attr TLong)) schema     -- and two people are 70
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      b <- person h "Peirce" 70
      s1 <- stats h
      closeHosi h
      checkEq "both batches are in the log, none sealed" (4, 0) (stTailEvents s1, stSealedSeq s1)
      forM_ [("a string for a number", schemaLong), ("a unique age taken twice", schemaUnique)] $ \(what, sch) -> do
        _ <- openRefused (cfgWith clock) dir sch
        checkEq ("the log is as it was: " ++ what) (stTailBytes s1) =<< getFileSize (dir </> "events" </> tailFile)
      -- the khana is lost as well: nobody can tell which schema it was made with, and still nothing is cut
      removePathForcibly (dir </> kd)
      _ <- openRefused (cfgWith clock) dir schemaUnique
      checkEq "the log is as it was, with the khana lost" (stTailBytes s1) =<< getFileSize (dir </> "events" </> tailFile)
      h2 <- openHosiWith (cfgWith clock) dir schema
      checkEq "Frege" (Just [VInt 70]) =<< ageOf h2 a
      checkEq "Peirce" (Just [VInt 70]) =<< ageOf h2 b
      checkEq "both batches are still in the log" (stTailEvents s1) . stTailEvents =<< stats h2
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("a khana that cannot be opened is made again beside the old one; if that fails the old one is not deleted", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let schemaLong = M.insert "person/name" (attr TLong) schema       -- the names are strings: a number it cannot be
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      _ <- seal h
      closeHosi h
      B.writeFile (dir </> kd </> "data.mdb") (BC.pack "this is not a database")
      _ <- openRefused (cfgWith clock) dir schemaLong
      check "the khana's directory is still there" =<< doesDirectoryExist (dir </> kd)
      check "and so is the file in it" =<< doesFileExist (dir </> kd </> "data.mdb")
      h2 <- openHosiWith (cfgWith clock) dir schema                    -- the right schema: made again, whole
      checkEq "Frege" (Just [VInt 70]) =<< ageOf h2 a
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("several unfolded tail batches go in together at open", tailCatchUp False)

  , ("...and when the khana refuses one of them, only that one is dropped", tailCatchUp True)

  , ("getEntities / queryMany / queryCellsMany answer what the single calls answer, in the order asked", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg (cfgWith clock) dir $ \h -> do
        a <- person h "Frege" 70
        b <- person h "Peirce" 75
        c <- person h "Boole" 49
        let ids = [c, a, 999999999, b, a]                          -- an id nobody has, and a repeat
        singles <- mapM (getEntity h) ids
        checkEq "getEntities = getEntity, one by one" singles =<< getEntities h ids
        checkEq "no ids" [] =<< getEntities h []
        let big = concat (replicate 1000 [a, b, c])                -- 3000 ids: read as data, never compiled
        bigs <- getEntities h big
        checkEq "a long list: all there, in order" (concat (replicate 1000 (map (singles !!) [1, 3, 0]))) bigs
        let qs = [ (t, []) | t <- diffQueries ]
                 ++ [("[:find ?n :in $ ?min :where [?e :person/name ?n] [?e :person/age ?a] [(>= ?a ?min)]]", [InScalar (VInt 70)])]
        one <- mapM (\(t, i) -> sort <$> query h t i) qs
        checkEq "queryMany = query, one by one" one . map sort =<< queryMany h qs
        checkEq "no queries" [] =<< queryMany h []
        cs  <- queryCellsMany h [(pullQuery, []), (head diffQueries, [])]
        cs1 <- queryCells h pullQuery []
        cs2 <- queryCells h (head diffQueries) []
        checkEq "queryCellsMany = queryCells, one by one"
          [sortOn show (map (map normCell) cs1), sortOn show cs2]
          (map (sortOn show . map (map normCell)) cs)
        r <- try (queryMany h [(head diffQueries, []), ("not a query", [])]) :: IO (Either SomeException [[[Value]]])
        check "a bad query fails the whole call" (either (const True) (const False) r)
        r2 <- try (queryMany h [(pullQuery, [])]) :: IO (Either SomeException [[[Value]]])
        check "a pull in :find is for queryCellsMany" (either (const True) (const False) r2))

  , ("writeMany: separate batches in one go; the invalid and the refused ones are Lefts, the rest go in", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let p n a = [ Create (Tmp 1) "person/name" (Lit (VStr n)), Create (Tmp 1) "person/age" (Lit (VInt a)) ]
          isBad r = case r of Left (BadArgument _) -> True; _ -> False
          isRejected r = case r of Left (WriteRejected _) -> True; _ -> False
          ok r = case r of Right x -> pure x; Left e -> throwIO e
      h <- openHosiWith (cfgWith clock) dir schema
      s0 <- stats h
      rs <- writeMany h [p "Frege" 70, [], p "Peirce" 75, [Create (Id 1) "person/age" (Lit (VInt 1))], p "Boole" 49]
      checkEq "five answers" 5 (length rs)
      check "the empty batch and the reserved id are BadArgument" (isBad (rs !! 1) && isBad (rs !! 3))
      [r1, r2, r3] <- mapM ok [rs !! 0, rs !! 2, rs !! 4]
      checkEq "the first batch starts at the next seq" (stNextSeq s0) (wrFirstSeq r1)
      checkEq "seqs are consecutive across batches" [wrLastSeq r1 + 1, wrLastSeq r2 + 1] [wrFirstSeq r2, wrFirstSeq r3]
      check "each batch has its own Tmp 1" (length (nub [wrTemps r1 M.! 1, wrTemps r2 M.! 1, wrTemps r3 M.! 1]) == 3)
      checkEq "ages" [Just [VInt 70], Just [VInt 75], Just [VInt 49]] =<< mapM (\r -> ageOf h (wrTemps r M.! 1)) [r1, r2, r3]
      checkEq "six events went in" (stNextSeq s0 + 6) . stNextSeq =<< stats h
      -- the khana refuses one batch (a string for a number): the others still go in, the refused one leaves no trace
      s1 <- stats h
      rs2 <- writeMany h [p "Cantor" 60, [Create (Tmp 1) "person/age" (Lit (VStr "old"))], p "Dedekind" 55]
      check "the middle one is WriteRejected" (isRejected (rs2 !! 1))
      [c1, c3] <- mapM ok [rs2 !! 0, rs2 !! 2]
      checkEq "Cantor" (Just [VInt 60]) =<< ageOf h (wrTemps c1 M.! 1)
      checkEq "Dedekind" (Just [VInt 55]) =<< ageOf h (wrTemps c3 M.! 1)
      checkEq "the seqs of the refused batch were not kept" (wrLastSeq c1 + 1) (wrFirstSeq c3)
      checkEq "four events went in" (stNextSeq s1 + 4) . stNextSeq =<< stats h
      -- one valid batch, and it is refused; and nothing valid at all
      cantor <- pure (wrTemps c1 M.! 1)
      rs3 <- writeMany h [[Create (Id cantor) "person/age" (Lit (VStr "x"))], []]
      check "refused, and invalid" (isRejected (head rs3) && isBad (rs3 !! 1))
      s2 <- stats h
      rs4 <- writeMany h [[], []]
      check "all invalid: BadArgument twice" (all isBad rs4)
      checkEq "...and the log did not move" (stTailBytes s2) . stTailBytes =<< stats h
      checkEq "no batches" 0 . length =<< writeMany h []
      checkEq "verify" [] =<< verify h
      h2 <- reopen (cfgWith clock) dir h
      checkEq "after reopen: Cantor" (Just [VInt 60]) =<< ageOf h2 cantor
      checkEq "after reopen: Boole" (Just [VInt 49]) =<< ageOf h2 (wrTemps r3 M.! 1)
      checkEq "verify after reopen" [] =<< verify h2
      closeHosi h2)

  , ("writeMany: a seal that falls due happens once, after the last batch", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let p n a = [ Create (Tmp 1) "person/name" (Lit (VStr n)), Create (Tmp 1) "person/age" (Lit (VInt a)) ]
      withHosiCfg ((cfgWith clock) { hcSealEvents = 5 }) dir $ \h -> do
        rs <- writeMany h [p "Frege" 70, p "Peirce" 75, p "Boole" 49]
        let sealed = [ wrSealed r | Right r <- rs ]
        checkEq "only the last report says it sealed" [False, False, True] sealed
        s <- stats h
        checkEq "one chunk" 1 (stChunks s)
        checkEq "the tail is empty" 0 (stTailEvents s)
        checkEq "verify" [] =<< verify h)

  , ("leftover temp files from a swap that never happened are removed at open", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      _ <- seal h
      closeHosi h
      B.writeFile (dir </> "events" </> "manifest.tmp") "half written"
      B.writeFile (dir </> "events" </> "tail.log.tmp") "half written"
      h2 <- openHosiWith (cfgWith clock) dir schema
      tmps <- filterM doesFileExist [dir </> "events" </> "manifest.tmp", dir </> "events" </> "tail.log.tmp"]
      checkEq "no temp files" [] tmps
      checkEq "the old world is intact" (Just [VInt 70]) =<< ageOf h2 a
      closeHosi h2)

  , ("verify notices a changed chunk byte; open refuses a tampered manifest", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      _ <- person h "Frege" 70
      Just sr <- seal h
      let cfile = dir </> "events" </> "chunks" </> ceName (head (srChunks sr)) </> chunkDataFile
      orig <- B.readFile cfile
      let flipAt k bs = B.take k bs <> B.singleton (B.index bs k + 1) <> B.drop (k + 1) bs
      B.writeFile cfile (flipAt (B.length orig `div` 2) orig)
      problems <- verify h
      check "chunk tamper detected" (not (null problems))
      B.writeFile cfile orig
      checkEq "restored, clean again" [] =<< verify h
      closeHosi h
      mf <- B.readFile (dir </> "events" </> manifestFile)
      let (pre, post) = B.breakSubstring "\"generation\":1" mf
      check "found the generation field" (not (B.null post))
      B.writeFile (dir </> "events" </> manifestFile) (pre <> "\"generation\":2" <> B.drop (B.length "\"generation\":1") post)
      r <- try (openHosiWith (cfgWith clock) dir schema)
      case r of
        Left (Corrupt _) -> pure ()
        Left e           -> throwIO (userError ("wrong error: " ++ show e))
        Right h'         -> closeHosi h' >> throwIO (userError "a tampered manifest was accepted"))

  , ("auto-seal: by amount, and by age", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      let cfg = (cfgWith clock) { hcSealEvents = 5 }
      withHosiCfg cfg dir $ \h -> do
        r1 <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "A")), Create (Tmp 1) "person/age" (Lit (VInt 1)) ]
        checkEq "2 events: not yet" False (wrSealed r1)
        r2 <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "B")), Create (Tmp 1) "person/age" (Lit (VInt 2)), Create (Tmp 1) "person/score" (Lit (VDouble 0.5)) ]
        checkEq "5 events: sealed" True (wrSealed r2)
        checkEq "tail empty" 0 . stTailEvents =<< stats h
      withTmp $ \dir2 -> do
        let cfg2 = (cfgWith clock) { hcSealAgeMs = Just 60000 }
        withHosiCfg cfg2 dir2 $ \h -> do
          _ <- person h "A" 1
          writeIORef clk (nov11 + 120000)
          r <- write h [Create (Tmp 1) "person/name" (Lit (VStr "B"))]
          checkEq "oldest event is 2 minutes old: sealed" True (wrSealed r))

  , ("ids: unique, increasing even if the clock goes backwards, month readable from the id", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      withHosiCfg (cfgWith clock) dir $ \h -> do
        a <- person h "A" 1
        writeIORef clk (nov11 - 86400000)                      -- the clock jumps back a day
        b <- person h "B" 2
        writeIORef clk dec05
        c <- person h "C" 3
        check "a < b < c" (a < b && b < c)
        checkEq "month of a" (2026, 11) (idMonth a)
        checkEq "month of c" (2026, 12) (idMonth c)
        pure ())
  , ("ids: thousands in one millisecond stay distinct", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg (cfgWith clock) dir $ \h -> do
        r <- write h [ Create (Tmp n) "person/age" (Lit (VInt (fromIntegral n))) | n <- [1 .. 3000] ]
        checkEq "3000 distinct ids" 3000 (S.size (S.fromList (M.elems (wrTemps r)))))

  , ("concurrent writers: every batch lands once, in order", withTmp $ \dir -> do
      h <- openHosiWith (backendCfg be) dir schema
      done <- newEmptyMVar
      forM_ [1 .. 4 :: Int] $ \t -> forkIO $ do
        forM_ [1 .. 25 :: Int] $ \k ->
          write h [Create (Tmp 1) "person/age" (Lit (VInt (fromIntegral (t * 1000 + k))))]
        putMVar done ()
      replicateM_ 4 (takeMVar done)
      s <- stats h
      checkEq "100 events" 100 (stTailEvents s)
      checkEq "seqs 1..100" 101 (stNextSeq s)
      checkEq "khana agrees" 100 (stKhanaDatoms s)
      h2 <- reopen (backendCfg be) dir h
      checkEq "after reopen" 100 . stKhanaDatoms =<< stats h2
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("writeIf: racing writers on one condition; exactly one wins, the others get the condition back and leave nothing", withTmp $ \dir -> do
      -- (nobody reads from outside while the others write: with the dtlv executable a read and a write
      -- at the same moment cannot both open the database)
      h <- openHosiWith (backendCfg be) dir schema
      p <- person h "Racer" 0
      s0 <- stats h
      done <- newEmptyMVar
      forM_ [1 .. 6 :: Int] $ \t -> forkIO $ do
        r <- try (writeIf h [Holds p "person/age" (VInt 0)] [Create (Id p) "person/age" (Lit (VInt (fromIntegral t)))])
        putMVar done r
      rs <- replicateM 6 (takeMVar done) :: IO [Either SomeException (Either [Cond] WriteReport)]
      let wins  = [ w | Right (Right w) <- rs ]
          lost  = [ c | Right (Left c)  <- rs ]
      checkEq "nobody threw" 6 (length wins + length lost)
      checkEq "one winner" 1 (length wins)
      checkEq "the five others were told which condition failed" (replicate 5 [Holds p "person/age" (VInt 0)]) lost
      Just [VInt winner] <- ageOf h p
      check "the value is the winner's, not a mix" (winner >= 1 && winner <= 6)
      s1 <- stats h
      checkEq "one event went in, the losers left no seq behind" (stNextSeq s0 + 1) (stNextSeq s1)
      checkEq "verify" [] =<< verify h
      h2 <- reopen (backendCfg be) dir h
      checkEq "after reopen" (Just [VInt winner]) =<< ageOf h2 p
      closeHosi h2)

  , ("writeIf: Holds, Lacks and Unset, one by one; the failed ones come back in order; nothing is written when any fails", withTmp $ \dir -> do
      h <- openHosiWith (backendCfg be) dir schema
      p <- person h "Frege" 70
      q <- person h "Peirce" 75
      let setAge i n = [Create (Id i) "person/age" (Lit (VInt n))]
          isBad r = case r of Left (BadArgument _) -> True; _ -> False
          nextSeq = stNextSeq <$> stats h
      s0 <- nextSeq
      -- Holds: the right value goes in; a stale one is Left and leaves nothing
      r1 <- writeIf h [Holds p "person/age" (VInt 70)] (setAge p 71)
      check "Holds with the current value" (either (const False) (const True) r1)
      checkEq "age is 71" (Just [VInt 71]) =<< ageOf h p
      r2 <- writeIf h [Holds p "person/age" (VInt 70)] (setAge p 99)
      checkEq "Holds with a stale value is Left" (Left [Holds p "person/age" (VInt 70)]) (fmap (const ()) r2)
      checkEq "...and the age is as it was" (Just [VInt 71]) =<< ageOf h p
      checkEq "...and no seq was used" (s0 + 1) =<< nextSeq
      -- Lacks and Unset
      r3 <- writeIf h [Lacks p "person/age" (VInt 70), Unset p "person/score"] [Create (Id p) "person/score" (Lit (VDouble 1.5))]
      check "Lacks (a value it does not have) and Unset (no value yet)" (either (const False) (const True) r3)
      r4 <- writeIf h [Unset p "person/score"] [Create (Id p) "person/score" (Lit (VDouble 2.5))]
      checkEq "Unset once it has a value is Left" (Left [Unset p "person/score"]) (fmap (const ()) r4)
      r5 <- writeIf h [Lacks p "person/age" (VInt 71)] (setAge p 5)
      checkEq "Lacks a value it has is Left" (Left [Lacks p "person/age" (VInt 71)]) (fmap (const ()) r5)
      -- several conditions, several entities: all must hold; the Left names only those that do not
      s1 <- nextSeq
      r6 <- writeIf h [ Holds p "person/age" (VInt 71), Holds q "person/age" (VInt 1), Unset q "person/score", Holds q "person/name" (VStr "x") ]
                      (setAge p 6 ++ setAge q 7)
      checkEq "the failed ones, in the order given"
        (Left [Holds q "person/age" (VInt 1), Holds q "person/name" (VStr "x")]) (fmap (const ()) r6)
      checkEq "neither entity changed" [Just [VInt 71], Just [VInt 75]] =<< mapM (ageOf h) [p, q]
      checkEq "no seq was used" s1 =<< nextSeq
      -- an entity that is not there: Unset holds, Holds does not
      r7 <- writeIf h [Holds 123456789 "person/age" (VInt 1)] (setAge q 8)
      check "Holds on a thing that is not there is Left" (either (const True) (const False) r7)
      -- no conditions: it is 'write'
      r8 <- writeIf h [] (setAge q 9)
      check "no conditions" (either (const False) (const True) r8)
      checkEq "q is 9" (Just [VInt 9]) =<< ageOf h q
      -- a reserved id in a condition is a BadArgument; a bad batch is an exception, as in write
      r9 <- try (writeIf h [Unset 1 "person/age"] (setAge p 1))
      check "reserved id in a condition" (isBad (fmap (const ()) (r9 :: Either HosiError (Either [Cond] WriteReport))))
      r10 <- try (writeIf h [Holds p "person/age" (VInt 71)] [])
      check "an empty batch is BadArgument" (isBad (fmap (const ()) (r10 :: Either HosiError (Either [Cond] WriteReport))))
      checkEq "verify" [] =<< verify h
      h2 <- reopen (backendCfg be) dir h
      checkEq "after reopen" [Just [VInt 71], Just [VInt 9]] =<< mapM (ageOf h2) [p, q]
      closeHosi h2)

  , ("scale: 20,000 events over several seals; every id found; reopen; verify", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      ids <- fmap concat . forM [1 .. 4 :: Int] $ \round_ -> do
        modifyIORef' clk (+ 1000)
        r <- write h [ Create (Tmp n) "person/age" (Lit (VInt (fromIntegral n))) | n <- [1 .. 5000] ]
        _ <- seal h
        pure (M.elems (wrTemps r))
      checkEq "four chunks" 4 . stChunks =<< stats h
      sample <- pure [ ids !! k | k <- [0, 777 .. length ids - 1] ]
      forM_ sample $ \i -> do
        ev <- history h i
        check ("history of " ++ show i) (length ev == 1)
      h2 <- reopen (cfgWith clock) dir h
      checkEq "datoms" 20000 . stKhanaDatoms =<< stats h2
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("erase: a post is gone from the events and from now; a pointer to it from another entity stays as a number; a write to it is refused; verify agrees before and after reopening", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      b <- person h "Peirce" 75
      r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "Boole")), Create (Tmp 1) "person/friend" (RefTo (Id b)) ]
      let c = wrTemps r M.! 1
      rep <- erase h [EraseIds [b]]
      checkEq "one record, nothing dropped, generation 1" (1, 0, 1) (length (erRecorded rep), length (erDropped rep), erGeneration rep)
      checkEq "b's two facts (name and age) were taken out of now" 2 (erRetracted rep)
      checkEq "b is not in now" M.empty =<< getEntity h b
      checkEq "b has no history" [] =<< history h b
      checkEq "a is untouched" (Just [VInt 70]) =<< ageOf h a
      checkEq "c still points at the number b was" (Just [VInt (fromIntegral b)]) . M.lookup "person/friend" =<< getEntity h c
      checkEq "verify clean" [] =<< verify h
      _ <- write h [name "Peirce"]                         -- the unique name is free again, as if nobody had had it
      refused <- try (write h [Create (Id b) "person/age" (Lit (VInt 1))]) :: IO (Either HosiError WriteReport)
      check "a write to b is refused" (case refused of Left (BadArgument _) -> True; _ -> False)
      h2 <- reopen (cfgWith clock) dir h
      checkEq "after reopen: b is still not in now" M.empty =<< getEntity h2 b
      checkEq "and still has no history" [] =<< history h2 b
      checkEq "verify clean" [] =<< verify h2
      again <- erase h2 [EraseIds [b]]
      checkEq "erasing it again changes nothing" (1, [], [], 0) (erGeneration again, erDropped again, erRecorded again, erRetracted again)
      closeHosi h2)

  , ("erase: a span of time takes a month whole: its chunks go with no record; the other months stay; ids up to 1 cannot be named", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      _ <- seal h
      writeIORef clk dec05
      b <- person h "Peirce" 75
      _ <- seal h
      bad <- try (erase h [EraseIds [1]]) :: IO (Either HosiError EraseReport)
      check "the reserved entity cannot be named" (case bad of Left (BadArgument _) -> True; _ -> False)
      rep <- erase h [EraseBetween (UTCTime (fromGregorian 2026 11 1) 0) (UTCTime (fromGregorian 2026 12 1) 0)]
      checkEq "November's chunk dropped whole, nothing recorded" (1, 0, 1) (length (erDropped rep), length (erRecorded rep), erGeneration rep)
      checkEq "its two facts left now" 2 (erRetracted rep)
      checkEq "one chunk left" 1 . stChunks =<< stats h
      checkEq "a is gone from now" M.empty =<< getEntity h a
      checkEq "and from history" [] =<< history h a
      checkEq "b (December) is there" (Just [VInt 75]) =<< ageOf h b
      checkEq "b has both its events" 2 . length =<< history h b
      checkEq "verify clean" [] =<< verify h
      h2 <- reopen (cfgWith clock) dir h
      checkEq "after reopen: a is gone, b is there" (M.empty, Just [VInt 75]) =<< ((,) <$> getEntity h2 a <*> ageOf h2 b)
      checkEq "verify clean" [] =<< verify h2
      -- everything, from id 0: the bookkeeping entity (id 1) is not an entity to erase, and is still there
      before <- stats h2
      _ <- erase h2 [EraseRange 0 maxBound]
      checkEq "b is gone too" M.empty =<< getEntity h2 b
      checkEq "the khana still knows how far it has folded" (stFoldedSeq before) . stFoldedSeq =<< stats h2
      checkEq "verify clean" [] =<< verify h2
      h3 <- reopen (cfgWith clock) dir h2
      checkEq "and so does the reopened one" (stFoldedSeq before) . stFoldedSeq =<< stats h3
      closeHosi h3)

  , ("erase: a crash after the manifest swap, before the khana followed: open makes the khana again from the events, which leave the erased ids out", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      b <- person h "Peirce" 75
      closeHosi h
      -- the events erase b (as erase does first); the khana is not told
      ev <- Ev.openEvents Ev.defaultEventsConfig { Ev.ecClock = clock } (dir </> "events")
      done <- Ev.withEventsLock ev (Ev.eraseNow ev (Erase.fromIds [b]))
      checkEq "the events are at generation 1" 1 (Ev.edGeneration done)
      Ev.closeEvents ev
      h2 <- openHosiWith (cfgWith clock) dir schema
      checkEq "b is not in now (the khana was made again)" M.empty =<< getEntity h2 b
      checkEq "a is" (Just [VInt 70]) =<< ageOf h2 a
      checkEq "verify clean" [] =<< verify h2
      -- and it is a khana that knows its generation: the next open does not make it again
      -- (a file put in its directory now would be swept away with the directory if it were made again)
      let marker = dir </> kd </> "this-khana-was-not-made-again"
      writeFile marker ""
      h3 <- reopen (cfgWith clock) dir h2
      check "the khana was kept, not made again" =<< doesFileExist marker
      checkEq "b is still not in now" M.empty =<< getEntity h3 b
      checkEq "verify clean" [] =<< verify h3
      closeHosi h3)

  , ("clean: after an erase and a clean, the erased values are in no file under the database directory (chunks, tail, khana); the others are; verify is clean", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      let count i = markerCount (BC.pack (marker i)) dir
      [a, b, c] <- forM [1, 2, 3] $ \i -> person h (T.pack (marker i)) (fromIntegral i)
      _ <- seal h
      check "before the erase every marker is on the disk" . all (> 0) =<< mapM count [1, 2, 3]
      _ <- erase h [EraseIds [b]]
      checkEq "b is gone from now and from history" (M.empty, []) =<< ((,) <$> getEntity h b <*> history h b)
      check "but its bytes are still in the files (the chunk keeps them until it is written again)" . (> 0) =<< markerCount (BC.pack (marker 2)) (dir </> "events")
      rep <- clean h defaultCleanOptions
      checkEq "one chunk written again, none left with a record, the khana made again" (1, 0, True) (length (crRewritten rep), crRecordsLeft rep, crKhanaCleaned rep)
      checkEq "...by the compacted copy, not by folding the events again" False (crKhanaRebuilt rep)
      checkEq "b's marker is in no file" 0 =<< count 2
      check "a's and c's are" . all (> 0) =<< mapM count [1, 3]
      checkEq "a and c are as they were, b is not there" [Just [VInt 1], Just [VInt 3], Nothing] =<< mapM (ageOf h) [a, c, b]
      checkEq "verify clean" [] =<< verify h
      rep2 <- clean h defaultCleanOptions
      checkEq "again: nothing to write, the khana is already clean" ([] :: [String], 0, False) (map fst (crRewritten rep2), crRecordsLeft rep2, crKhanaCleaned rep2)
      h2 <- reopen (cfgWith clock) dir h
      checkEq "after reopen: still in no file; still clean" (0, []) =<< ((,) <$> count 2 <*> verify h2)
      rep3 <- clean h2 defaultCleanOptions
      checkEq "and a clean after the reopen knows the khana is clean" False (crKhanaCleaned rep3)
      closeHosi h2)

  , ("clean: a month erased whole needs no rewrite, but the khana is made new all the same", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      let count i = markerCount (BC.pack (marker i)) dir
      _ <- person h (T.pack (marker 1)) 1
      _ <- seal h
      writeIORef clk dec05
      b <- person h (T.pack (marker 2)) 2
      _ <- seal h
      rep <- erase h [EraseBetween (UTCTime (fromGregorian 2026 11 1) 0) (UTCTime (fromGregorian 2026 12 1) 0)]
      checkEq "November's chunk dropped whole: no record" (1, 0) (length (erDropped rep), length (erRecorded rep))
      cr <- clean h defaultCleanOptions
      checkEq "nothing to write again, but the khana is made new" ([] :: [String], 0, True) (map fst (crRewritten cr), crRecordsLeft cr, crKhanaCleaned cr)
      checkEq "...by the compacted copy" False (crKhanaRebuilt cr)
      checkEq "November's marker is in no file" 0 =<< count 1
      check "December's is" . (> 0) =<< count 2
      checkEq "December's person is there" (Just [VInt 2]) =<< ageOf h b
      checkEq "verify clean" [] =<< verify h
      closeHosi h)

  , ("clean: a few chunks at a time; the khana waits for the last one", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      let count i = markerCount (BC.pack (marker i)) dir
      _  <- person h (T.pack (marker 1)) 1
      p2 <- person h (T.pack (marker 2)) 2
      _ <- seal h
      writeIORef clk dec05
      _  <- person h (T.pack (marker 3)) 3
      q2 <- person h (T.pack (marker 4)) 4
      _ <- seal h
      _ <- erase h [EraseIds [p2, q2]]
      c1 <- clean h defaultCleanOptions { coMaxChunks = 1 }
      checkEq "first call: one chunk, one still waiting, the khana not yet" (1, 1, False) (length (crRewritten c1), crRecordsLeft c1, crKhanaCleaned c1)
      c2 <- clean h defaultCleanOptions { coMaxChunks = 1 }
      checkEq "second call: the other chunk, none waiting, the khana now" (1, 0, True) (length (crRewritten c2), crRecordsLeft c2, crKhanaCleaned c2)
      checkEq "...by the compacted copy" False (crKhanaRebuilt c2)
      checkEq "the erased markers are in no file, the others are" ([0, 0], True) =<< ((,) <$> mapM count [2, 4] <*> (all (> 0) <$> mapM count [1, 3]))
      checkEq "verify clean" [] =<< verify h
      closeHosi h)

  , ("erase by owner: what an account owns goes, in any month and what it had deleted too; the account, b's things and a reply to a thing stay (the reply as a number); after a clean it is on no disk", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      let count i = markerCount (BC.pack (marker i)) dir
          owns = ["person/friend"]
      a <- person h "Account A" 0
      b <- person h "Account B" 0
      p1 <- ownedThing h a 1
      p2 <- ownedThing h b 2
      p3 <- ownedThing h a 3
      _ <- write h [Delete (Id p3) "person/friend" (RefTo (Id a))]          -- a gave up the pointer; only the Create still says p3 was a's
      rp <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "Reply")), Create (Tmp 1) "person/friend" (RefTo (Id p1)) ]
      let reply = wrTemps rp M.! 1
      _ <- seal h
      writeIORef clk dec05
      p4 <- ownedThing h a 4                                                  -- December, and still in the tail
      rep <- erase h [EraseOwnedBy owns [a] Nothing]
      checkEq "p1, p3 (deleted pointer) and p4 (from the tail); not b's, not the reply, not a" (sort [p1, p3, p4]) (erOwned rep)
      checkEq "generation 1; p1 and p4 had three facts, p3 two" (1, 8) (erGeneration rep, erRetracted rep)
      forM_ [p1, p3, p4] $ \i -> do
        checkEq "gone from now" M.empty =<< getEntity h i
        checkEq "and from history" [] =<< history h i
      checkEq "b's thing stays" (Just [VInt 2]) =<< ageOf h p2
      checkEq "the account stays: the caller adds EraseIds when the account goes too" (Just [VStr "Account A"]) . M.lookup "person/name" =<< getEntity h a
      checkEq "the reply stays, and points at the number p1 was" (Just [VInt (fromIntegral p1)]) . M.lookup "person/friend" =<< getEntity h reply
      checkEq "verify clean" [] =<< verify h
      h2 <- reopen (cfgWith clock) dir h
      checkEq "after reopen: still gone, still there" ([M.empty, M.empty, M.empty], Just [VInt 2]) =<< ((,) <$> mapM (getEntity h2) [p1, p3, p4] <*> ageOf h2 p2)
      again <- erase h2 [EraseOwnedBy owns [a] Nothing]
      -- p4 was alone in its chunk, which went whole; p1 and p3 are in a chunk with a record, so their Creates are still there to be found
      checkEq "before a clean: asked again, the Creates that are left are found, and nothing new is erased" ([p1, p3], 1, 0) (erOwned again, erGeneration again, erRetracted again)
      _ <- clean h2 defaultCleanOptions
      checkEq "the erased markers are in no file, b's is" ([0, 0, 0], True) =<< ((,) <$> mapM count [1, 3, 4] <*> ((> 0) <$> count 2))
      none <- erase h2 [EraseOwnedBy owns [a] Nothing]
      checkEq "after a clean there is nothing of them to find" ([], 1, 0) (erOwned none, erGeneration none, erRetracted none)
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("erase by owner: a window is on the creation time; a second call can take what the first found owns, with the account; nothing to match writes nothing; a window that ends before it starts is refused", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      let owns  = ["person/friend"]
          novem = (UTCTime (fromGregorian 2026 11 1) 0, UTCTime (fromGregorian 2026 12 1) 0)
          long  = (UTCTime (fromGregorian 2000 1 1) 0, UTCTime (fromGregorian 2001 1 1) 0)
      a  <- person h "Account A" 0
      n1 <- ownedThing h a 1
      _ <- seal h
      writeIORef clk dec05
      d1 <- ownedThing h a 2
      _ <- seal h
      forM_ [ ("no attributes", EraseOwnedBy [] [a] Nothing), ("no owners", EraseOwnedBy owns [] Nothing)
            , ("an owner nobody names", EraseOwnedBy owns [999999] Nothing), ("a window before the first id", EraseOwnedBy owns [a] (Just long)) ] $ \(what, e) -> do
        rep <- erase h [e]
        checkEq (what ++ ": nothing found, nothing erased") ([], 0, 0) (erOwned rep, erGeneration rep, erRetracted rep)
      refused <- try (erase h [EraseOwnedBy owns [a] (Just (snd novem, fst novem))]) :: IO (Either HosiError EraseReport)
      check "a window that ends before it starts is refused" (case refused of Left (BadArgument _) -> True; _ -> False)
      checkEq "none of that erased anything" (Just [VInt 1], Just [VInt 2]) =<< ((,) <$> ageOf h n1 <*> ageOf h d1)
      -- November only: n1 goes, d1 stays
      r1 <- erase h [EraseOwnedBy owns [a] (Just novem)]
      checkEq "the window of November: n1 only" ([n1], 1) (erOwned r1, erGeneration r1)
      checkEq "n1 is gone, d1 is there" (M.empty, Just [VInt 2]) =<< ((,) <$> getEntity h n1 <*> ageOf h d1)
      -- a thing owned by d1 (a media of a post), in the tail; then the account with its things, and in a second call what those own
      m1 <- ownedThing h d1 3
      r2 <- erase h [EraseOwnedBy owns [a] Nothing]
      checkEq "the first call: a's things, n1 found again (its Create is still in a chunk with a record), d1 new" ([n1, d1], 2) (erOwned r2, erGeneration r2)
      r3 <- erase h [EraseOwnedBy owns (erOwned r2) Nothing, EraseIds [a]]
      checkEq "the second call: what d1 (and n1) own, and the account" ([m1], 3) (erOwned r3, erGeneration r3)
      checkEq "all four are gone from now" [M.empty, M.empty, M.empty, M.empty] =<< mapM (getEntity h) [a, n1, d1, m1]
      checkEq "verify clean" [] =<< verify h
      h2 <- reopen (cfgWith clock) dir h
      checkEq "after reopen: still gone" [M.empty, M.empty, M.empty, M.empty] =<< mapM (getEntity h2) [a, n1, d1, m1]
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2)

  , ("freeze: a month nobody has touched goes cold; history, now and verify stay as they were; a month that is read stays hot; a late write makes a hot chunk that freezes into a second cold one; without hcColdAfterMs nothing happens", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      let days n = n * 86400000
          cfg = (cfgWith clock) { hcColdAfterMs = Just (days 10), hcColdBlockEvents = 4 }
          tiers = (\mf -> sort (map ceTier (mfChunks mf))) <$> readManifest (dir </> "events")
          nov = (KCreate, (2026, 11))
          dec = (KCreate, (2026, 12))
      h <- openHosiWith cfg dir schema
      a <- person h "Frege" 1
      _ <- seal h
      ha0 <- history h a
      writeIORef clk dec05
      b <- person h "Peirce" 2
      _ <- seal h
      hb0 <- history h b
      writeIORef clk (dec05 + days 3)
      r1 <- freeze h defaultFreezeOptions
      checkEq "November (quiet for 27 days) went cold, December (3 days) did not" ([nov], []) (map fst (frFrozen r1), frSkipped r1)
      checkEq "one hot chunk and one cold" ["cold", "hot"] =<< tiers
      checkEq "history is what it was" ([ha0, hb0], [Just [VInt 1], Just [VInt 2]]) =<< ((,) <$> mapM (history h) [a, b] <*> mapM (ageOf h) [a, b])
      checkEq "verify clean" [] =<< verify h
      r2 <- freeze h defaultFreezeOptions
      checkEq "again: nothing (November is cold already, December not old enough)" ([] :: [(Kind, Month)], [] :: [(Kind, Month)]) (map fst (frFrozen r2), frSkipped r2)
      -- a late write to the November entity: a hot chunk in November again
      writeIORef clk (dec05 + days 5)
      _ <- write h [Create (Id a) "person/tag" (Lit (VStr "late"))]
      _ <- seal h
      checkEq "a hot chunk in November again: two hot, one cold" ["cold", "hot", "hot"] =<< tiers
      writeIORef clk (dec05 + days 30)
      r3 <- freeze h defaultFreezeOptions { foMaxUnits = 1 }
      checkEq "one unit at a time, the oldest first: November" [nov] (map fst (frFrozen r3))
      r4 <- freeze h defaultFreezeOptions
      checkEq "then December" [dec] (map fst (frFrozen r4))
      checkEq "three cold chunks (November's two, December's one) and no hot" ["cold", "cold", "cold"] =<< tiers
      checkEq "a's history has the late event too" (length ha0 + 1) . length =<< history h a
      checkEq "...and now says so" (Just [VStr "late"]) . M.lookup "person/tag" =<< getEntity h a
      checkEq "verify clean" [] =<< verify h
      h2 <- reopen cfg dir h
      checkEq "after reopen: the same answers" ([length ha0 + 1, length hb0], [Just [VInt 1], Just [VInt 2]]) =<< ((,) <$> (map length <$> mapM (history h2) [a, b]) <*> mapM (ageOf h2) [a, b])
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2
      -- the default (hcColdAfterMs = Nothing): nothing is cold, nothing is written about use
      withTmp $ \dir2 -> do
        (clk3, clock3) <- newClock nov11
        h3 <- openHosiWith (cfgWith clock3) dir2 schema
        _ <- person h3 "Frege" 1
        _ <- seal h3
        writeIORef clk3 (nov11 + days 400)
        r5 <- freeze h3 defaultFreezeOptions
        checkEq "no unit is cold, however long it has been" ([] :: [(Kind, Month)], [] :: [(Kind, Month)]) (map fst (frFrozen r5), frSkipped r5)
        checkEq "no access.json" False =<< doesFileExist (dir2 </> "events" </> "access.json")
        closeHosi h3)

  , ("backfill: reading newest first, a step holds only what the full khana holds, and the last holds all of it", withTmp $ \dir -> do
      (clk, clock) <- newClock oct18
      withHosiCfg (cfgWith clock) dir $ \h -> do
        ids@[a, _, c, _, _, f] <- backfillWorld h clk
        full <- entitiesOf h ids
        (stages, final, cov) <- backfillSteps h (dir </> "bf") (2026, 10) ids
        backfillChecks (2026, 10) full stages final cov
        -- the first step is the unsealed log: an update of an entity whose Create nobody has read is held as it is
        let (st0, snap0) = head stages
        checkEq "step 0 is the log, and every month still has chunks to read"
          (Coverage (2026, 10) [] [(2026, 10), (2026, 11), (2026, 12)]) (bfCoverage st0)
        checkEq "a is held with the update alone (its Create is not read, so no name)" (M.fromList [("person/age", [VInt 11])]) (snap0 M.! a)
        checkEq "f was created in the log: whole" (full M.! f) (snap0 M.! f)
        checkEq "c is not seen yet" M.empty (snap0 M.! c))

  , ("backfill since a later month reads only the chunks of entities created then", withTmp $ \dir -> do
      (clk, clock) <- newClock oct18
      withHosiCfg (cfgWith clock) dir $ \h -> do
        ids <- backfillWorld h clk
        full <- entitiesOf h ids
        forM_ [((2026, 11), 13, 9), ((2026, 12), 5, 4)] $ \(since, nEv, nFacts) -> do
          (stages, final, cov) <- backfillSteps h (dir </> ("bf-" ++ showMonth since)) since ids
          backfillChecks since full stages final cov
          checkEq (showMonth since ++ ": events read") nEv (bfEventsRead (fst (last stages)))
          checkEq (showMonth since ++ ": facts held") nFacts (bfFactsHeld (fst (last stages)))
          checkEq (showMonth since ++ ": complete months") [ m | m <- [(2026, 10), (2026, 11), (2026, 12)], m >= since ] (cvComplete cov))

  , ("backfill leaves out what erase took, as the full khana does", withTmp $ \dir -> do
      (clk, clock) <- newClock oct18
      withHosiCfg (cfgWith clock) dir $ \h -> do
        ids@[_, _, _, d, _, _] <- backfillWorld h clk
        _ <- erase h [EraseIds [d]]
        full <- entitiesOf h ids
        checkEq "d is gone from now" M.empty (full M.! d)
        (stages, final, cov) <- backfillSteps h (dir </> "bf") (2026, 10) ids
        backfillChecks (2026, 10) full stages final cov)

  , ("backfill of a world that is all log: one step, and its month is complete", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg (cfgWith clock) dir $ \h -> do
        a <- person h "A" 1
        _ <- write h [Create (Id a) "person/age" (Lit (VInt 2))]
        (stages, final, cov) <- backfillSteps h (dir </> "bf") (2026, 11) [a]
        checkEq "one step: the log" [(0, 1)] [ (bfIndex st, bfCount st) | (st, _) <- stages ]
        checkEq "its month is complete at once" (Coverage (2026, 11) [(2026, 11)] []) cov
        checkEq "and the later update is the one held" (M.fromList [(a, M.fromList [("person/age", [VInt 2]), ("person/name", [VStr "A"])])]) final)

  , ("backfill refuses a directory that exists, and removes its own when a step fails", withTmp $ \dir -> do
      (clk, clock) <- newClock oct18
      withHosiCfg (cfgWith clock) dir $ \h -> do
        _ <- backfillWorld h clk
        createDirectory (dir </> "taken")
        r <- try (backfillSince h (dir </> "taken") (2026, 10) (\_ _ -> pure ()))
        case r of
          Left (BadArgument _) -> pure ()
          Left e               -> throwIO e
          Right (k, _)         -> closeKhana k >> throwIO (userError "an existing directory should be refused")
        r2 <- try (backfillSince h (dir </> "bf") (2026, 10) (\st _ -> when (bfIndex st == 1) (throwIO (userError "stop"))))
        case r2 of
          Left (_ :: SomeException) -> pure ()
          Right (k, _)              -> closeKhana k >> throwIO (userError "the failing step should have stopped it")
        checkEq "the half-built khana is gone" False =<< doesPathExist (dir </> "bf")
        checkEq "the world is as it was" [] =<< verify h)

  , ("two khanas are compared page by page: the differences are exact, whatever the page", withTmp $ \dir -> do
      rt <- newKhanaRuntime be
      let ent i = fromIntegral (1000 + 10 * i) :: HosiId
          one i = [ (ent i, "person/name", VStr (T.pack ("n" ++ show i))), (ent i, "person/age", VInt (fromIntegral i))
                  , (ent i, "person/score", VDouble (fromIntegral i / 4)) ]
          full  = concatMap one [0 .. 11 :: Int] ++ [ (ent 5, "person/tag", VStr (T.pack ("t" ++ show j))) | j <- [0 .. 8 :: Int] ]
          minus xs = filter (`notElem` xs)
          entity i = filter (\(e, _, _) -> e == ent i)
          pairs =
            [ ("the same",                          full, full)
            , ("the first entity's age is missing", full, minus [(ent 0, "person/age", VInt 0)] full)
            , ("a score in the middle is missing",  full, minus [(ent 6, "person/score", VDouble 1.5)] full)
            , ("the last entity's name is missing", full, minus [(ent 11, "person/name", VStr "n11")] full)
            , ("a whole entity in the middle is missing", full, minus (entity 7 full) full)
            , ("the big entity is missing",         full, minus (entity 5 full) full)
            , ("an entity after the last one",      full ++ one 20, full)
            , ("an entity before the first one",    full, full ++ [(5, "person/name", VStr "early")])
            , ("a value changed",                   full, minus [(ent 3, "person/age", VInt 3)] full ++ [(ent 3, "person/age", VInt 99)])
            , ("one is empty",                      full, [])
            , ("the other is empty",                [], full)
            ]
      forM_ pairs $ \(what, fa, fb) -> do
        let sa = S.fromList fa
            sb = S.fromList fb
            want = (S.size (sa `S.difference` sb), S.size (sb `S.difference` sa))
        a <- openKhana rt be (dir </> "a") schema
        b <- openKhana rt be (dir </> "b") schema
        applyFacts 7 a fa
        applyFacts 7 b fb
        checkEq (what ++ ": pages of about 4") want =<< endsIn what (compareKhanas 4 a b)
        checkEq (what ++ ": one page, the other way round") (snd want, fst want) =<< compareKhanas 1000 b a
        closeKhana a
        closeKhana b
        removePathForcibly (dir </> "a")
        removePathForcibly (dir </> "b")
      closeKhanaRuntime rt)

  , ("a page of facts never splits an entity, and the pages together are every fact", withTmp $ \dir -> do
      rt <- newKhanaRuntime be
      let ent i = fromIntegral (1000 + 10 * i) :: HosiId
          one i = [ (ent i, "person/name", VStr (T.pack ("n" ++ show i))), (ent i, "person/age", VInt (fromIntegral i)) ]
          facts = concatMap one [0 .. 9 :: Int] ++ [ (ent 4, "person/tag", VStr (T.pack ("t" ++ show j))) | j <- [0 .. 8 :: Int] ]
      k <- openKhana rt be (dir </> "k") schema
      applyFacts 7 k facts
      everything <- khanaDatoms k
      let pages lo = do (p, next) <- khanaDatomPage k lo 4
                        rest <- maybe (pure []) (pages . Just) next
                        pure (p : rest)
      ps <- endsIn "walking the pages" (pages Nothing)
      checkEq "together, every fact" (S.fromList everything) (S.fromList (concat ps))
      checkEq "and no fact twice" (length everything) (length (concat ps))
      let entitiesOf = map (S.fromList . map dE) ps
      checkEq "no entity is in two pages" (S.size (S.unions entitiesOf)) (sum (map S.size entitiesOf))
      -- a page holds 4 facts at most; when its first entity needs more, the page is asked for again at twice the size, until it fits
      let firstEntity p = case p of (d : _) -> length (takeWhile ((== dE d) . dE) p); [] -> 0
      check "a page stays small: 4 facts, or less than twice what its first entity needs" (all (\p -> length p <= max 4 (2 * firstEntity p - 1)) ps)
      check "and the big entity is in a page of its own size" (any (\p -> firstEntity p == 11) ps)
      check "there is more than one page" (length ps > 1)
      closeKhana k
      closeKhanaRuntime rt)

  , ("verify finds a khana that differs from the events, and says by how many", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      b <- person h "Peirce" 75
      checkEq "to begin with, nothing" [] =<< verify h
      closeHosi h
      -- two facts the events do not have (one of them replaces Peirce's age), as a hand in the khana would leave them
      rt <- newKhanaRuntime be
      k <- openKhana rt be (dir </> khanaDirName be) schema
      applyFacts 10 k [(a, "person/tag", VStr "extra"), (b, "person/age", VInt 76)]
      closeKhana k
      closeKhanaRuntime rt
      h2 <- openHosiWith (cfgWith clock) dir schema
      checkEq "the difference, counted" ["khana differs from a fresh fold: 2 extra, 1 missing"] =<< verify h2
      rebuildKhana h2
      checkEq "made again from the events, nothing" [] =<< verify h2
      closeHosi h2)
  ]
  where
    cfgWith c = (backendCfg be) { hcClock = c }
    -- Three months of people: October a b, November c d, December e; then December edits to a b c d (sealed);
    -- then one more edit of a and a new f (edited once more), in the log (unsealed).
    backfillWorld h clk = do
      writeIORef clk oct18
      a <- person h "A" 1
      b <- person h "B" 2
      _ <- write h [Create (Id a) "person/tag" (Lit (VStr "x"))]
      writeIORef clk nov11
      c <- person h "C" 3
      d <- person h "D" 4
      _ <- seal h
      writeIORef clk dec05
      e <- person h "E" 5
      _ <- write h [Create (Id a) "person/age" (Lit (VInt 10))]
      _ <- write h [Delete (Id b) "person/age" (Lit (VInt 2))]
      _ <- write h [Create (Id c) "person/tag" (Lit (VStr "t"))]
      _ <- write h [Create (Id c) "person/age" (Lit (VInt 30))]
      _ <- write h [Create (Id c) "person/age" (Lit (VInt 31))]    -- twice in one chunk: only the later one is c's age
      _ <- write h [Create (Id d) "person/name" (Lit (VStr "D2"))]
      _ <- write h [Delete (Id a) "person/tag" (Lit (VStr "x"))]
      _ <- seal h
      _ <- write h [Create (Id a) "person/age" (Lit (VInt 11))]
      f <- person h "F" 6
      _ <- write h [Create (Id f) "person/age" (Lit (VInt 7))]       -- twice in the log: only the later one is f's age
      pure [a, b, c, d, e, f]
    -- What "now" holds about each of these.
    entitiesOf h ids = M.fromList <$> mapM (\i -> (,) i <$> getEntity h i) ids
    -- Backfill into @path@, keeping what the khana held about @ids@ after every step.
    backfillSteps h path since ids = do
      seen <- newIORef []
      (k, cov) <- backfillSince h path since $ \st kh -> do
        snap <- M.fromList <$> mapM (\i -> (,) i <$> khanaEntity kh i) ids
        modifyIORef seen ((st, snap) :)
      stages <- reverse <$> readIORef seen
      final <- M.fromList <$> mapM (\i -> (,) i <$> khanaEntity k i) ids
      closeKhana k
      pure (stages, final, cov)
    -- What a backfill since @since@ promises, given what the full khana holds (@full@): entities created before @since@
    -- are not there; at every step nothing is held that the full khana does not hold, and an entity of a complete month
    -- is whole; the steps only add; the last one holds it all and has nothing pending.
    backfillChecks since full stages final cov = do
      let expected = M.mapWithKey (\i m -> if idMonth i >= since then m else M.empty) full
          n = length stages
          covs = map (bfCoverage . fst) stages
          grows xs = and (zipWith (<=) xs (drop 1 xs))
          held snap = and [ v `elem` M.findWithDefault [] at (expected M.! i) | (i, es) <- M.toList snap, (at, vs) <- M.toList es, v <- vs ]
      checkEq "the steps are numbered 0 .. n-1" [0 .. n - 1] (map (bfIndex . fst) stages)
      checkEq "every step knows how many there are" [n] (nub (map (bfCount . fst) stages))
      forM_ stages $ \(st, snap) -> do
        let at = "step " ++ show (bfIndex st) ++ ": "
        check (at ++ "nothing is held that the full khana does not hold") (held snap)
        forM_ [ i | i <- M.keys snap, idMonth i `elem` cvComplete (bfCoverage st) ] $ \i ->
          checkEq (at ++ "an entity of a complete month is whole") (expected M.! i) (snap M.! i)
      check "events read only grow" (grows (map (bfEventsRead . fst) stages))
      check "facts held only grow" (grows (map (bfFactsHeld . fst) stages))
      check "complete months only grow" (and (zipWith (\x y -> S.fromList (cvComplete x) `S.isSubsetOf` S.fromList (cvComplete y)) covs (drop 1 covs)))
      checkEq "the last step holds all of it" expected final
      checkEq "the facts held at the end are the facts of those entities" (sum (length <$> concatMap M.elems (M.elems expected))) (bfFactsHeld (fst (last stages)))
      checkEq "nothing is pending at the end" [] (cvPending cov)
      check "so it can be queried" (cvQueryable cov)
      checkEq "the report that comes back is the last step's" (last covs) cov
    kd = khanaDirName be
    -- A file in the khana's directory: it is still there only if that directory was not replaced.
    markKhana dir = writeFile (dir </> kd </> "the-old-khana") "x"
    isOldKhana dir = doesFileExist (dir </> kd </> "the-old-khana")
    -- Open and be refused: the message of the 'SchemaRefused'.
    openRefused cfg dir sch = do
      r <- try (openHosiWith cfg dir sch)
      case r of
        Left (SchemaRefused m) -> pure m
        Left e               -> throwIO e
        Right h              -> closeHosi h >> throwIO (userError "the open should have been refused")
    -- A khana that only knows Frege, while the log holds more: Peirce and Boole as two unsealed
    -- batches, and (if @bad@) a batch the khana would refuse between them and a good batch after it.
    tailCatchUp bad = withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith (cfgWith clock) dir schema
      a <- person h "Frege" 70
      closeHosi h
      copyDir (dir </> kd) (dir </> "khana.saved")
      h1 <- openHosiWith (cfgWith clock) dir schema
      b <- person h1 "Peirce" 75
      c <- person h1 "Boole" 49
      s <- stats h1
      closeHosi h1
      when bad $ do
        tl <- openTail (dir </> "events") False
        _ <- appendBatch tl [E.Event (stNextSeq s) KCreate a "person/age" (VStr "seventy")]
        _ <- appendBatch tl [E.Event (stNextSeq s + 1) KCreate b "person/age" (VInt 76)]
        closeTail tl
      removePathForcibly (dir </> kd)
      renameDirectory (dir </> "khana.saved") (dir </> kd)
      h2 <- openHosiWith (cfgWith clock) dir schema
      checkEq "Frege (the refused batch changed nothing)" (Just [VInt 70]) =<< ageOf h2 a
      checkEq "Peirce" (Just [VInt (if bad then 76 else 75)]) =<< ageOf h2 b
      checkEq "Boole" (Just [VInt 49]) =<< ageOf h2 c
      s2 <- stats h2
      checkEq "the refused batch is out of the log, the others stay"
        (stTailEvents s + (if bad then 1 else 0)) (stTailEvents s2)
      checkEq "folded up to the last event" (if bad then stNextSeq s + 1 else stNextSeq s - 1) (stFoldedSeq s2)
      checkEq "verify clean" [] =<< verify h2
      closeHosi h2

withHosiCfg :: HosiConfig -> FilePath -> (Hosi -> IO a) -> IO a
withHosiCfg cfg dir = bracket (openHosiWith cfg dir schema) closeHosi

-- ---------------------------------------------------------------------------
-- The Datalevin khana (needs the dtlv executable; see main)

-- | Queries asked of both khanas; the rows are sorted before they are compared (a set has no order).
diffQueries :: [T.Text]
diffQueries =
  [ "[:find ?n :where [?e :person/name ?n]]"
  , "[:find ?n ?f :where [?e :person/name ?n] [?e :person/friend ?x] [?x :person/name ?f]]"
  , "[:find ?n :where [?e :person/name ?n] [?e :person/age ?a] [(> ?a 70)]]"
  , "[:find ?n :where [?e :person/name ?n] (not [?e :person/friend _])]"
  , "[:find (count ?e) (sum ?a) (avg ?a) :where [?e :person/age ?a]]"
  , "[:find ?t :where [_ :person/tag ?t]]" ]

pullQuery :: T.Text
pullQuery = "[:find (pull ?e [:person/name :person/age {:person/friend [:person/name]}]) :where [?e :person/name _]]"

-- | A pull's attributes come back as a map from Datalevin: compare them as sets of attributes (their order means nothing).
normCell :: Cell -> Cell
normCell c = case c of CPull p -> CPull (go p); _ -> c
  where go p = case p of
          PEntity kvs -> PEntity (sortOn fst [ (k, go v) | (k, v) <- kvs ])
          PMany xs    -> PMany (map go xs)
          _           -> p

-- | The checks that hold for every way of reaching Datalevin.
dtlvTests :: KhanaBackend -> [Test]
dtlvTests be =
  [ ("dtlv: what it cannot do says so, and a missing dtlv is found before anything is touched", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith ((backendCfg be) { hcClock = clock }) dir schema
      _ <- person h "Frege" 70
      r2 <- try (query h "[:find ?n :in $ % :where [?e :person/name ?n]]" [InRules []]) :: IO (Either SomeException [[Value]])
      check "InRules is refused" (either (const True) (const False) r2)
      checkEq "...and the khana is still good" [] =<< verify h
      closeHosi h)

  , ("dtlv: rules sent as EDN text answer a recursive rule next to other inputs, and a cycle ends", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg ((backendCfg be) { hcClock = clock }) dir $ \h -> do
        -- a -> b -> c, and c -> b: a cycle between b and c
        r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "a")), Create (Tmp 2) "person/name" (Lit (VStr "b")), Create (Tmp 3) "person/name" (Lit (VStr "c"))
                     , Create (Tmp 1) "person/friend" (RefTo (Tmp 2)), Create (Tmp 2) "person/friend" (RefTo (Tmp 3)), Create (Tmp 3) "person/friend" (RefTo (Tmp 2)) ]
        let a = wrTemps r M.! 1
            -- every clause of one rule names its head variables the same way: Datalevin refuses to join them otherwise
            rules = "[[(knows ?a ?b) [?a :person/friend ?b]] [(knows ?a ?b) [?a :person/friend ?x] (knows ?x ?b)]]"
            q = "[:find ?n :in $ ?a % :where (knows ?a ?x) [?x :person/name ?n]]"
        rows <- query h q [InScalar (VInt (fromIntegral a)), InRulesEdn rules]
        checkEq "everyone a reaches, the cycle once" (sortOn show [[VStr "b"], [VStr "c"]]) (sortOn show rows)
        checkEq "...and the khana is still good" [] =<< verify h)

  , ("dtlv: :order-by, :limit and :offset sort and cut a relation; a form Datalevin would ignore is refused before it is asked", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg ((backendCfg be) { hcClock = clock }) dir $ \h -> do
        mapM_ (\(n, a) -> person h n a) [("Frege", 70), ("Peirce", 75), ("Boole", 49), ("Cantor", 75), ("Godel", 71)]
        let rows = ":where [?e :person/age ?a] [?e :person/name ?n]"
            row a n = [VInt a, VStr n]
            q t = query h ("[:find ?a ?n " <> t <> "]")
            young = [row 49 "Boole", row 70 "Frege", row 71 "Godel", row 75 "Cantor", row 75 "Peirce"]
        checkEq "ascending, the second variable breaks the tie" young =<< q (rows <> " :order-by [?a ?n]") []
        checkEq "descending, then ascending"
          [row 75 "Cantor", row 75 "Peirce", row 71 "Godel", row 70 "Frege", row 49 "Boole"]
          =<< q (rows <> " :order-by [?a :desc ?n :asc]") []
        checkEq "limit" (take 2 young) =<< q (rows <> " :order-by [?a ?n] :limit 2") []
        checkEq "offset and limit" (take 2 (drop 1 young)) =<< q (rows <> " :order-by [?a ?n] :offset 1 :limit 2") []
        checkEq "offset past the end" [] =<< q (rows <> " :order-by [?a ?n] :offset 9") []
        checkEq "a limit from :in" (take 3 young)
          =<< query h ("[:find ?a ?n :in $ ?k " <> rows <> " :order-by [?a ?n] :limit ?k]") [InScalar (VInt 3)]
        checkEq "the map form, a vector of pairs" (take 2 (reverse young))
          =<< query h "{:find [?a ?n] :where [[?e :person/age ?a] [?e :person/name ?n]] :order-by [[?a :desc] [?n :desc]] :limit 2}" []
        -- the cut is after the sort and does not cut the join short: the oldest two, whichever way the rows came
        checkEq "the two oldest of all" [row 75 "Cantor", row 75 "Peirce"] =<< q (rows <> " :order-by [?a :desc ?n] :limit 2") []
        s1 <- try (query h "[:find ?a . :where [?e :person/age ?a] :order-by [?a]]" []) :: IO (Either SomeException [[Value]])
        check "a scalar find is refused, not answered whole" (either (const True) (const False) s1)
        s2 <- try (query h "[:find [?a ...] :where [?e :person/age ?a] :limit 2]" []) :: IO (Either SomeException [[Value]])
        check "so is a collection find" (either (const True) (const False) s2)
        checkEq "...and the khana is still good" [] =<< verify h)

  , ("dtlv: highest walks one attribute from its top: the largest first, ties by id, a page after a page; what it cannot walk is refused", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg = (backendCfg be) { hcClock = clock }
      withHosiCfg cfg dir $ \h -> do
        let ages = [70, 75, 49, 75, 71, 75, 30, 49] :: [Integer]
        ids <- zipWithM (\i a -> person h (T.pack ("p" ++ show (i :: Int))) a) [1 ..] ages
        let facts = [ Datom e "person/age" (VInt (fromIntegral a)) | (e, a) <- zip ids ages ]
            top = reverse (sortOn (\d -> (dV d, dE d)) facts)           -- the largest value, then the largest id, first
            place d = (dV d, dE d)
            pages n = go Nothing
              where go cur = do
                      ds <- highest h "person/age" cur n
                      if null ds then pure [] else (ds :) <$> go (Just (place (last ds)))
        checkEq "the top three" (take 3 top) =<< highest h "person/age" Nothing 3
        checkEq "a count larger than there are" top =<< highest h "person/age" Nothing 50
        forM_ [1, 2, 3, 8, 9 :: Int] $ \n -> do
          ps <- endsIn ("pages of " ++ show n) (pages n)
          checkEq ("pages of " ++ show n ++ ", one after the other, are all of it") top (concat ps)
          check ("...and none is longer than " ++ show n) (all ((<= n) . length) ps)
        -- the cursor is a place in the order, a fact that is there or not: what comes after it, and not itself
        forM_ (map place top ++ [(VInt 73, 5), (VInt 1000, 1), (VInt (-5), 1)]) $ \p@(v, e) ->
          checkEq ("after " ++ show (v, e)) [ d | d <- top, place d < p ] =<< highest h "person/age" (Just p) 50
        viaQuery <- query h "[:find ?e ?a :where [?e :person/age ?a] :order-by [?a :desc ?e :desc]]" []
        checkEq "the order is the one :order-by gives" [ [VInt (dE d), dV d] | d <- top ] viaQuery
        -- strings by their bytes, doubles with a sign, a many-valued attribute: every fact is one
        checkEq "strings" (map (VStr . T.pack . ("p" ++) . show) [8, 7, 6 :: Int]) . map dV =<< highest h "person/name" Nothing 3
        sc <- forM [1.5, -2.0, 3.25] $ \x -> (\r -> wrTemps r M.! 1) <$> write h [Create (Tmp 1) "person/score" (Lit (VDouble x))]
        checkEq "doubles, a negative one last" [VDouble 3.25, VDouble 1.5, VDouble (-2.0)] . map dV =<< highest h "person/score" Nothing 9
        checkEq "...and the ids that go with them" [sc !! 2, sc !! 0, sc !! 1] . map dE =<< highest h "person/score" Nothing 9
        tg <- (\r -> wrTemps r M.! 1) <$> write h [ Create (Tmp 1) "person/tag" (Lit (VStr t)) | t <- ["a", "b", "c"] ]
        checkEq "one entity's many values are many facts" [Datom tg "person/tag" (VStr "c"), Datom tg "person/tag" (VStr "b")]
          =<< highest h "person/tag" Nothing 2
        checkEq "an attribute nobody has" [] =<< highest h "person/friend" Nothing 5
        -- it is "now": a fact that was taken out, or an entity that was erased, is not in it
        _ <- write h [Delete (Id (ids !! 1)) "person/age" (Lit (VInt 75))]
        checkEq "a deleted fact" [ d | d <- top, dE d /= ids !! 1 ] =<< highest h "person/age" Nothing 50
        _ <- erase h [EraseIds [ids !! 3]]
        checkEq "an erased entity" [ d | d <- top, dE d `notElem` [ids !! 1, ids !! 3] ] =<< highest h "person/age" Nothing 50
        -- what is refused is the caller's, and nothing is asked of Datalevin
        let refused why act = do
              r <- try act :: IO (Either HosiError [Datom])
              check why (case r of Left (BadArgument _) -> True; _ -> False)
        refused "a count of 0" (highest h "person/age" Nothing 0)
        refused "a negative count" (highest h "person/age" Nothing (-3))
        refused "an attribute that is not in the schema" (highest h "no/such" Nothing 3)
        refused "a cursor of another type" (highest h "person/age" (Just (VStr "x", 1)) 3)
        refused "a cursor of another type (a double for a long)" (highest h "person/age" (Just (VDouble 70, 1)) 3)
        checkEq "...and the khana is still good" [] =<< verify h
      -- a schema with an attribute of no type, and a tuple: neither has an order to walk
      withTmp $ \dir2 -> do
        let sch2 = M.fromList [ ("a/any", attr TAny), ("f/from", ref), ("f/to", ref), ("f/pair", identity (tuple ["f/from", "f/to"])) ]
        h2 <- openHosiWith cfg dir2 sch2
        _ <- write h2 [Create (Tmp 1) "a/any" (Lit (VInt 5)), Create (Tmp 2) "f/from" (Lit (VInt 5)), Create (Tmp 2) "f/to" (Lit (VInt 6))]
        let refused2 why act = do
              r <- try act :: IO (Either HosiError [Datom])
              check why (case r of Left (BadArgument _) -> True; _ -> False)
        refused2 "an attribute with no type" (highest h2 "a/any" Nothing 3)
        refused2 "a tuple" (highest h2 "f/pair" Nothing 3)
        closeHosi h2)

  , ("dtlv: highestIn merges the runs of some keys: the newest first whoever wrote it, ties by id, a page after a page; the keys not asked for, a half-made tuple and what cannot be walked stay out", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg = (backendCfg be) { hcClock = clock }
          sch = M.fromList
            [ ("note/by", attr TString), ("note/at", attr TLong), ("note/by+at", tuple ["note/by", "note/at"])
            , ("x/any", attr TAny), ("x/txt", attr TString)
            , ("r/by", ref), ("r/at", attr TLong), ("r/by+at", tuple ["r/by", "r/at"])
            , ("x/three", tuple ["note/by", "note/at", "x/txt"]), ("x/txtorder", tuple ["note/by", "x/txt"])
            , ("x/anykey", tuple ["x/any", "note/at"]) ]
          -- authors that are prefixes of one another and one that is not ASCII; the notes of a group have the same author and time
          authors = ["a", "a1", "a10", "a100", "b", "\233"] :: [T.Text]
          spec = [ (authors !! (g `mod` 6), fromIntegral g * 10 + 5) | i <- [0 .. 59 :: Int], let g = i `div` 3 ] :: [(T.Text, Int64)]
          newest ns = reverse (sortOn (\(_, t, e) -> (t, e)) ns)
          mk (a, t, e) = Datom e "note/by+at" (VTuple [Just (VStr a), Just (VInt t)])
          place (_, t, e) = (t, e)
      h <- openHosiWith cfg dir sch
      notes0 <- forM spec $ \(a, t) -> do
        r <- write h [Create (Tmp 1) "note/by" (Lit (VStr a)), Create (Tmp 1) "note/at" (Lit (VInt t))]
        pure (a, t, wrTemps r M.! 1)
      -- a note with a key and no time, and one with a time and no key: a tuple that is half made
      _ <- write h [Create (Tmp 1) "note/by" (Lit (VStr "a"))]
      _ <- write h [Create (Tmp 1) "note/at" (Lit (VInt 99999))]
      let wantFor ns ks = map mk [ x | x@(a, _, _) <- newest ns, a `elem` ks ]
          ask ks cur n = highestIn h "note/by+at" (map VStr ks) cur n
          sets = [ ["a"], ["a1"], ["a", "a1"], ["a1", "a10", "a100"], authors, ["b", "\233"], ["a", "a", "b"], ["a", "nobody"], ["nobody"], [] ] :: [[T.Text]]
          pagesOf ks n = go Nothing
            where go cur = do
                    ds <- ask ks cur n
                    case ds of
                      [] -> pure []
                      _  -> case dV (last ds) of
                              VTuple [_, Just o] -> (ds :) <$> go (Just (o, dE (last ds)))
                              v                  -> error ("a fact that is not a pair: " ++ show v)
      forM_ sets $ \ks -> checkEq ("all of it for " ++ show ks) (wantFor notes0 ks) =<< ask ks Nothing 500
      checkEq "the top four of all" (take 4 (wantFor notes0 authors)) =<< ask authors Nothing 4
      forM_ [1, 2, 7, 100 :: Int] $ \n ->
        forM_ [authors, ["a1", "a10"], ["a"]] $ \ks -> do
          ps <- endsIn ("pages of " ++ show n) (pagesOf ks n)
          checkEq ("pages of " ++ show n ++ " for " ++ show ks ++ " are all of it") (wantFor notes0 ks) (concat ps)
          check "...and none is longer than the count" (all ((<= n) . length) ps)
      -- the cursor is a place in the order: what is after it, and not itself; a place that nobody has works too
      forM_ (map place notes0 ++ [(73, 5), (100000, 1), (-5, 1)]) $ \p@(t, e) ->
        forM_ [authors, ["a1", "b"]] $ \ks ->
          checkEq ("after " ++ show p ++ " for " ++ show ks)
            (map mk [ x | x@(a, _, _) <- newest notes0, a `elem` ks, place x < p ]) =<< ask ks (Just (VInt t, e)) 500
      -- it is "now": a fact that was taken out takes the note out of it (its tuple is half made), and so does an erase
      let (a5, t5, e5) = notes0 !! 5
          (_, _, e7)   = notes0 !! 7
      _ <- write h [Delete (Id e5) "note/at" (Lit (VInt t5))]
      _ <- erase h [EraseIds [e7]]
      let notes1 = [ x | x@(_, _, e) <- notes0, e /= e5, e /= e7 ]
      checkEq ("a deleted time (" ++ T.unpack a5 ++ ") and an erased note") (wantFor notes1 authors) =<< ask authors Nothing 500
      -- the keys may be references (an author is an entity): three authors, twelve notes, some of the same author and time
      let ents = [ e | (_, _, e) <- take 3 notes1 ]
      rs <- forM [0 .. 11 :: Int] $ \i -> do
        let au = ents !! (i `mod` 3); at = 1000 + fromIntegral (i `div` 4) :: Int64
        r <- write h [Create (Tmp 1) "r/by" (RefTo (Id au)), Create (Tmp 1) "r/at" (Lit (VInt at))]
        pure (au, at, wrTemps r M.! 1)
      let wantR ks = [ Datom e "r/by+at" (VTuple [Just (VInt (fromIntegral au)), Just (VInt at)])
                     | (au, at, e) <- reverse (sortOn (\(_, t, e) -> (t, e)) rs), au `elem` ks ]
          askR ks cur n = highestIn h "r/by+at" (map (VInt . fromIntegral) ks) cur n
      forM_ [[head ents], [head ents, ents !! 2], ents] $ \ks -> do
        checkEq ("references as keys: " ++ show ks) (wantR ks) =<< askR ks Nothing 500
        let go cur = do
              ds <- askR ks cur 3
              case ds of
                [] -> pure []
                _  -> case dV (last ds) of
                        VTuple [_, Just o] -> (ds ++) <$> go (Just (o, dE (last ds)))
                        v                  -> error ("a fact that is not a pair: " ++ show v)
        checkEq ("...in pages of 3: " ++ show ks) (wantR ks) =<< endsIn "pages of references" (go Nothing)
      -- what is refused is the caller's
      let refused why act = do
            r <- try act :: IO (Either HosiError [Datom])
            check why (case r of Left (BadArgument _) -> True; _ -> False)
      refused "a count of 0" (ask authors Nothing 0)
      refused "an attribute that is not in the schema" (highestIn h "no/such" [VStr "a"] Nothing 3)
      refused "an attribute that is not a tuple" (highestIn h "note/at" [VInt 1] Nothing 3)
      refused "a tuple of three" (highestIn h "x/three" [VStr "a"] Nothing 3)
      refused "an order part that is not a long" (highestIn h "x/txtorder" [VStr "a"] Nothing 3)
      refused "a key part with no type" (highestIn h "x/anykey" [VInt 1] Nothing 3)
      refused "a key of another type" (highestIn h "note/by+at" [VStr "a", VInt 7] Nothing 3)
      refused "a cursor of another type" (highestIn h "note/by+at" [VStr "a"] (Just (VStr "x", 1)) 3)
      checkEq "...and the khana is still good" [] =<< verify h
      closeHosi h)

  , ("dtlv: a composite tuple is made by the khana: it is read and looked up, one pair is held once, it follows its parts, and it is not written", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg = (backendCfg be) { hcClock = clock }
          sch ps = M.fromList [ ("person/name", identity (attr TString)), ("follow/from", ref), ("follow/to", ref), ("follow/pair", identity (tuple ps)) ]
          fromTo = sch ["follow/from", "follow/to"]
          pairOf x y = VTuple [Just (VInt (fromIntegral x)), Just (VInt (fromIntegral y))]
          follow h x y = (\r -> wrTemps r M.! 1) <$> write h [ Create (Tmp 1) "follow/from" (RefTo (Id x)), Create (Tmp 1) "follow/to" (RefTo (Id y)) ]
          pairs h = sort <$> query h "[:find ?e ?p :where [?e :follow/pair ?p]]" []
          pairText x y = "[" <> T.pack (show x) <> " " <> T.pack (show y) <> "]"
          rejected r = case r of Left (WriteRejected _) -> True; _ -> False
      h0 <- openHosiWith cfg dir fromTo
      let named n = (\r -> wrTemps r M.! 1) <$> write h0 [Create (Tmp 1) "person/name" (Lit (VStr n))]
      a <- named "Frege"; b <- named "Peirce"; c <- named "Boole"
      ab <- follow h0 a b
      checkEq "the entity carries its pair" (Just [pairOf a b]) . M.lookup "follow/pair" =<< getEntity h0 ab
      checkEq "looked up by a tuple written in the query" [[VInt (fromIntegral ab)]]
        =<< query h0 ("[:find ?e :where [?e :follow/pair " <> pairText a b <> "]]") []
      checkEq "looked up by a tuple given as an input" [[VInt (fromIntegral ab)]]
        =<< query h0 "[:find ?e :in $ ?p :where [?e :follow/pair ?p]]" [InScalar (pairOf a b)]
      checkEq "the other way round is another pair" [] =<< query h0 "[:find ?e :in $ ?p :where [?e :follow/pair ?p]]" [InScalar (pairOf b a)]
      s0 <- stats h0
      dup <- try (follow h0 a b)
      check "the same pair again is refused" (rejected dup)
      s1 <- stats h0
      checkEq "...and left nothing in the log" (stTailBytes s0, stNextSeq s0) (stTailBytes s1, stNextSeq s1)
      ba <- follow h0 b a
      ac <- follow h0 a c
      checkEq "three pairs" [ [VInt (fromIntegral e), pairOf x y] | (e, x, y) <- sort [(ab, a, b), (ba, b, a), (ac, a, c)] ] =<< pairs h0
      w1 <- try (write h0 [Create (Tmp 1) "follow/pair" (Lit (pairOf a b))]) :: IO (Either HosiError WriteReport)
      check "a tuple is not written" (case w1 of Left (BadArgument _) -> True; _ -> False)
      w2 <- try (write h0 [Create (Id ab) "follow/pair" (Lit (VInt 1))])
      check "...and nothing else is written to its place" (rejected w2)
      -- a part that goes takes the pair with it: the half that is left is a pair with nil
      _ <- write h0 [Delete (Id ac) "follow/to" (Lit (VInt (fromIntegral c)))]
      checkEq "a part taken out: the pair has a nil" (Just [VTuple [Just (VInt (fromIntegral a)), Nothing]]) . M.lookup "follow/pair" =<< getEntity h0 ac
      -- ...and two half-made pairs that are alike are one pair, to a unique tuple
      half1 <- try (write h0 [Create (Tmp 1) "follow/from" (RefTo (Id c))]) :: IO (Either HosiError WriteReport)
      check "a first half-made pair is taken" (either (const False) (const True) half1)
      half2 <- try (write h0 [Create (Tmp 1) "follow/from" (RefTo (Id c))]) :: IO (Either HosiError WriteReport)
      check "a second one like it is refused" (rejected half2)
      checkEq "verify" [] =<< verify h0
      before <- pairs h0
      closeHosi h0
      h1 <- openHosiWith cfg dir fromTo
      checkEq "after a reopen the pairs are the same" before =<< pairs h1
      rebuildKhana h1
      checkEq "the khana made again from the events holds the same pairs" before =<< pairs h1
      checkEq "verify after the rebuild" [] =<< verify h1
      -- erase takes a pair out like anything else, and clean leaves no trace of it in the file
      _ <- erase h1 [EraseIds [ab]]
      checkEq "the erased entity has no pair" [] =<< query h1 "[:find ?e :in $ ?p :where [?e :follow/pair ?p]]" [InScalar (pairOf a b)]
      _ <- clean h1 defaultCleanOptions
      checkEq "verify after clean" [] =<< verify h1
      closeHosi h1
      -- the parts of a tuple are part of the schema: another order is another tuple, and the khana is made again under it
      h2 <- openHosiWith cfg dir (sch ["follow/to", "follow/from"])
      checkEq "the other order, from the same events" (Just [pairOf a b]) . M.lookup "follow/pair" =<< getEntity h2 ba
      checkEq "verify under the other schema" [] =<< verify h2
      closeHosi h2)

  , ("dtlv: awkward strings and keywords come back exactly as they went in", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg ((backendCfg be) { hcClock = clock }) dir $ \h -> do
        let odd_ = "q\"uote \\ back\nline\ttab\r\f\b é日本 😀 ;comment #_ {[(" :: T.Text
            iri  = Keyword "http://example.org/ns#name"
        r <- write h [ Create (Tmp 1) "person/tag" (Lit (VStr odd_)), Create (Tmp 1) "person/score" (Lit (VDouble 1.0e-5))
                     , Create (Tmp 1) "misc/kw" (Lit (VKw iri)), Create (Tmp 1) "misc/flag" (Lit (VBool True)) ]
        let i = wrTemps r M.! 1
        e <- getEntity h i
        checkEq "the string" (Just [VStr odd_]) (M.lookup "person/tag" e)
        checkEq "the double" (Just [VDouble 1.0e-5]) (M.lookup "person/score" e)
        checkEq "the keyword" (Just [VKw iri]) (M.lookup "misc/kw" e)
        checkEq "the bool" (Just [VBool True]) (M.lookup "misc/flag" e)
        checkEq "verify" [] =<< verify h)
  ]

-- | Only the resident native `dtlv` (what needs the executable itself: its dump, its mark in the khana's file).
nativeTests :: FilePath -> [Test]
nativeTests exe =
  [ ("a khana from before the schema mark was kept is trusted by nobody: it is made again once at open, and then kept", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg = (backendCfg (KhanaDtlvRepl exe)) { hcClock = clock }
          kdir = dir </> khanaDirName (KhanaDtlvRepl exe)
      h <- openHosiWith cfg dir schema
      a <- person h "Frege" 70
      closeHosi h
      -- as if a build that kept no mark had made this khana: take the mark out, and leave a file to see whether the khana is replaced
      r <- newDtlvRepl exe
      d <- openDtlv (NativeRepl r) kdir (khanaSchema schema)
      dtlvTransact d [DRetract metaEntity schemaMarkAttr (VStr (schemaMark schema))]
      closeDtlv d
      stopDtlvRepl r
      writeFile (kdir </> "the-old-khana") "x"
      h2 <- openHosiWith cfg dir schema
      check "no mark: the khana was made again" . not =<< doesFileExist (kdir </> "the-old-khana")
      checkEq "Frege" (Just [VInt 70]) =<< ageOf h2 a
      closeHosi h2
      writeFile (kdir </> "the-old-khana") "x"
      h3 <- openHosiWith cfg dir schema
      check "and with the mark it made, it is kept" =<< doesFileExist (kdir </> "the-old-khana")
      checkEq "verify clean" [] =<< verify h3
      closeHosi h3)

  , ("a schema that is refused at open is not written into the old khana: it holds what it held, types and datoms", withTmp $ \dir0 -> do
      (_, clock) <- newClock nov11
      let cfg = (backendCfg (KhanaDtlvRepl exe)) { hcClock = clock }
          schemaLong = M.insert "person/name" (attr TLong) schema      -- the names are strings: a number it cannot be
      -- once with the events sealed (the sealed fold refuses), once with them in the log (the log's fold refuses)
      forM_ [True, False] $ \sealed -> do
        let dir  = dir0 </> show sealed
            kdir = dir </> khanaDirName (KhanaDtlvRepl exe)
            -- everything the khana holds (its catalog of types, and its datoms), as `dtlv dump` writes it
            catalog = do out <- readProcess exe ["-d", kdir, "dump", "-g"] ""
                         pure (sort (lines out))
            what s = (if sealed then "sealed: " else "in the log: ") ++ s
        h <- openHosiWith cfg dir schema
        _ <- person h "Frege" 70
        when sealed (void (seal h))
        closeHosi h
        before <- catalog
        check (what "the dump has the names in it") (any (":person/name" `isInfixOf`) before)
        r <- try (openHosiWith cfg dir schemaLong) :: IO (Either HosiError Hosi)
        check (what "the open was refused") (either (\e -> case e of SchemaRefused _ -> True; _ -> False) (const False) r)
        checkEq (what "the khana is as it was") before =<< catalog)

  ]

-- | Only the resident native @dtlv repl@ ('KhanaDtlvRepl').
replTests :: FilePath -> [Test]
replTests exe =
  -- what the JVM runs (the write queue and the regions), run on the repl as it is
  writeQueueTests (KhanaDtlvRepl exe) ++
  regionsTests (KhanaDtlvRepl exe) ++
  [ ("the repl stays up: a hundred operations cost no more processes than the first few", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let counter = dir </> "calls"
          wrapper = dir </> "dtlv-counting.sh"
          cfg = (backendCfg (KhanaDtlvRepl wrapper)) { hcClock = clock }
      writeFile wrapper ("#!/bin/sh\necho x >> " ++ show counter ++ "\nexec " ++ show exe ++ " \"$@\"\n")
      callProcess "chmod" ["+x", wrapper]
      let calls = (do s <- readFile counter; let n = length (lines s) in n `seq` pure n) `catch` \(_ :: IOException) -> pure (0 :: Int)
      h <- openHosiWith cfg (dir </> "db") schema
      ids <- forM [1 .. 50 :: Integer] (\i -> person h (T.pack ("p" ++ show i)) (20 + i))
      n1 <- calls
      check ("the look at open and the one repl: at most two processes, not " ++ show n1) (n1 <= 2)
      forM_ ids $ \i -> ageOf h i
      _ <- getEntities h ids
      _ <- queryMany h [ (t, []) | t <- diffQueries ]
      n2 <- calls
      checkEq "fifty writes and as many reads later: the same processes" n1 n2
      closeHosi h)

  , ("values that look like the repl's own words: newlines, the prompt, a lone paren, unicode, a thousand characters", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith ((backendCfg (KhanaDtlvRepl exe)) { hcClock = clock }) dir schema
      let names = [ "a\nuser> b", "user> ", "\nuser> ", ")))", "\"quoted\" ; not a comment", "日本語 한국어 😀", T.replicate 1000 "x\n" ]
      ids <- forM (zip names [1 ..]) (\(n, i) -> person h n i)
      es <- getEntities h ids
      checkEq "every name comes back as it went in" (map (\n -> Just [VStr n]) names) (map (M.lookup "person/name") es)
      checkEq "verify" [] =<< verify h
      closeHosi h)

  , ("a big batch goes through the repl's one line: twenty thousand facts in one write", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith ((backendCfg (KhanaDtlvRepl exe)) { hcClock = clock }) dir schema
      r <- write h (concat [ [ Create (Tmp i) "person/name" (Lit (VStr (T.pack ("big" ++ show i)))), Create (Tmp i) "person/age" (Lit (VInt (fromIntegral i))) ] | i <- [1 .. 10000] ])
      check "all the temps got ids" (M.size (wrTemps r) == 10000)
      checkEq "one of them" (Just [VInt 777]) =<< ageOf h (wrTemps r M.! 777)
      checkEq "verify" [] =<< verify h
      closeHosi h)

  , ("the repl dies: the call that met it fails, the next one starts a new one and nothing is lost", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let pidFile = dir </> "repl.pid"
          wrapper = dir </> "dtlv-wrapper.sh"
      writeFile wrapper ("#!/bin/sh\necho $$ >> " ++ show pidFile ++ "\nexec " ++ show exe ++ " \"$@\"\n")
      callProcess "chmod" ["+x", wrapper]
      h <- openHosiWith ((backendCfg (KhanaDtlvRepl wrapper)) { hcClock = clock }) (dir </> "db") schema
      a <- person h "Frege" 70
      pids <- lines <$> readFile pidFile
      callProcess "kill" ["-9", last pids]                                       -- the repl is the last process started
      r <- try (person h "Peirce" 75) :: IO (Either SomeException HosiId)
      check "the call that met the dead repl fails" (either (const True) (const False) r)
      b <- person h "Boole" 49                                                   -- a new repl
      checkEq "Frege survived" (Just [VInt 70]) =<< ageOf h a
      checkEq "Boole is in" (Just [VInt 49]) =<< ageOf h b
      checkEq "verify" [] =<< verify h
      closeHosi h)

  , ("the khana has a write-ahead log, as under the JVM: an erased value is in txlog/ until clean, and in no file after it", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith ((backendCfg (KhanaDtlvRepl exe)) { hcClock = clock }) dir schema
      _ <- person h (T.pack (marker 1)) 1
      b <- person h (T.pack (marker 2)) 2
      check "the khana's txlog directory is there" =<< doesDirectoryExist (dir </> "khana-dtlv" </> "txlog")
      _ <- erase h [EraseIds [b]]
      check "erased, not cleaned: the value is still in the khana's log" . (> 0) =<< markerCount (BC.pack (marker 2)) (dir </> "khana-dtlv" </> "txlog")
      _ <- clean h defaultCleanOptions
      checkEq "cleaned: it is in no file" 0 =<< markerCount (BC.pack (marker 2)) dir
      check "the other one is" . (> 0) =<< markerCount (BC.pack (marker 1)) dir
      closeHosi h)

  , ("the repl is killed after a run of writes (no close): the next open finds the khana whole, or behind the tail and catches up", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let pidFile = dir </> "repl.pid"
          wrapper = dir </> "dtlv-wrapper.sh"
          cfg = (backendCfg (KhanaDtlvRepl wrapper)) { hcClock = clock }
      writeFile wrapper ("#!/bin/sh\necho $$ >> " ++ show pidFile ++ "\nexec " ++ show exe ++ " \"$@\"\n")
      callProcess "chmod" ["+x", wrapper]
      h <- openHosiWith cfg (dir </> "db") schema
      ids <- forM [1 .. 30 :: Integer] (\i -> person h (T.pack ("p" ++ show i)) (100 + i))
      pids <- lines <$> readFile pidFile
      callProcess "kill" ["-9", last pids]
      closeHosi h `catch` \(_ :: SomeException) -> pure ()
      h2 <- openHosiWith cfg (dir </> "db") schema
      checkEq "every one of the thirty is in the khana" (map (\i -> Just [VInt (100 + i)]) [1 .. 30]) =<< mapM (ageOf h2) ids
      checkEq "verify" [] =<< verify h2
      closeHosi h2)

  , ("a dtlv that cannot start gives an error with its last words, and does not hang", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let fake = dir </> "not-dtlv.sh"
      writeFile fake "#!/bin/sh\necho 'no thanks' >&2\nexit 3\n"
      callProcess "chmod" ["+x", fake]
      r <- try (openHosiWith ((backendCfg (KhanaDtlvRepl fake)) { hcClock = clock }) (dir </> "db") schema) :: IO (Either SomeException Hosi)
      case r of
        Left e  -> check ("the message tells what dtlv said: " ++ show e) ("no thanks" `isInfixOf` show e)
        Right h -> closeHosi h >> throwIO (userError "it should not have opened"))

  , ("a missing dtlv is found before anything is touched", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      r <- try (openHosiWith ((backendCfg (KhanaDtlvRepl "/no/such/dtlv")) { hcClock = clock }) dir schema) :: IO (Either HosiError Hosi)
      check "a missing dtlv is BackendMissing" (either (\e -> case e of BackendMissing _ -> True; _ -> False) (const False) r)
      checkEq "...and no database directory was made" False =<< doesDirectoryExist (dir </> "events"))

  , ("fulltext: the native dtlv cannot run the analyzer, and says so before anything is made", withTmp $ \dir -> do
      r <- try (openHosiWith (defaultConfig { hcKhanaBackend = KhanaDtlvRepl exe }) (dir </> "w") (M.fromList [("note/body", fulltext (attr TString))]))
             :: IO (Either HosiError Hosi)
      check "refused as a schema" (case r of Left (SchemaRefused m) -> "KhanaDatalevinJvm" `isInfixOf` m; _ -> False))

  , ("max and min in :find work through the repl (the 1.2.11 interpreter)", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg ((backendCfg (KhanaDtlvRepl exe)) { hcClock = clock }) dir $ \h -> do
        _ <- person h "Frege" 70
        _ <- person h "Peirce" 75
        checkEq "max" [[VInt 75]] =<< query h "[:find (max ?a) . :where [?e :person/age ?a]]" []
        checkEq "min" [[VInt 70]] =<< query h "[:find (min ?a) . :where [?e :person/age ?a]]" [])
  ]

-- | Only the resident JVM.
regionsTests :: KhanaBackend -> [Test]
regionsTests be =
  [ ("regions: each name is its own world; what is written in one is not in another, it is all there after a restart, and the names on disk are known", withTmp $ \dir -> do
      let cfg = defaultRegionsConfig dir (backendCfg be) sch
          names = ["alpha", "beta", "gamma", "delta", "eps.1_x-2"] :: [T.Text]
      rg <- newRegions cfg
      forM_ (zip [1 ..] names) $ \(n, nm) -> do
        r <- withRegion rg nm (\h -> write h (note n) >> pure ())
        check ("written in " ++ T.unpack nm) (either (const False) (const True) r)
      forM_ (zip [1 ..] names) $ \(n, nm) -> do
        r <- withRegion rg nm (\h -> query h "[:find ?n :where [?e :note/n ?n]]" [])
        checkEq ("only its own in " ++ T.unpack nm) (Right [[VInt n]]) (either (Left . show) Right r)
      closeRegions rg
      rg2 <- newRegions cfg
      r <- withRegion rg2 "gamma" (\h -> query h "[:find ?n :where [?e :note/n ?n]]" [])
      checkEq "after a restart it is all there" (Right [[VInt 3]]) (either (Left . show) Right r)
      ks <- knownRegions rg2
      checkEq "the names on disk" (sort names) (sort ks)
      st <- regionStats rg2
      checkEq "one opened after the restart" 1 (rsOpened st)
      closeRegions rg2)
  , ("regions: a name that cannot be a directory is refused, and nothing on disk is touched", withTmp $ \dir -> do
      let cfg = defaultRegionsConfig dir (backendCfg be) sch
      rg <- newRegions cfg
      forM_ ["", "..", ".", "a/b", ".hidden", "a b", "x\\y", T.replicate 65 "a"] $ \nm -> do
        r <- withRegion rg nm (\_ -> pure ())
        check ("refused: " ++ show nm) (case r of Left (BadRegionName _) -> True; _ -> False)
      checkEq "nothing was made" [] =<< SD.listDirectory dir
      closeRegions rg)
  , ("regions: at the limit the one used longest ago is closed and comes back with its data; when every open region is in use the answer is TooManyBusy and nothing is closed", withTmp $ \dir -> do
      let cfg = (defaultRegionsConfig dir (backendCfg be) sch) { rgMaxOpen = 2 }
      rg <- newRegions cfg
      forM_ (zip [1 ..] ["a", "b", "c"] :: [(Int64, T.Text)]) $ \(n, nm) -> withRegion rg nm (\h -> write h (note n))
      checkEq "a was closed to make room" ["b", "c"] . sort =<< openRegionNames rg
      r <- withRegion rg "a" (\h -> query h "[:find ?n :where [?e :note/n ?n]]" [])
      checkEq "a comes back with its data" (Right [[VInt 1]]) (either (Left . show) Right r)
      st <- regionStats rg
      checkEq "two were closed to make room" 2 (rsEvicted st)
      r2 <- withRegion rg "a" (\_ -> withRegion rg "x" (\_ -> withRegion rg "y" (\_ -> pure ())))
      check "all in use: TooManyBusy" (case r2 of Right (Right (Left (TooManyBusy 2))) -> True; _ -> False)
      checkEq "nothing was closed under anyone" 2 . length =<< openRegionNames rg
      st2 <- regionStats rg
      checkEq "counted" 1 (rsBusy st2)
      check "the advice tells of it" (any ("TooManyBusy" `L.isInfixOf`) (regionsAdvice cfg st2))
      closeRegions rg)
  , ("regions: a region that cannot be opened is answered OpenFailed and the others go on", withTmp $ \dir -> do
      let cfg = defaultRegionsConfig dir (backendCfg be) sch
      writeFile (dir </> "bad") "a file where the directory of the region should be"
      rg <- newRegions cfg
      r <- withRegion rg "bad" (\_ -> pure ())
      check "OpenFailed with a reason" (case r of Left (OpenFailed why) -> not (null why); _ -> False)
      r2 <- withRegion rg "good" (\h -> write h (note 7) >> query h "[:find ?n :where [?e :note/n ?n]]" [])
      checkEq "the other works" (Right [[VInt 7]]) (either (Left . show) Right r2)
      st <- regionStats rg
      checkEq "counted" 1 (rsOpenFailed st)
      check "the advice tells of it" (any ("failed to open" `L.isInfixOf`) (regionsAdvice cfg st))
      closeRegions rg)
  , ("regions: pinned, a region is not closed to make room; let go, it can be", withTmp $ \dir -> do
      let cfg = (defaultRegionsConfig dir (backendCfg be) sch) { rgMaxOpen = 1 }
      rg <- newRegions cfg
      Right (_, unpin) <- pinRegion rg "a"
      r <- withRegion rg "b" (\_ -> pure ())
      check "b has no place while a is pinned" (case r of Left (TooManyBusy 1) -> True; _ -> False)
      unpin
      r2 <- withRegion rg "b" (\_ -> pure ())
      check "b has it once a is let go" (either (const False) (const True) r2)
      checkEq "a was closed" ["b"] =<< openRegionNames rg
      closeRegions rg
      r3 <- withRegion rg "a" (\_ -> pure ())
      check "closed: RegionsClosed" (case r3 of Left RegionsClosed -> True; _ -> False))
  , ("regions: spread over two resident processes, each region is still its own world", withTmp $ \dir -> do
      let cfg = (defaultRegionsConfig dir (backendCfg be) sch) { rgJvms = 2 }
          names = ["r1", "r2", "r3", "r4"] :: [T.Text]
      rg <- newRegions cfg
      forM_ (zip [1 ..] names) $ \(n, nm) -> withRegion rg nm (\h -> write h (note n))
      forM_ (zip [1 ..] names) $ \(n, nm) -> do
        r <- withRegion rg nm (\h -> query h "[:find ?n :where [?e :note/n ?n]]" [])
        checkEq ("only its own in " ++ T.unpack nm) (Right [[VInt n]]) (either (Left . show) Right r)
      closeRegions rg)
  ]
  where
    sch = M.fromList [ ("note/n", attr TLong) ]
    note n = [Create (Tmp 1) "note/n" (Lit (VInt n))]

writeQueueTests :: KhanaBackend -> [Test]
writeQueueTests be =
  [ ("write queue: many writers at once; each gets its own answer and its own id, every write is in the events and in the khana, and the trips are fewer than the writes", withTmp $ \dir -> do
      let cfg = backendCfg be
          sch = M.fromList [ ("note/n", attr TLong), ("note/by", attr TString) ]
          note n = [Create (Tmp 1) "note/n" (Lit (VInt (fromIntegral (n :: Int)))), Create (Tmp 1) "note/by" (Lit (VStr "x"))]
          writers = 16; per = 10
      h <- openHosiWith cfg dir sch
      q <- newWriteQueue h defaultQueueConfig
      outs <- forM [0 .. writers - 1] $ \t -> do
        v <- newEmptyMVar
        _ <- forkIO $ do
          rs <- forM [0 .. per - 1] $ \k -> do
            let n = t * per + k
            r <- submit q (note n)
            pure (n, r)
          putMVar v rs
        pure v
      got <- concat <$> mapM takeMVar outs
      let idOf (n, Written wr) = Just (n, wrTemps wr M.! 1)
          idOf _               = Nothing
          ok = [ p | Just p <- map idOf got ]
      checkEq "every write was Written" (writers * per) (length ok)
      checkEq "each write has an id of its own" (writers * per) (M.size (M.fromList [ (i, ()) | (_, i) <- ok ]))
      rows <- query h "[:find ?e ?n :where [?e :note/n ?n]]" []
      checkEq "the khana holds exactly these, each id with its own number"
              (sort [ [VInt (fromIntegral i), VInt (fromIntegral n)] | (n, i) <- ok ]) (sort rows)
      st <- queueStats q
      checkEq "all were taken in" (writers * per) (qsSubmitted st)
      checkEq "none turned away" 0 (qsBusy st)
      checkEq "none crashed" 0 (qsCrashedTrips st)
      check "never more trips than writes" (qsBatches st >= 1 && qsBatches st <= writers * per)
      closeWriteQueue q
      r <- submit q (note 0)
      check "a closed room answers Closed" (case r of Closed -> True; _ -> False)
      closeHosi h)
  , ("write queue: a room that is full answers Busy and takes nothing; the advice says so, and says nothing when all is well", withTmp $ \dir -> do
      let cfg = backendCfg be
          sch = M.fromList [ ("note/n", attr TLong) ]
          note = [Create (Tmp 1) "note/n" (Lit (VInt 1))]
      h <- openHosiWith cfg dir sch
      let tiny = defaultQueueConfig { qcMaxPending = 0 }
      qFull <- newWriteQueue h tiny
      r <- submit qFull note
      check "Busy" (case r of Busy -> True; _ -> False)
      checkEq "nothing was written" [] =<< query h "[:find (count ?e) :where [?e :note/n]]" []
      st <- queueStats qFull
      checkEq "one turned away" 1 (qsBusy st)
      check "the advice tells of it" (any ("turned away" `L.isInfixOf`) (queueAdvice tiny st))
      closeWriteQueue qFull
      q <- newWriteQueue h defaultQueueConfig
      r2 <- submit q note
      check "a write goes through" (case r2 of Written _ -> True; _ -> False)
      st2 <- queueStats q
      checkEq "nothing to say when all is well" [] (queueAdvice defaultQueueConfig st2)
      closeWriteQueue q
      closeHosi h)
  , ("write queue: a trip that fails is answered Crashed with a reason, the caller is not left waiting, and the advice tells of it", withTmp $ \dir -> do
      let cfg = backendCfg be
          sch = M.fromList [ ("note/n", attr TLong) ]
          note = [Create (Tmp 1) "note/n" (Lit (VInt 1))]
      h <- openHosiWith cfg dir sch
      q <- newWriteQueue h defaultQueueConfig
      r1 <- submit q note
      check "the first write goes through" (case r1 of Written _ -> True; _ -> False)
      closeHosi h                                   -- from here every trip fails
      r2 <- submit q note
      check "Crashed, with a reason" (case r2 of Crashed why -> not (null why); _ -> False)
      r3 <- submit q note
      check "...and the room still answers the next" (case r3 of Crashed _ -> True; _ -> False)
      st <- queueStats q
      check "the trips that failed are counted" (qsCrashedTrips st >= 2)
      check "the advice tells of them" (any ("trips failed" `L.isInfixOf`) (queueAdvice defaultQueueConfig st))
      closeWriteQueue q)
  ]

jvmTests :: FilePath -> FilePath -> [Test]
jvmTests java jar =
  writeQueueTests (KhanaDatalevinJvm java jar) ++
  regionsTests (KhanaDatalevinJvm java jar) ++
  [ ("fulltext: words are found (letters and digits, lower-cased; Han, kana and hangul by pairs); an erased note is found no more, and clean leaves its words in no file", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg = (backendCfg (KhanaDatalevinJvm java jar)) { hcClock = clock }
          sch = M.fromList [ ("note/body", fulltext (attr TString)), ("note/by", attr TString) ]
          search h q = sort <$> query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]]]" [InScalar (VStr q)]
          note h t = (\r -> wrTemps r M.! 1) <$> write h [Create (Tmp 1) "note/body" (Lit (VStr t)), Create (Tmp 1) "note/by" (Lit (VStr "x"))]
          ids = map (\i -> [VInt (fromIntegral i)])
          secret = "秘密の言葉"
      h <- openHosiWith cfg dir sch
      n1 <- note h "The quick brown fox, 3 jumps!"
      n2 <- note h (T.pack ("zebra " ++ T.unpack secret ++ " crossing"))
      n3 <- note h "ZEBRA zebra Zebra crossing"
      n4 <- note h "한국어 문장입니다"
      checkEq "a word" (ids [n1]) =<< search h "fox"
      checkEq "...in any case" (ids [n2, n3]) =<< search h "ZeBrA"
      checkEq "a number" (ids [n1]) =<< search h "3"
      checkEq "the punctuation is not a word" (ids [n1]) =<< search h "jumps"
      checkEq "a word of Han and kana, found by two of its characters" (ids [n2]) =<< search h "秘密"
      checkEq "...and by the whole of it" (ids [n2]) =<< search h secret
      checkEq "hangul too" (ids [n4]) =<< search h "문장"
      checkEq "a word that is nowhere" [] =<< search h "unicorn"
      checkEq "{:top 1} gives the best one only" 1 . length
        =<< query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q {:top 1}) [[?e ?a ?v]]]]" [InScalar (VStr "zebra")]
      checkEq "with no attribute, every fulltext attribute is searched" (ids [n1])
        =<< (sort <$> query h "[:find ?e :in $ ?q :where [(fulltext $ ?q) [[?e _ _]]]]" [InScalar (VStr "fox")])
      -- a word is found in a note that has it, whichever attribute: the attribute is part of the question
      checkEq "the other attribute is not searched" [] =<< search h "x"
      checkEq "verify" [] =<< verify h
      -- it follows the data: a value taken out is not found, a new one is
      _ <- write h [Delete (Id n1) "note/body" (Lit (VStr "The quick brown fox, 3 jumps!")), Create (Id n1) "note/body" (Lit (VStr "a quiet owl"))]
      checkEq "the old words of a changed value are gone" [] =<< search h "fox"
      checkEq "...and the new ones are there" (ids [n1]) =<< search h "owl"
      -- an erased note is not found, and after clean its words are in no file
      before <- markerCount (TE.encodeUtf8 "秘密") dir
      check "(the words are in some file before an erase)" (before > 0)
      _ <- erase h [EraseIds [n2]]
      checkEq "an erased note is not found" [] =<< search h "秘密"
      rep <- clean h defaultCleanOptions
      check "clean made the khana again, as it must for an index of words" (crKhanaCleaned rep && crKhanaRebuilt rep)
      checkEq "its words are in no file" 0 =<< markerCount (TE.encodeUtf8 "秘密") dir
      checkEq "the others are still found" (ids [n3]) =<< search h "zebra"
      checkEq "verify after clean" [] =<< verify h
      closeHosi h
      -- another schema is another khana: no fulltext, then fulltext again
      h2 <- openHosiWith cfg dir (M.fromList [ ("note/body", attr TString), ("note/by", attr TString) ])
      r <- try (search h2 "owl") :: IO (Either SomeException [[Value]])
      check "without the flag there is nothing to search with" (either (const True) (const False) r)
      closeHosi h2
      h3 <- openHosiWith cfg dir sch
      checkEq "with the flag again, the khana was made again and finds them" (ids [n1]) =<< search h3 "owl"
      closeHosi h3)

  , ("the khana has a write-ahead log: an erased value is in txlog/ until clean, and in no file after it", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith ((backendCfg (KhanaDatalevinJvm java jar)) { hcClock = clock }) dir schema
      _ <- person h (T.pack (marker 1)) 1
      b <- person h (T.pack (marker 2)) 2
      check "the khana's txlog directory is there" =<< doesDirectoryExist (dir </> "khana-dtlv" </> "txlog")
      _ <- erase h [EraseIds [b]]
      check "erased, not cleaned: the value is still in the khana's log" . (> 0) =<< markerCount (BC.pack (marker 2)) (dir </> "khana-dtlv" </> "txlog")
      _ <- clean h defaultCleanOptions
      checkEq "cleaned: it is in no file" 0 =<< markerCount (BC.pack (marker 2)) dir
      check "the other one is" . (> 0) =<< markerCount (BC.pack (marker 1)) dir
      closeHosi h)

  , ("a missing java, or a missing jar, is found before anything is touched", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let bad be d = do r <- try (openHosiWith ((backendCfg be) { hcClock = clock }) d schema) :: IO (Either HosiError Hosi)
                        pure (either (\e -> case e of BackendMissing _ -> True; _ -> False) (const False) r)
      checkEq "no java" True =<< bad (KhanaDatalevinJvm "/no/such/java" jar) (dir </> "a")
      checkEq "no jar" True =<< bad (KhanaDatalevinJvm java "/no/such/datalevin.jar") (dir </> "b")
      checkEq "...and nothing was made" [False, False] =<< mapM (doesDirectoryExist . (dir </>)) ["a", "b"])

  , ("(max ?x) / (min ?x) in :find mean what they say", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg ((backendCfg (KhanaDatalevinJvm java jar)) { hcClock = clock }) dir $ \h -> do
        mapM_ (\(n, a) -> person h n a) [("Frege", 70), ("Peirce", 75), ("Boole", 49), ("Cantor", 75)]
        checkEq "max" [[VInt 75]] =<< query h "[:find (max ?a) . :where [?e :person/age ?a]]" []
        checkEq "min" [[VInt 49]] =<< query h "[:find (min ?a) . :where [?e :person/age ?a]]" [])

  , ("the JVM dies in the middle: that one call fails, the next starts a new JVM, nothing is lost", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let pidFile = dir </> "jvm.pid"
          wrapper = dir </> "java-wrapper.sh"
      writeFile wrapper ("#!/bin/sh\necho $$ > " ++ show pidFile ++ "\nexec " ++ show java ++ " \"$@\"\n")
      callProcess "chmod" ["+x", wrapper]
      h <- openHosiWith ((backendCfg (KhanaDatalevinJvm wrapper jar)) { hcClock = clock }) (dir </> "db") schema
      a <- person h "Frege" 70
      pid <- takeWhile (/= '\n') <$> readFile pidFile
      callProcess "kill" ["-9", pid]
      r <- try (person h "Peirce" 75) :: IO (Either SomeException HosiId)
      check "the call that met the dead JVM fails" (either (const True) (const False) r)
      b <- person h "Boole" 49                                                   -- a new JVM
      checkEq "Frege survived" (Just [VInt 70]) =<< ageOf h a
      checkEq "Boole is in" (Just [VInt 49]) =<< ageOf h b
      checkEq "verify" [] =<< verify h
      closeHosi h)

  , ("a java that cannot start Datalevin gives an error with its last words, and does not hang", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let fake = dir </> "not-java.sh"
      writeFile fake "#!/bin/sh\necho 'no thanks' >&2\nexit 3\n"
      callProcess "chmod" ["+x", fake]
      r <- try (openHosiWith ((backendCfg (KhanaDatalevinJvm fake jar)) { hcClock = clock }) (dir </> "db") schema) :: IO (Either SomeException Hosi)
      case r of
        Left e  -> check ("the message tells what the JVM said: " ++ show e) ("no thanks" `isInfixOf` show e)
        Right h -> closeHosi h >> throwIO (userError "it should not have opened"))
  ]

-- | The two ways of reaching Datalevin look at the same files.
mixedTests :: FilePath -> FilePath -> FilePath -> [Test]
mixedTests exe java jar =
  [ ("one khana-dtlv/, two ways in: written through the JVM, read through the repl, and back", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let open be = openHosiWith ((backendCfg be) { hcClock = clock }) dir schema
      h1 <- open (KhanaDatalevinJvm java jar)
      a <- person h1 "Frege" 70
      closeHosi h1
      h2 <- open (KhanaDtlvRepl exe)
      checkEq "the repl reads what the JVM wrote" (Just [VInt 70]) =<< ageOf h2 a
      b <- person h2 "Peirce" 75
      closeHosi h2
      h3 <- open (KhanaDatalevinJvm java jar)
      checkEq "the JVM reads what the repl wrote" (Just [VInt 75]) =<< ageOf h3 b
      checkEq "verify" [] =<< verify h3
      closeHosi h3)
  ]

-- ---------------------------------------------------------------------------
-- Generators

genValue :: Gen Value
genValue = oneof
  [ VInt . fromIntegral <$> (arbitrary :: Gen Int)
  , VDouble . (/ 8) . fromIntegral <$> (arbitrary :: Gen Int)
  , VStr . T.pack <$> listOf (elements ("abc é日本 \n\0" :: String))
  , VBool <$> arbitrary
  , (VKw . Keyword . T.pack) <$> listOf1 (elements ("abc/:" :: String)) ]

-- | A chunk source that has no @mphf.idx@ (as a chunk far away, whose index we do not hold).
withoutMphf :: ChunkSource -> ChunkSource
withoutMphf src = src { csWhole = \f -> if f == chunkMphfFile then pure Nothing else csWhole src f }

isStar :: Probe a -> Bool
isStar (Star _) = True
isStar _        = False

-- | Every @k@th element, as many times round as needed (a spread of ids to look up).
cycle' :: Int -> [a] -> [a]
cycle' k xs = case drop (k - 1) xs of
  []      -> []
  (y : _) -> y : cycle' k (drop k xs)

genEvents :: Gen [E.Event]
genEvents = do
  n <- choose (1, 60)
  forM [1 .. n] $ \s -> do
    i <- choose (100, 118)                           -- few distinct ids, so runs have several events
    a <- elements ["name", "age", "tag", "x/y"]
    v <- genValue
    pure (E.Event (fromIntegral (s :: Int)) KCreate i a v)

-- | Events whose values come from a small pool, so that a chunk's dictionary has something to do
-- (long strings, keywords, a few numbers, and the two zeros).
genRepetitiveEvents :: Gen [E.Event]
genRepetitiveEvents = do
  pool <- resize 6 $ listOf1 (frequency
    [ (3, VStr . T.pack <$> vectorOf 12 (elements ("abcde" :: String)))
    , (2, VKw . Keyword . T.pack <$> vectorOf 9 (elements ("xyz/" :: String)))
    , (1, VInt <$> arbitrary)
    , (1, elements [VDouble 0.0, VDouble (-0.0), VDouble 1.5])
    , (1, VBool <$> arbitrary) ])
  n <- choose (1, 80)
  forM [1 .. n] $ \s -> do
    i <- choose (100, 112)
    a <- elements ["name", "type", "x/y"]
    v <- elements pool
    pure (E.Event (fromIntegral (s :: Int)) KCreate i a v)

copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
  createDirectoryIfMissing True to
  names <- listDirectory from
  forM_ names $ \n -> do
    isDir <- doesDirectoryExist (from </> n)
    if isDir then copyDir (from </> n) (to </> n) else copyFile (from </> n) (to </> n)

-- ---------------------------------------------------------------------------
-- Words made by a tokenizer of the host (the tokenizer for Japanese is an example of its own: example/hosi-mikke)

-- | A tokenizer made for the tests, with no program behind it: the words of a text are its runs of letters and digits in lower case,
-- and, if it is to, a plural "s" is cut off a word of more than three letters ("cats" and "cat" are one word).  A text with "boom" in it
-- makes it throw.  It gives back how many times it was closed.
toyTokenizer :: T.Text -> Bool -> IO (Tokenizer, IORef Int)
toyTokenizer mark cutPlural = do
  closes <- newIORef 0
  let wordsOf t = T.unwords [ cut w | w <- T.words (T.map (\c -> if isAlphaNum c then toLower c else ' ') t) ]
      cut w = if cutPlural && T.length w > 3 && T.last w == 's' then T.init w else w
      make ts | any ("boom" `T.isInfixOf`) ts = throwIO (userError "boom")
              | otherwise                      = pure (map wordsOf ts)
  pure (Tokenizer { tkMark = mark, tkWords = make, tkClose = modifyIORef' closes (+ 1) }, closes)

-- | Fulltext attributes whose words a tokenizer of the host makes, on a khana of any backend.
wordsTests :: KhanaBackend -> [Test]
wordsTests be =
  [ ("words: a tokenizer makes the words of a fulltext attribute (found by them, not as written); a changed or erased value is found no more, and clean leaves its words in no file", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      (tk, closes) <- toyTokenizer "toy/1" True
      let cfg = (backendCfg be) { hcClock = clock, hcTokenizer = Just tk }
          sch = M.fromList [ ("note/body", fulltext (attr TString)), ("note/by", attr TString) ]
          search h q = sort <$> query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]]]" [InScalar (VStr q)]
          note h t = (\r -> wrTemps r M.! 1) <$> write h [Create (Tmp 1) "note/body" (Lit (VStr t)), Create (Tmp 1) "note/by" (Lit (VStr "x"))]
          ids = map (\i -> [VInt (fromIntegral i)])
      h <- openHosiWith cfg dir sch
      cats   <- note h "Cats sleep all day"
      dog    <- note h "A dog barks at night"
      ver    <- note h "Version 3.10 released"
      secret <- note h "The password is swordfish"
      checkEq "a word as the tokenizer made it: cat finds Cats" (ids [cats]) =<< search h "cat"
      checkEq "a search text is made into words by it, too: CATS finds Cats" (ids [cats]) =<< search h "CATS"
      checkEq "barks, bark" (ids [dog]) =<< search h "bark"
      checkEq "a number" (ids [ver]) =<< search h "3"
      checkEq "a word that is nowhere" [] =<< search h "unicorn"
      checkEq "the text may be written in the query" (ids [cats])
        =<< (sort <$> query h "[:find ?e :where [(fulltext $ :note/body \"cats\") [[?e ?a ?v]]]]" [])
      checkEq "with no attribute, every fulltext attribute is searched" (ids [cats])
        =<< (sort <$> query h "[:find ?e :in $ ?q :where [(fulltext $ ?q) [[?e _ _]]]]" [InScalar (VStr "cats")])
      checkEq "several words: all of them must be there (cat and day are in one note, day and night in none)" (ids [cats]) =<< search h "cat day"
      checkEq "...and a note that has one of them is not found" [] =<< search h "day night"
      checkEq "the other attribute is not searched" [] =<< search h "x"
      -- the text is kept as written: the words are beside it
      ent <- getEntity h cats
      checkEq "the attribute has the text as written" (Just [VStr "Cats sleep all day"]) (M.lookup "note/body" ent)
      check "...and its words are in another attribute" (M.member "note/body.words" ent)
      checkEq "verify" [] =<< verify h
      -- it follows the data: a value taken out is not found, a new one is
      _ <- write h [Delete (Id cats) "note/body" (Lit (VStr "Cats sleep all day")), Create (Id cats) "note/body" (Lit (VStr "Birds sing"))]
      checkEq "the old words of a changed value are gone" [] =<< search h "sleep"
      checkEq "...and the new ones are there" (ids [cats]) =<< search h "sing"
      -- a value taken out and nothing put in its place (a new value would replace the words by itself: this is the Delete alone)
      gone <- note h "An elephant has a trunk"
      checkEq "(found before)" (ids [gone]) =<< search h "elephant"
      _ <- write h [Delete (Id gone) "note/body" (Lit (VStr "An elephant has a trunk"))]
      checkEq "a value taken out is found no more" [] =<< search h "elephant"
      -- a note with no words, and then with some: the old words do not stay
      empty <- note h ""
      _ <- write h [Delete (Id empty) "note/body" (Lit (VStr "")), Create (Id empty) "note/body" (Lit (VStr "rain"))]
      checkEq "a text that had no words, and now has" (ids [empty]) =<< search h "rain"
      -- an erased note is not found, and after clean its words are in no file
      before <- markerCount (TE.encodeUtf8 "swordfish") dir
      check "(the words are in some file before an erase)" (before > 0)
      _ <- erase h [EraseIds [secret]]
      checkEq "an erased note is not found" [] =<< search h "swordfish"
      rep <- clean h defaultCleanOptions
      check "clean made the khana again, as it must for an index of words" (crKhanaCleaned rep && crKhanaRebuilt rep)
      checkEq "its words are in no file" 0 =<< markerCount (TE.encodeUtf8 "swordfish") dir
      checkEq "the others are still found" (ids [dog]) =<< search h "bark"
      checkEq "verify after clean" [] =<< verify h
      -- closing the world closes the tokenizer it made the runtime with
      checkEq "(not closed yet)" 0 =<< readIORef closes
      closeHosi h
      checkEq "closeHosi closed the tokenizer" 1 =<< readIORef closes
      -- opened again with the same tokenizer (the same mark): the khana is kept, not made again
      h2 <- openHosiWith cfg dir sch
      checkEq "found after a reopen" (ids [dog]) =<< search h2 "bark"
      closeHosi h2
      -- another mark is other words (here: the plural stays), so another khana, made again from the events
      (tk2, _) <- toyTokenizer "toy/2" False
      h3 <- openHosiWith (cfg { hcTokenizer = Just tk2 }) dir sch
      checkEq "the plural stays now: barks finds it" (ids [dog]) =<< search h3 "barks"
      checkEq "...and bark does not" [] =<< search h3 "bark"
      closeHosi h3
      -- no tokenizer: the schema is a fulltext one, and only the JVM has an analyzer of its own
      r <- try (openHosiWith (cfg { hcTokenizer = Nothing }) dir sch) :: IO (Either HosiError Hosi)
      case be of
        KhanaDatalevinJvm _ _ -> either (const (pure ())) closeHosi r
        _ -> check "without a tokenizer the native dtlv refuses a fulltext attribute" (case r of Left (SchemaRefused _) -> True; _ -> False))

  , ("words: a search of several words finds what has all of them (of the best :top by relevance, those that have every word)", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      (tk, _) <- toyTokenizer "toy/1" True
      let sch = M.fromList [ ("note/body", fulltext (attr TString)), ("note/by", attr TString) ]
          ask h qtxt t = sort <$> query h qtxt [InScalar (VStr t)]
          search h = ask h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]]]"
          note h t = (\r -> wrTemps r M.! 1) <$> write h [Create (Tmp 1) "note/body" (Lit (VStr t)), Create (Tmp 1) "note/by" (Lit (VStr "x"))]
          ids = sort . map (\i -> [VInt (fromIntegral i)])
      h <- openHosiWith ((backendCfg be) { hcClock = clock, hcTokenizer = Just tk }) dir sch
      d1 <- note h "Cats sleep all day"          -- cat sleep all day
      d2 <- note h "A dog barks at night"        -- a dog bark at night
      d3 <- note h "Cats chase dogs at night"    -- cat chase dog at night
      d4 <- note h "Day night day night"
      d5 <- note h "Dogs bark all night"         -- dog bark all night
      d6 <- note h "Dogma of the night"          -- dogma: not the word dog
      let dogNight = ids [d2, d3, d5]
      checkEq "one word is what it was: every note that has it" (ids [d2, d3, d4, d5, d6]) =<< search h "night"
      checkEq "two words: the notes that have both" dogNight =<< search h "dog night"
      checkEq "...in any order" dogNight =<< search h "night dog"
      checkEq "...a word said twice is one word" dogNight =<< search h "dog DOGS night"
      checkEq "...made into words as the tokenizer does (dogs is dog)" dogNight =<< search h "dogs night"
      checkEq "a word is a word, not the start of one (dogma is not dog)" [] =<< search h "dogma dog"
      checkEq "three words" (ids [d3]) =<< search h "cat chase night"
      checkEq "a word that is nowhere" [] =<< search h "cat unicorn"
      checkEq "a word that the analyzer of the search drops (at) is still asked for" (ids [d2, d3]) =<< search h "dog at night"
      -- the same, with the text written in the query, with no attribute, with a short binding, beside other clauses
      checkEq "the text written in the query" dogNight
        =<< (sort <$> query h "[:find ?e :where [(fulltext $ :note/body \"dogs night\") [[?e ?a ?v]]]]" [])
      checkEq "no attribute: every fulltext attribute is searched" dogNight
        =<< ask h "[:find ?e :in $ ?q :where [(fulltext $ ?q) [[?e _ _]]]]" "dog night"
      checkEq "a binding of ?e only" dogNight
        =<< ask h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e]]]]" "dog night"
      checkEq "beside another clause" dogNight
        =<< ask h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]] [?e :note/by \"x\"]]" "dog night"
      checkEq "two searches in one query" (ids [d1])
        =<< (sort <$> query h "[:find ?e :where [(fulltext $ :note/body \"cat day\") [[?e]]] [(fulltext $ :note/body \"cat sleep\") [[?e]]]]" [])
      checkEq "two searches that find different notes do not share what the checks add" (sort [[VInt (fromIntegral d1), VInt (fromIntegral f)] | f <- [d2, d3, d5]])
        =<< (sort <$> query h "[:find ?e ?f :where [(fulltext $ :note/body \"cat day\") [[?e]]] [(fulltext $ :note/body \"dog night\") [[?f]]]]" [])
      checkEq "the query as a map" dogNight
        =<< ask h "{:find [?e] :in [$ ?q] :where [[(fulltext $ :note/body ?q) [[?e ?a ?v]]]]}" "dog night"
      -- :top: the best N by relevance, and of them the ones that have every word
      let top n = ask h (T.pack ("[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q {:top " ++ show (n :: Int) ++ "}) [[?e ?a ?v]]]]"))
      checkEq "a :top as big as the notes is the whole answer" dogNight =<< top 100 "dog night"
      one <- top 1 "dog night"
      check "{:top 1}: at most one, and one that has both" (length one <= 1 && all (`elem` dogNight) one)
      checkEq "{:top 1} on one word gives the best one" 1 . length =<< top 1 "night"
      -- a call inside or / not cannot have its words checked there: several words are refused, one word is as before
      checkEq "one word inside not" (ids [d1])
        =<< (sort <$> query h "[:find ?e :where [?e :note/by \"x\"] (not [(fulltext $ :note/body \"night\") [[?e]]])]" [])
      checkEq "...and a word said twice is still one word there" (ids [d1])
        =<< (sort <$> query h "[:find ?e :where [?e :note/by \"x\"] (not [(fulltext $ :note/body \"night NIGHT\") [[?e]]])]" [])
      refused <- try (query h "[:find ?e :where [?e :note/by \"x\"] (not [(fulltext $ :note/body \"dog night\") [[?e]]])]" []) :: IO (Either SomeException [[Value]])
      check "several words inside not are refused, not searched as if there were one" (either (const True) (const False) refused)
      refusedOr <- try (query h "[:find ?e :where (or [(fulltext $ :note/body \"dog night\") [[?e]]] [(fulltext $ :note/body \"cat\") [[?e]]])]" []) :: IO (Either SomeException [[Value]])
      check "...and inside or" (either (const True) (const False) refusedOr)
      checkEq "verify" [] =<< verify h
      closeHosi h)

  , ("words: a tokenizer that throws rejects the write (nothing is written) and fails the query; the world goes on", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      (tk, _) <- toyTokenizer "toy/1" True
      let sch = M.fromList [ ("note/body", fulltext (attr TString)) ]
          search h q = query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]]]" [InScalar (VStr q)]
      h <- openHosiWith ((backendCfg be) { hcClock = clock, hcTokenizer = Just tk }) dir sch
      r1 <- write h [Create (Tmp 1) "note/body" (Lit (VStr "fine"))]
      bad <- try (write h [Create (Tmp 1) "note/body" (Lit (VStr "a boom here"))])
      check "the write is rejected" (case bad of Left (WriteRejected _) -> True; _ -> False)
      q <- try (search h "boom") :: IO (Either SomeException [[Value]])
      check "a search that makes it throw fails" (either (const True) (const False) q)
      checkEq "nothing of the rejected write is there" [[VInt (fromIntegral (wrTemps r1 M.! 1))]] =<< search h "fine"
      checkEq "verify (the events and the khana agree: the rejected write is in neither)" [] =<< verify h
      _ <- write h [Create (Tmp 1) "note/body" (Lit (VStr "and onward"))]
      checkEq "a write after it goes in" 1 . length =<< search h "onward"
      closeHosi h)

  , ("words: a runtime made with a tokenizer is the one a Hosi opened on it uses, and closeHosi leaves it for whoever made the runtime", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      (tk, closes) <- toyTokenizer "toy/1" True
      let sch = M.fromList [ ("note/body", fulltext (attr TString)) ]
      rt <- newKhanaRuntimeWith be (Just tk)
      h <- openHosiShared rt ((backendCfg be) { hcClock = clock }) dir sch
      r <- write h [Create (Tmp 1) "note/body" (Lit (VStr "Cats sleep"))]
      found <- query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e]]]]" [InScalar (VStr "cat")]
      checkEq "found" [[VInt (fromIntegral (wrTemps r M.! 1))]] found
      closeHosi h
      checkEq "the Hosi did not close what it was lent" 0 =<< readIORef closes
      closeKhanaRuntime rt
      checkEq "the runtime closes the tokenizer" 1 =<< readIORef closes)
  ]
