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
import           Data.IORef
import           Data.List (sort, sortOn, nub, isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word
import           System.Directory
import           System.Environment (lookupEnv)
import           System.Exit
import           System.FilePath ((</>))
import           System.IO
import           System.Process (callProcess)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Hosigraph
import           Hosigraph.Bin
import           Hosigraph.Chunk
import           Takka.Edn (Edn(..), readEdn)
import           Takka.Pull (Pulled(..))
import           Hosigraph.Id (epochMs, idMs, makeId)
import           Hosigraph.Khana.Dtlv (ednKw, ednStr, ednVal, schemaEdn, tameEdn)
import           Hosigraph.Manifest
import           Hosigraph.Mphf
import           Hosigraph.Probe (confirmSlot)
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

prop :: Testable p => Int -> p -> IO ()
prop n p = do
  r <- quickCheckWithResult stdArgs { maxSuccess = n, chatty = False } p
  unless (isSuccess r) (throwIO (userError (output r)))

main :: IO ()
main = do
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
      nativeRun = case mexe of
        Just exe -> tag "[dtlv]" (hosiTests (KhanaDtlv exe) ++ dtlvTests (KhanaDtlv exe) ++ nativeTests exe)
        Nothing  -> []
      jvmRun = case mjar of
        Just jar -> tag "[jvm]" (hosiTests (KhanaDatalevinJvm java jar) ++ dtlvTests (KhanaDatalevinJvm java jar) ++ jvmTests java jar)
        Nothing  -> []
      mixedRun = case (mexe, mjar) of
        (Just exe, Just jar) -> tag "[dtlv+jvm]" (mixedTests exe java jar)
        _                    -> []
      dtlvRun = nativeRun ++ jvmRun ++ mixedRun
  when (null nativeRun) $ putStrLn "  (dtlv tests skipped: set HOSIGRAPH_DTLV to the path of the dtlv executable)"
  when (null jvmRun) $ putStrLn "  (jvm tests skipped: set HOSIGRAPH_DATALEVIN_JAR to the Datalevin standalone jar, and HOSIGRAPH_JAVA to java)"
  results <- forM (pureTests ++ hosiTests KhanaDatalevinNative ++ dtlvRun) $ \(name, act) -> do
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

nov11, dec05 :: Integer
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

ageOf :: Hosi -> HosiId -> IO (Maybe [Value])
ageOf h i = M.lookup "person/age" <$> getEntity h i

reopen :: HosiConfig -> FilePath -> Hosi -> IO Hosi
reopen cfg dir h = closeHosi h >> openHosiWith cfg dir schema

-- ---------------------------------------------------------------------------
-- The tests

-- | Tests that need no khana at all.
pureTests :: [Test]
pureTests =
  [ ("sha256: known vectors", do
      checkEq "empty" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (sha256Hex B.empty)
      checkEq "abc" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" (sha256Hex "abc")
      checkEq "two blocks" "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
        (sha256Hex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
      checkEq "10000 patterned bytes" "c3d5cea5e36f10537bb7aeb14e6740a973a9dd5b55c22347d465693f1ded4ced" (sha256Hex (B.pack [ fromIntegral (i * 7 `mod` 251 :: Int) | i <- [0 .. 9999] ])))
  , ("crc32: standard check value", checkEq "crc32 123456789" 0xCBF43926 (crc32 "123456789"))

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

  , ("dtlv edn: a string is written so that a reader gives the same text back (QuickCheck)", prop 300 $
      forAll (listOf (frequency [(8, arbitrary), (2, elements ("\"\\\n\r\t\0\DEL\f\b é日本😀" :: String))])) $ \s ->
        let t = T.pack s in readEdn (ednStr t) === Right (EStr t))
  , ("dtlv edn: tameEdn spells \\f and \\b as \\u escapes and leaves \\\\f alone", do
      checkEq "f, b and an escaped backslash before f" (Right (EStr "x\fy\bz\\f")) (readEdn (tameEdn "\"x\\fy\\bz\\\\f\""))
      checkEq "other escapes are untouched" (Right (EStr "a\nb\"c")) (readEdn (tameEdn "\"a\\nb\\\"c\"")))
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
        -- an id inside chunk 1's range that was never written: turned away by the fingerprint
        let ghost = makeId (idMs a + 1800000) 0
        check "ghost id is inside chunk 1's range" (ghost > a && ghost < a2)
        (eg, tg) <- historyTrace h ghost
        checkEq "no events" [] eg
        checkEq "fingerprint rejects (the body was not read)" [NotStar] (map snd (ltProbed tg)))

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
  ]
  where
    cfgWith c = (backendCfg be) { hcClock = c }
    kd = khanaDirName be
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

-- | A pull's attributes are a list in the in-process engine and a map in Datalevin: compare them as sets of attributes.
normCell :: Cell -> Cell
normCell c = case c of CPull p -> CPull (go p); _ -> c
  where go p = case p of
          PEntity kvs -> PEntity (sortOn fst [ (k, go v) | (k, v) <- kvs ])
          PMany xs    -> PMany (map go xs)
          _           -> p

-- | What "now" looks like from the outside.
nowOf :: Hosi -> [HosiId] -> IO ([M.Map Keyword [Value]], [[[Value]]], [[Cell]], Word64)
nowOf h ids = do
  es <- getEntities h ids
  qs <- map sort <$> queryMany h [ (qt, []) | qt <- diffQueries ]
  cs <- sortOn show . map (map normCell) <$> queryCells h pullQuery []
  n  <- stKhanaDatoms <$> stats h
  pure (es, qs, cs, n)

sameNow :: Hosi -> Hosi -> String -> [HosiId] -> IO ()
sameNow a b label ids = do
  x <- nowOf a ids
  y <- nowOf b ids
  checkEq label x y

-- | The checks that hold for every way of reaching Datalevin.
dtlvTests :: KhanaBackend -> [Test]
dtlvTests be =
  [ ("dtlv vs the in-process engine: the same writes give the same \"now\" (also after seal, reopen, rebuild)", withTmp $ \dir -> do
      (clk, clock) <- newClock nov11
      let cfg be = (backendCfg be) { hcClock = clock }
          openB be sub = openHosiWith (cfg be) (dir </> sub) schema
      hv <- openB KhanaDatalevinNative "native"
      hd <- openB be "dtlv"
      let twice act = (,) <$> act hv <*> act hd
          first = [ Create (Tmp 1) "person/name" (Lit (VStr "Frege")), Create (Tmp 1) "person/age" (Lit (VInt 70))
                  , Create (Tmp 1) "person/score" (Lit (VDouble 1.5))
                  , Create (Tmp 1) "person/tag" (Lit (VStr "a")), Create (Tmp 1) "person/tag" (Lit (VStr "b"))
                  , Create (Tmp 2) "person/name" (Lit (VStr "Peirce")), Create (Tmp 2) "person/age" (Lit (VInt 75))
                  , Create (Tmp 1) "person/friend" (RefTo (Tmp 2)) ]
      (r1, r1') <- twice (\h -> write h first)
      checkEq "the same ids" (wrTemps r1) (wrTemps r1')
      let frege = wrTemps r1 M.! 1; peirce = wrTemps r1 M.! 2
      sameNow hv hd "after the first write" [frege, peirce]
      _ <- twice seal
      sameNow hv hd "after seal" [frege, peirce]
      modifyIORef' clk (+ 86400000)
      _ <- twice $ \h -> write h [ Create (Id frege) "person/age" (Lit (VInt 71)), Delete (Id frege) "person/tag" (Lit (VStr "a"))
                                 , Delete (Id peirce) "person/age" (Lit (VInt 75)) ]
      (r3, _) <- twice $ \h -> write h [ Create (Tmp 1) "person/name" (Lit (VStr "Boole")), Create (Tmp 1) "person/age" (Lit (VInt 49))
                                       , Create (Tmp 1) "person/friend" (RefTo (Id frege)) ]
      let boole = wrTemps r3 M.! 1
          ids = [frege, peirce, boole]
      sameNow hv hd "after an update, deletes and a third person" ids
      checkEq "the in-process khana verifies" [] =<< verify hv
      checkEq "dtlv verifies" [] =<< verify hd
      closeHosi hv >> closeHosi hd
      hv2 <- openB KhanaDatalevinNative "native"
      hd2 <- openB be "dtlv"
      sameNow hv2 hd2 "after reopen" ids
      rebuildKhana hv2 >> rebuildKhana hd2
      sameNow hv2 hd2 "after rebuildKhana" ids
      closeHosi hv2 >> closeHosi hd2)

  , ("dtlv: the khana can change backend; the events stay the one truth", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let cfg be = (backendCfg be) { hcClock = clock }
      h <- openHosiWith (cfg KhanaDatalevinNative) dir schema
      a <- person h "Frege" 70
      _ <- seal h
      b <- person h "Peirce" 75
      closeHosi h
      h2 <- openHosiWith (cfg be) dir schema          -- no khana-dtlv/ yet: it is folded from the events
      checkEq "Frege" (Just [VInt 70]) =<< ageOf h2 a
      checkEq "Peirce" (Just [VInt 75]) =<< ageOf h2 b
      c <- person h2 "Boole" 49
      checkEq "verify (dtlv)" [] =<< verify h2
      closeHosi h2
      h3 <- openHosiWith (cfg KhanaDatalevinNative) dir schema               -- and back: the in-process khana is behind and catches up
      checkEq "Boole, after going back" (Just [VInt 49]) =<< ageOf h3 c
      checkEq "verify (in-process)" [] =<< verify h3
      closeHosi h3
      checkEq "two khanas, side by side" [True, True] =<< mapM (doesDirectoryExist . (dir </>)) ["khana", "khana-dtlv"])

  , ("dtlv: what it cannot do says so, and a missing dtlv is found before anything is touched", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      h <- openHosiWith ((backendCfg be) { hcClock = clock }) dir schema
      _ <- person h "Frege" 70
      r1 <- try (withKhana h (\_ -> pure ())) :: IO (Either HosiError ())
      check "withKhana is refused" (either (\e -> case e of BadArgument _ -> True; _ -> False) (const False) r1)
      r2 <- try (query h "[:find ?n :in $ % :where [?e :person/name ?n]]" [InRules []]) :: IO (Either SomeException [[Value]])
      check "InRules is refused" (either (const True) (const False) r2)
      checkEq "...and the khana is still good" [] =<< verify h
      closeHosi h)

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

-- | Only the native `dtlv`.
nativeTests :: FilePath -> [Test]
nativeTests exe =
  [ ("a missing dtlv is found before anything is touched", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      r <- try (openHosiWith ((backendCfg (KhanaDtlv "/no/such/dtlv")) { hcClock = clock }) dir schema) :: IO (Either HosiError Hosi)
      check "a missing dtlv is a BadArgument" (either (\e -> case e of BadArgument _ -> True; _ -> False) (const False) r)
      checkEq "...and no database directory was made" False =<< doesDirectoryExist (dir </> "events"))

  , ("a batch costs one dtlv process, however many items are in it", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let counter = dir </> "calls"
          wrapper = dir </> "dtlv-counting.sh"
          cfg = (backendCfg (KhanaDtlv wrapper)) { hcClock = clock }
      writeFile wrapper ("#!/bin/sh\necho x >> " ++ show counter ++ "\nexec " ++ show exe ++ " \"$@\"\n")
      callProcess "chmod" ["+x", wrapper]
      let calls = (do s <- readFile counter; let n = length (lines s) in n `seq` pure n)    -- counted now, not when somebody looks
                    `catch` \(_ :: IOException) -> pure (0 :: Int)
      h <- openHosiWith cfg (dir </> "db") schema
      a <- person h "p0" 20
      closeHosi h
      copyDir (dir </> "db" </> "khana-dtlv") (dir </> "saved")
      h1 <- openHosiWith cfg (dir </> "db") schema
      ids <- forM [1 .. 5 :: Integer] (\i -> person h1 (T.pack ("p" ++ show i)) (20 + i))
      n0 <- calls
      _ <- getEntities h1 (a : ids)
      n1 <- calls
      checkEq "six entities: one process" 1 (n1 - n0)
      _ <- queryMany h1 [ (t, []) | t <- diffQueries ]
      n2 <- calls
      checkEq "six queries: one process" 1 (n2 - n1)
      _ <- stats h1
      n3 <- calls
      checkEq "stats: one process" 1 (n3 - n2)
      _ <- writeMany h1 [ [Create (Tmp 1) "person/name" (Lit (VStr (T.pack ("q" ++ show i)))), Create (Tmp 1) "person/age" (Lit (VInt i))] | i <- [1 .. 5] ]
      n4 <- calls
      checkEq "five batches written together: one process" 1 (n4 - n3)
      closeHosi h1
      removePathForcibly (dir </> "db" </> "khana-dtlv")
      renameDirectory (dir </> "saved") (dir </> "db" </> "khana-dtlv")     -- the khana knows only p0; five batches are in the tail
      m0 <- calls
      h2 <- openHosiWith cfg (dir </> "db") schema
      m1 <- calls
      checkEq "open and catch up on five batches: start, read the seq, one transaction" 3 (m1 - m0)
      checkEq "verify" [] =<< verify h2
      closeHosi h2)

  , ("canary: (max ?x) / (min ?x) in :find do not work through `dtlv exec` 1.2.10 (if this fails, it was fixed upstream: update the README)", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      withHosiCfg ((backendCfg (KhanaDtlv exe)) { hcClock = clock }) dir $ \h -> do
        _ <- person h "Frege" 70
        _ <- person h "Peirce" 75
        r <- try (query h "[:find (max ?a) . :where [?e :person/age ?a]]" []) :: IO (Either SomeException [[Value]])
        check "max is still answered with the whole collection, which we cannot read as a number" (either (const True) (const False) r)
        checkEq "avg works" [[VDouble 72.5]] =<< query h "[:find (avg ?a) . :where [?e :person/age ?a]]" [])
  ]

-- | Only the resident JVM.
jvmTests :: FilePath -> FilePath -> [Test]
jvmTests java jar =
  [ ("a missing java, or a missing jar, is found before anything is touched", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let bad be d = do r <- try (openHosiWith ((backendCfg be) { hcClock = clock }) d schema) :: IO (Either HosiError Hosi)
                        pure (either (\e -> case e of BadArgument _ -> True; _ -> False) (const False) r)
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
  [ ("one khana-dtlv/, two ways in: written through the JVM, read through dtlv, and back", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let open be = openHosiWith ((backendCfg be) { hcClock = clock }) dir schema
      h1 <- open (KhanaDatalevinJvm java jar)
      a <- person h1 "Frege" 70
      closeHosi h1
      h2 <- open (KhanaDtlv exe)
      checkEq "dtlv reads what the JVM wrote" (Just [VInt 70]) =<< ageOf h2 a
      b <- person h2 "Peirce" 75
      closeHosi h2
      h3 <- open (KhanaDatalevinJvm java jar)
      checkEq "the JVM reads what dtlv wrote" (Just [VInt 75]) =<< ageOf h3 b
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

genEvents :: Gen [E.Event]
genEvents = do
  n <- choose (1, 60)
  forM [1 .. n] $ \s -> do
    i <- choose (100, 118)                           -- few distinct ids, so runs have several events
    a <- elements ["name", "age", "tag", "x/y"]
    v <- genValue
    pure (E.Event (fromIntegral (s :: Int)) KCreate i a v)

copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
  createDirectoryIfMissing True to
  names <- listDirectory from
  forM_ names $ \n -> do
    isDir <- doesDirectoryExist (from </> n)
    if isDir then copyDir (from </> n) (to </> n) else copyFile (from </> n) (to </> n)
