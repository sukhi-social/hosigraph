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
import           Data.IORef
import           Data.Int (Int64)
import           Data.List (sort, sortOn, nub, isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..))
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
import           Hosigraph.BinaryFuse
import           Hosigraph.Chunk
import qualified Hosigraph.Erase as Erase
import           Takka.Edn (Edn(..), readEdn, renderEdn)
import           Takka.Syntax (Pulled(..))
import           Hosigraph.Id (epochMs, idMs, idRangeBetween, idSeqNo, makeId)
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
  results <- forM (pureTests ++ dtlvRun) $ \(name, act) -> do
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
      countIn 0 <$> B.readFile p
  where
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

  , ("clean: when the compacted copy cannot be made, or is not the khana, the khana is folded again from the events, and the report says so", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let empty = dir </> "empty-db"
          -- a dtlv that, asked to copy, either says "Copy error" and exits with 0 (as the real one does), or copies an empty database
          wrapper how = dir </> ("dtlv-" ++ how ++ ".sh")
          script how = "#!/bin/sh\nif [ \"$4\" = copy ]; then\n" ++ (case how of
            "refuses" -> "  echo 'Copy error: no room on this disk (a test)'; exit 0\n"
            _         -> "  exec " ++ show exe ++ " -d " ++ show empty ++ " -c copy \"$5\"\n") ++ "fi\nexec " ++ show exe ++ " \"$@\"\n"
          one how = do
            let db = dir </> how
                count i = markerCount (BC.pack (marker i)) db
            writeFile (wrapper how) (script how)
            callProcess "chmod" ["+x", wrapper how]
            h <- openHosiWith ((backendCfg (KhanaDtlv (wrapper how))) { hcClock = clock }) db schema
            [a, b] <- forM [1, 2] $ \i -> person h (T.pack (marker i)) (fromIntegral i)
            _ <- seal h
            _ <- erase h [EraseIds [b]]
            rep <- clean h defaultCleanOptions
            checkEq (how ++ ": cleaned, and by folding again") (True, True) (crKhanaCleaned rep, crKhanaRebuilt rep)
            checkEq (how ++ ": the erased marker is in no file, the other is") (0, True) =<< ((,) <$> count 2 <*> ((> 0) <$> count 1))
            checkEq (how ++ ": nothing is left lying about") False =<< doesDirectoryExist (db </> "khana-dtlv.new")
            checkEq (how ++ ": a is there, b is not") [Just [VInt 1], Nothing] =<< mapM (ageOf h) [a, b]
            checkEq (how ++ ": verify clean") [] =<< verify h
            again <- clean h defaultCleanOptions
            checkEq (how ++ ": it is clean now: the next clean has nothing to do") (False, False) (crKhanaCleaned again, crKhanaRebuilt again)
            closeHosi h
      one "refuses"
      one "copies-nothing")

  , ("canary: (max ?x) / (min ?x) in :find do not work through `dtlv exec` 1.2.10 (if this fails, it was fixed upstream: update docs/guide.md)", withTmp $ \dir -> do
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
  [ ("the khana has a write-ahead log: an erased value is in txlog/ until clean, and in no file after it", withTmp $ \dir -> do
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
