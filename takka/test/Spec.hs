{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Self-contained test runner (no test framework needed beyond QuickCheck).
--   build/takka-test            -- run everything
module Main (main) where

import           Control.Concurrent
import           Control.Exception hiding (assert)
import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import           Data.IORef
import           Data.List (nub, permutations, sort)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word
import           System.Directory
import           System.Environment (lookupEnv)
import           System.Exit
import           System.FilePath ((</>))
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Posix.Process (getProcessID)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Takka
import           Takka.Codec
import           Takka.Edn
import qualified Takka.KV as KV

-- ---------------------------------------------------------------------------
-- Harness

{-# NOINLINE counter #-}
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir act = do
  base <- getTemporaryDirectory
  pid  <- getProcessID
  n    <- atomicModifyIORef' counter (\c -> (c + 1, c))
  let d = base </> ("takka-test-" ++ show pid ++ "-" ++ show n)
  bracket_ (createDirectoryIfMissing True d) (removeDirectoryRecursive d) (act d)

testEnvCfg :: EnvConfig
testEnvCfg = defaultEnvConfig { cfgMapSize = 256 * 1024 * 1024, cfgNoSync = True }

-- | TAKKA_TEST_PC=1 runs the whole suite with prefix compression switched on.
{-# NOINLINE prefixOn #-}
prefixOn :: Bool
prefixOn = unsafePerformIO (maybe False (== "1") <$> lookupEnv "TAKKA_TEST_PC")

testCfg :: ConnConfig
testCfg = defaultConnConfig { ccEnv = testEnvCfg, ccPrefixCompression = prefixOn }

withTestConn :: Schema -> (Conn -> IO a) -> IO a
withTestConn sch act = withTempDir $ \d ->
  bracket (openConnWith testCfg d sch) closeConn act

unit :: String -> IO () -> IO Bool
unit name act = do
  r <- try act
  case r of
    Right () -> putStrLn ("  ok    " ++ name) >> pure True
    Left e   -> putStrLn ("  FAIL  " ++ name ++ "\n        " ++ show (e :: SomeException)) >> pure False

prop :: Testable p => String -> Int -> p -> IO Bool
prop name n p = do
  r <- quickCheckWithResult stdArgs { maxSuccess = n, chatty = False } p
  case r of
    Success{} -> putStrLn ("  ok    " ++ name ++ "  (" ++ show n ++ " cases)") >> pure True
    _         -> putStrLn ("  FAIL  " ++ name ++ "\n" ++ output r) >> pure False

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq what expected actual =
  unless (expected == actual) $
    throwIO (userError (what ++ ": expected " ++ show expected ++ ", got " ++ show actual))

assertThat :: String -> Bool -> IO ()
assertThat what ok = unless ok (throwIO (userError what))

expectError :: String -> IO a -> (TakkaError -> Bool) -> IO ()
expectError what act ok = do
  r <- try act
  case r of
    Left e | ok e -> pure ()
           | otherwise -> throwIO (userError (what ++ ": wrong error " ++ show e))
    Right _ -> throwIO (userError (what ++ ": expected an error but it succeeded"))

section :: String -> IO ()
section s = putStrLn ("\n" ++ s)

-- ---------------------------------------------------------------------------
-- Generators

instance Arbitrary Value where
  arbitrary = oneof
    [ VInt <$> oneof [arbitrary, elements [minBound, maxBound, 0, -1, 1]]
    , VDouble <$> arbitrary `suchThat` (not . isNaN)
    , VStr . T.pack <$> listOf (elements ("ab\0\233" :: String))
    , VBool <$> arbitrary
    , VKw . Keyword . T.pack <$> listOf (elements ("ab\0/" :: String)) ]

-- | Strings / keywords long enough to be stored out of line, often sharing long prefixes.
genBigValue :: Gen Value
genBigValue = do
  n <- choose (inlineMax + 1, 3000)
  base <- elements ["x", "ab\0", "\233"]
  tailPart <- listOf (elements ("ab\0" :: String))
  let t = T.take n (T.replicate n base) <> T.pack (take 3 tailPart)
  oneof [pure (VStr t), pure (VKw (Keyword t))]

rankOf :: Value -> Int
rankOf v = case v of VBool _ -> 0; VInt _ -> 1; VDouble _ -> 2; VStr _ -> 3; VKw _ -> 4

modelCompare :: Value -> Value -> Ordering
modelCompare a b = compare (rankOf a) (rankOf b) <> case (a, b) of
  (VBool x, VBool y)       -> compare x y
  (VInt x, VInt y)         -> compare x y
  (VDouble x, VDouble y)   -> compare x y
  (VStr x, VStr y)         -> compare x y
  (VKw (Keyword x), VKw (Keyword y)) -> compare x y
  _                        -> EQ

-- ---------------------------------------------------------------------------
-- Codec

codecTests :: IO [Bool]
codecTests = do
  section "Codec"
  sequence
    [ prop "value round-trip" 2000 $ \v ->
        decodeValue (encodeValue v) === Just (v, B.empty)
    , prop "encoding preserves order" 5000 $ \a b ->
        compare (encodeValue a) (encodeValue b) === modelCompare a b
    , prop "encoding is self-delimiting" 2000 $ \v (B.pack -> suffix) ->
        decodeValue (B.append (encodeValue v) suffix) === Just (v, suffix)
    , prop "EAV/AVE key round-trip" 2000 $ \e aid v ->
        let e' = abs e in
        decodeEavKey (eavKey e' aid v) (entryPayload v) === Just (e', aid, v)
        .&&. decodeAveKey (aveKey aid v e') (entryPayload v) === Just (aid, v, e')
    , prop "large values: index keys stay short, payload carries the value" 300 $
        forAll genBigValue $ \v (abs -> e) aid ->
          B.length (eavKey e aid v) <= 511 .&&. B.length (aveKey aid v e) <= 511
          .&&. isBigValue v === True
          .&&. decodeEavKey (eavKey e aid v) (entryPayload v) === Just (e, aid, v)
          .&&. decodeAveKey (aveKey aid v e) (entryPayload v) === Just (aid, v, e)
          .&&. (decodeEavKey (eavKey e aid v) B.empty === Nothing)
    , prop "small values have no payload; equal values share a repr, different ones do not" 1000 $ \a b ->
        (not (isBigValue a) ==> entryPayload a === B.empty)
        .&&. ((indexRepr a == indexRepr b) === (a == b))
    , prop "entity ids sort numerically" 2000 $ \a b ->
        compare (encodeEid a) (encodeEid b) === compare a b
    ]

-- ---------------------------------------------------------------------------
-- KV / order statistics

genKey :: Gen B.ByteString
genKey = B.pack <$> (choose (1, 4) >>= \n -> vectorOf n (elements [0, 1, 2, 254, 255]))

kvTests :: IO [Bool]
kvTests = do
  section "KV and counted B+tree (dlmdb order statistics)"
  sequence
    [ prop "range counts, ranks and get_rank agree with a sorted-list model (counted DBI)" 150 $
        forAll (choose (1, 300) >>= \n -> vectorOf n genKey) $ \ks0 ->
        forAll genKey $ \lo -> forAll genKey $ \hi ->
        monadicIO $ do
          let ks = S.toAscList (S.fromList ks0)
          ok <- run $ withTempDir $ \d -> KV.withEnv d testEnvCfg $ \env -> do
            dbi <- KV.withWriteTxn env $ \t -> do
              x <- KV.openDbi t "t" KV.dbiCounted
              forM_ ks0 $ \k -> KV.put t x k B.empty
              pure x
            KV.withReadTxn env $ \t -> do
              -- first call in a fresh transaction (regression: used to return 0)
              c1 <- KV.countRange t dbi (Just lo) (Just hi)
              let m1 = fromIntegral (length [ k | k <- ks, k >= lo, k < hi ])
              c2 <- KV.countRange t dbi Nothing (Just hi)
              c3 <- KV.countRange t dbi (Just lo) Nothing
              c4 <- KV.countPrefix t dbi (B.take 1 lo)
              total <- KV.countAll t dbi
              ranks <- forM (zip [0 ..] ks) $ \(i, k) -> do
                r1 <- KV.keyRank t dbi k
                r2 <- KV.getRank t dbi i
                pure (r1 == Just i && fmap fst r2 == Just k)
              pure (and [ c1 == m1
                        , c2 == fromIntegral (length (filter (< hi) ks))
                        , c3 == fromIntegral (length (filter (>= lo) ks))
                        , c4 == fromIntegral (length (filter (B.isPrefixOf (B.take 1 lo)) ks))
                        , total == fromIntegral (length ks)
                        , and ranks ])
          assert ok
    , unit "50,000 keys across many pages: counts and ranks are exact" $
        withTempDir $ \d -> KV.withEnv d testEnvCfg $ \env -> do
          let key i = B.pack (map (fromIntegral . fromEnum) ("k" ++ pad 6 (show (i :: Int))))
              pad n s = replicate (n - length s) '0' ++ s
          dbi <- KV.withWriteTxn env $ \t -> do
            x <- KV.openDbi t "t" KV.dbiCounted
            forM_ [0 .. 49999] $ \i -> KV.put t x (key i) B.empty
            pure x
          KV.withReadTxn env $ \t -> do
            c <- KV.countPrefix t dbi (B.pack (map (fromIntegral . fromEnum) ("k01" :: String)))
            assertEq "prefix k01 count" 10000 c
            c' <- KV.countRange t dbi (Just (key 12345)) (Just (key 45678))
            assertEq "range count" (45678 - 12345) c'
            r <- KV.getRank t dbi 31337
            assertEq "getRank" (Just (key 31337)) (fmap fst r)
            k <- KV.keyRank t dbi (key 49999)
            assertEq "keyRank last" (Just 49999) k
    , unit "oversized key is rejected with an LmdbError" $ do
        r <- try $ withTempDir $ \d -> KV.withEnv d testEnvCfg $ \env ->
               KV.withWriteTxn env $ \t -> do
                 x <- KV.openDbi t "t" KV.dbiPlain
                 KV.put t x (B.replicate 600 7) B.empty
        case r of
          Left e  -> assertThat ("unexpected code " ++ show e) (KV.lmdbCode e == -30781)
          Right _ -> throwIO (userError "no error for a 600-byte key")
    , unit "write transaction aborts on exception (all-or-nothing)" $
        withTempDir $ \d -> KV.withEnv d testEnvCfg $ \env -> do
          dbi <- KV.withWriteTxn env $ \t -> KV.openDbi t "t" KV.dbiPlain
          _ <- try (KV.withWriteTxn env $ \t -> do
                      KV.put t dbi "k" "v"
                      throwIO (userError "boom")) :: IO (Either IOException ())
          v <- KV.withReadTxn env $ \t -> KV.get t dbi "k"
          assertEq "value after abort" Nothing v
    , prefixCompressionCanary
    , unit "prefix compression is safe for Takka-shaped keys (shared leading bytes), 40 random trials" $
        forM_ [1 .. 40 :: Int] $ \_ -> do
          ks <- generate (choose (20, 400) >>= \n -> vectorOf n
                  (B.append (B.pack [0, 0, 0, 2]) . B.pack <$> (choose (1, 6) >>= \m -> vectorOf m (elements [0, 1, 2, 254, 255]))))
          dels <- generate (sublistOf (S.toList (S.fromList ks)))
          withTempDir $ \d -> KV.withEnv d testEnvCfg $ \env -> do
            dbi <- KV.withWriteTxn env $ \t -> do
              x <- KV.openDbi t "t" (KV.dbiCounted <> KV.dbiPrefixCompression)
              forM_ ks $ \k -> KV.put t x k B.empty
              pure x
            KV.withWriteTxn env $ \t -> forM_ dels $ \k -> KV.del t dbi k
            let model = S.toAscList (S.fromList ks `S.difference` S.fromList dels)
            got <- KV.withReadTxn env $ \t -> map fst <$> KV.scanPrefix t dbi B.empty
            assertEq "scan equals model" model got
    ]

-- | Not a pass/fail check of Takka: it watches an upstream dlmdb bug.  While the
-- bug exists it reports "known"; the day it stops reproducing it tells you that
-- prefix compression may be worth enabling by default.
prefixCompressionCanary :: IO Bool
prefixCompressionCanary = do
  corrupted <- withTempDir $ \d -> KV.withEnv d testEnvCfg $ \env -> do
    dbi <- KV.withWriteTxn env $ \t -> do
      x <- KV.openDbi t "t" KV.dbiPrefixCompression
      forM_ [[2], [0, 2], [0, 1, 1]] $ \k -> KV.put t x (B.pack k) B.empty
      pure x
    got <- KV.withReadTxn env $ \t -> map (B.unpack . fst) <$> KV.scanPrefix t dbi B.empty
    pure (got /= [[0, 1, 1], [0, 2], [2]])
  putStrLn $ if corrupted
    then "  known dlmdb prefix-compression bug still reproduces (docs/dlmdb-findings.md); it stays opt-in"
    else "  NOTE  the dlmdb prefix-compression bug no longer reproduces - consider enabling it by default"
  pure True

-- ---------------------------------------------------------------------------
-- Store

personSchema :: Schema
personSchema = M.fromList
  [ ("person/name",   identity (attr TString))
  , ("person/alias",  many (attr TString))
  , ("person/age",    attr TLong)
  , ("person/friend", many ref)
  , ("person/boss",   ref)
  ]

rowsOf :: Conn -> Text -> IO [[Value]]
rowsOf c t = q c t []

storeTests :: IO [Bool]
storeTests = do
  section "Store: transactions, schema, constraints"
  sequence
    [ unit "temp ids resolve, datoms are queryable" $ withTestConn personSchema $ \c -> do
        rep <- transactEdn c "[{:db/id -1 :person/name \"A\" :person/age 1 :person/friend [-2]} {:db/id -2 :person/name \"B\" :person/age 2}]"
        assertEq "tempids" 2 (M.size (txTempids rep))
        r <- rowsOf c "[:find ?a ?b :where [?x :person/name ?a] [?x :person/friend ?y] [?y :person/name ?b]]"
        assertEq "friend join" [[VStr "A", VStr "B"]] r
    , unit "upsert on a unique identity attribute merges instead of duplicating" $ withTestConn personSchema $ \c -> do
        _ <- transactEdn c "[{:person/name \"A\" :person/age 1}]"
        _ <- transactEdn c "[{:person/name \"A\" :person/age 2 :person/alias [\"x\"]}]"
        r <- rowsOf c "[:find ?e ?a :where [?e :person/name \"A\"] [?e :person/age ?a]]"
        assertEq "one entity, new age" 1 (length r)
        assertEq "age updated" (VInt 2) (r !! 0 !! 1)
        n <- withDb c $ \db -> length <$> datoms db Nothing (Just "person/name") Nothing
        assertEq "single name datom" 1 n
    , unit "cardinality one replaces; report shows the retraction" $ withTestConn personSchema $ \c -> do
        rep0 <- transactEdn c "[{:db/id -1 :person/name \"A\" :person/age 5}]"
        let e = txTempids rep0 M.! (-1)
        rep <- transact c [Add (EId e) "person/age" (TVal (VInt 6))]
        assertEq "tx-data" [ (False, Datom e "person/age" (VInt 5)), (True, Datom e "person/age" (VInt 6)) ] (txData rep)
    , unit "cardinality many accumulates; duplicates are ignored" $ withTestConn personSchema $ \c -> do
        _ <- transactEdn c "[{:person/name \"A\" :person/alias [\"x\" \"y\"]}]"
        _ <- transactEdn c "[{:person/name \"A\" :person/alias [\"y\" \"z\"]}]"
        r <- rowsOf c "[:find ?al :where [?e :person/name \"A\"] [?e :person/alias ?al]]"
        assertEq "aliases" [[VStr "x"], [VStr "y"], [VStr "z"]] r
    , unit "unique violation is rejected" $ withTestConn personSchema $ \c -> do
        rep <- transactEdn c "[{:db/id -1 :person/name \"A\"} {:db/id -2 :person/name \"B\"}]"
        let b = txTempids rep M.! (-2)
        expectError "dup name" (transact c [Add (EId b) "person/name" (TVal (VStr "A"))])
          (\e -> case e of TxError m -> "unique" `isIn` m; _ -> False)
    , unit "type mismatch is rejected and nothing is written" $ withTestConn personSchema $ \c -> do
        expectError "bad type"
          (transactEdn c "[{:person/name \"Ghost\" :person/age 1} {:person/name \"B\" :person/age \"x\"}]")
          (\e -> case e of TxError m -> "expected long" `isIn` m; _ -> False)
        r <- rowsOf c "[:find ?n :where [?e :person/name ?n]]"
        assertEq "atomic" [] r
    , unit "retract and retractEntity (including inbound refs)" $ withTestConn personSchema $ \c -> do
        _ <- transactEdn c "[{:db/id -1 :person/name \"A\" :person/friend [-2]} {:db/id -2 :person/name \"B\" :person/age 3}]"
        _ <- transactEdn c "[[:db/retract [:person/name \"B\"] :person/age 3]]"
        r1 <- rowsOf c "[:find ?a :where [?e :person/name \"B\"] [?e :person/age ?a]]"
        assertEq "age retracted" [] r1
        _ <- transactEdn c "[[:db/retractEntity [:person/name \"B\"]]]"
        r2 <- rowsOf c "[:find ?n :where [?e :person/name ?n]]"
        assertEq "B is gone" [[VStr "A"]] r2
        r3 <- rowsOf c "[:find ?f :where [?e :person/name \"A\"] [?e :person/friend ?f]]"
        assertEq "A no longer has a dangling friend" [] r3
    , unit "lookup refs and nested maps" $ withTestConn personSchema $ \c -> do
        _ <- transactEdn c "[{:person/name \"A\" :person/boss {:person/name \"Boss\" :person/age 50}}]"
        r <- rowsOf c "[:find ?b ?a :where [?e :person/name \"A\"] [?e :person/boss ?x] [?x :person/name ?b] [?x :person/age ?a]]"
        assertEq "nested" [[VStr "Boss", VInt 50]] r
        _ <- transactEdn c "[[:db/add [:person/name \"A\"] :person/age 9]]"
        r2 <- rowsOf c "[:find ?a :where [?e :person/name \"A\"] [?e :person/age ?a]]"
        assertEq "lookup ref add" [[VInt 9]] r2
    , unit "schema conflicts are refused, widening is allowed" $ withTestConn personSchema $ \c -> do
        expectError "type change" (updateSchema c (M.fromList [("person/age", attr TString)]))
          (\e -> case e of SchemaError _ -> True; _ -> False)
        updateSchema c (M.fromList [("person/boss", many ref)])
        s <- getSchema c
        assertEq "widened" (Just Many) (asCard <$> M.lookup "person/boss" s)
    , unit "data and schema survive close/reopen" $ withTempDir $ \d -> do
        bracket (openConnWith testCfg d personSchema) closeConn $ \c ->
          void (transactEdn c "[{:person/name \"A\" :person/age 7 :person/alias [\"a\"]}]")
        bracket (openConnWith testCfg d M.empty) closeConn $ \c -> do
          r <- rowsOf c "[:find ?n ?a :where [?e :person/name ?n] [?e :person/age ?a]]"
          assertEq "persisted" [[VStr "A", VInt 7]] r
          s <- getSchema c
          assertEq "schema persisted" (Just UniqueIdentity) (asUnique <$> M.lookup "person/name" s)
    , unit "a Db is a stable snapshot while another thread commits" $ withTestConn personSchema $ \c -> do
        _ <- transactEdn c "[{:person/name \"A\"}]"
        withDb c $ \db -> do
          before <- datomCount db
          done <- newEmptyMVar
          _ <- forkIO (transactEdn c "[{:person/name \"B\"} {:person/name \"C\"}]" >> putMVar done ())
          takeMVar done
          after <- datomCount db
          assertEq "snapshot unchanged" before after
        n <- withDb c datomCount
        assertEq "new snapshot sees the commit" 3 n
    , unit "concurrent writers are serialised without losing updates" $ withTestConn personSchema $ \c -> do
        dones <- forM [1 .. 8 :: Int] $ \i -> do
          mv <- newEmptyMVar
          _ <- forkIO $ do
            forM_ [1 .. 10 :: Int] $ \j ->
              transactEdn c (T.pack ("[{:person/name \"w" ++ show i ++ "-" ++ show j ++ "\"}]"))
            putMVar mv ()
          pure mv
        mapM_ takeMVar dones
        r <- rowsOf c "[:find (count ?e) :where [?e :person/name _]]"
        assertEq "80 entities" [[VInt 80]] r
    ]
  where isIn needle hay = needle `T.isInfixOf` T.pack hay

-- ---------------------------------------------------------------------------
-- Large values

bigValueTests :: IO [Bool]
bigValueTests = do
  section "Large values (stored out of line)"
  let docSchema = M.fromList
        [ ("doc/title", identity (attr TString)), ("doc/body", attr TString)
        , ("doc/key", identity (attr TString)), ("doc/n", attr TLong)
        , ("doc/tags", many (attr TString)) ]
      big n c = T.replicate n (T.singleton c)
  sequence
    [ unit "10 KB and 1 MB strings round-trip and can be looked up by value" $ withTestConn docSchema $ \c -> do
        let b1 = big 10000 'a'; b2 = big 1000000 'b'
        _ <- transact c [ EntityMap ENew [("doc/title", TVal (VStr "one")), ("doc/body", TVal (VStr b1))]
                        , EntityMap ENew [("doc/title", TVal (VStr "two")), ("doc/body", TVal (VStr b2))] ]
        r <- rowsOf c "[:find ?t ?b :where [?e :doc/title ?t] [?e :doc/body ?b]]"
        assertEq "values" [[VStr "one", VStr b1], [VStr "two", VStr b2]] r
        r2 <- q c "[:find ?t :in $ ?b :where [?e :doc/body ?b] [?e :doc/title ?t]]" [InScalar (VStr b2)]
        assertEq "lookup by value" [[VStr "two"]] r2
        r3 <- q c "[:find ?t :in $ ?b :where [?e :doc/body ?b] [?e :doc/title ?t]]" [InScalar (VStr (b2 <> "!"))]
        assertEq "near miss finds nothing" [] r3
    , unit "big values sharing a long prefix stay distinct (many-valued attribute)" $ withTestConn docSchema $ \c -> do
        let p = big 5000 'z'
        _ <- transact c [EntityMap ENew [("doc/title", TVal (VStr "t")), ("doc/tags", TMany [TVal (VStr (p <> "1")), TVal (VStr (p <> "2")), TVal (VStr "small")])]]
        r <- rowsOf c "[:find ?g :where [?e :doc/tags ?g]]"
        assertEq "three tags" 3 (length r)
        r2 <- q c "[:find ?t :in $ ?g :where [?e :doc/tags ?g] [?e :doc/title ?t]]" [InScalar (VStr (p <> "2"))]
        assertEq "exact one" [[VStr "t"]] r2
    , unit "a big value as unique identity: upsert merges, a duplicate is rejected" $ withTestConn docSchema $ \c -> do
        let k = big 3000 'k'
        _ <- transact c [EntityMap ENew [("doc/key", TVal (VStr k)), ("doc/n", TVal (VInt 1))]]
        _ <- transact c [EntityMap ENew [("doc/key", TVal (VStr k)), ("doc/n", TVal (VInt 2))]]
        r <- rowsOf c "[:find ?e ?n :where [?e :doc/n ?n]]"
        assertEq "one entity, updated" 1 (length r)
        assertEq "n" (VInt 2) (r !! 0 !! 1)
        rep <- transact c [EntityMap ENew [("doc/key", TVal (VStr "other")), ("doc/n", TVal (VInt 9))]]
        let e2 = head [ dE d | (True, d) <- txData rep, dA d == "doc/n" ]
        expectError "duplicate big key" (transact c [Add (EId e2) "doc/key" (TVal (VStr k))])
          (\e -> case e of TxError m -> "unique" `T.isInfixOf` T.pack m; _ -> False)
    , unit "replace and retract big values; entity and counts stay exact" $ withTestConn docSchema $ \c -> do
        let b1 = big 2000 'p'; b2 = big 2500 'q'
        rep <- transact c [EntityMap (ETemp (-1)) [("doc/title", TVal (VStr "d")), ("doc/body", TVal (VStr b1))]]
        let e = txTempids rep M.! (-1)
        rep2 <- transact c [Add (EId e) "doc/body" (TVal (VStr b2))]
        assertEq "replace reports the retraction" [(False, Datom e "doc/body" (VStr b1)), (True, Datom e "doc/body" (VStr b2))] (txData rep2)
        ent <- withDb c $ \db -> entity db e
        assertEq "entity" (Just [VStr b2]) (M.lookup "doc/body" ent)
        _ <- transact c [Retract (EId e) "doc/body" (VStr b1)]          -- not present: no-op
        ent2 <- withDb c $ \db -> entity db e
        assertEq "still there" (Just [VStr b2]) (M.lookup "doc/body" ent2)
        _ <- transact c [Retract (EId e) "doc/body" (VStr b2)]
        ent3 <- withDb c $ \db -> entity db e
        assertEq "gone" Nothing (M.lookup "doc/body" ent3)
    , unit "200 big inserts then deletes: counts, ranks and scans agree" $ withTestConn docSchema $ \c -> do
        let body i = T.pack (show i) <> big (600 + i * 7) (toEnum (97 + i `mod` 26))
        rep <- transact c [ EntityMap (ETemp (negate (fromIntegral i))) [("doc/title", TVal (VStr (T.pack ("t" ++ show i)))), ("doc/body", TVal (VStr (body i)))]
                          | i <- [1 .. 200 :: Int] ]
        let ids = [ (i, txTempids rep M.! negate (fromIntegral i)) | i <- [1 .. 200 :: Int] ]
        _ <- transact c [ RetractEntity (EId e) | (i, e) <- ids, i `mod` 3 == 0 ]
        withDb c $ \db -> do
          ds <- allDatoms db
          n  <- datomCount db
          assertEq "datom count" (fromIntegral (length ds)) n
          nb <- countDatoms db Nothing (Just "doc/body") Nothing
          bodies <- datoms db Nothing (Just "doc/body") Nothing
          assertEq "body count" (fromIntegral (length bodies)) nb
          assertEq "134 survivors (200 minus the 66 multiples of 3)" 134 (length bodies)
          assertEq "all intact" True (and [ dV d == VStr (body i) | d <- bodies, (i, e) <- ids, e == dE d ])
    , unit "JSON-LD with a large literal" $ withTestConn M.empty $ \c -> do
        let txt = big 20000 'j'
        _ <- importJsonLD c (A.object [ "@id" A..= ("http://ex.org/a" :: Text), "http://ex.org/text" A..= txt ])
        r <- rowsOf c "[:find ?t :where [?e :jsonld/id \"http://ex.org/a\"] [?e :http://ex.org/text ?t]]"
        assertEq "round trip" [[VStr txt]] r
    ]

-- ---------------------------------------------------------------------------
-- Query engine vs. brute force

data Model = Model
  { mN     :: Int
  , mAge   :: M.Map Int Int
  , mTags  :: M.Map Int (S.Set Text)
  , mLinks :: S.Set (Int, Int)
  } deriving Show

genModel :: Gen Model
genModel = do
  n    <- choose (1, 7)
  ages <- forM [1 .. n] $ \i -> frequency [(3, (\a -> Just (i, a)) <$> choose (0, 5)), (1, pure Nothing)]
  tags <- forM [1 .. n] $ \i -> (\ts -> (i, S.fromList ts)) <$> sublistOf ["a", "b", "c"]
  k    <- choose (0, 14)
  ls   <- vectorOf k ((,) <$> choose (1, n) <*> choose (1, n))
  pure (Model n (M.fromList (catMaybes ages)) (M.fromList tags) (S.fromList ls))

modelSchema :: Schema
modelSchema = M.fromList
  [ ("p/age",  attr TLong)
  , ("p/tag",  many (attr TString))
  , ("p/link", many ref) ]

modelOps :: Model -> [TxOp]
modelOps m =
  [ EntityMap (ETemp (negate (fromIntegral i)))
      (  [ ("p/age", TVal (VInt (fromIntegral a))) | Just a <- [M.lookup i (mAge m)] ]
      ++ [ ("p/tag", TMany [ TVal (VStr t) | t <- S.toList ts ]) | Just ts <- [M.lookup i (mTags m)], not (S.null ts) ]
      ++ [ ("p/link", TMany [ TVal (VInt (negate (fromIntegral j))) | (i', j) <- S.toList (mLinks m), i' == i ])
         | any ((== i) . fst) (S.toList (mLinks m)) ] )
  | i <- [1 .. mN m] ]

withModel :: Model -> (Conn -> (Int -> Value) -> IO a) -> IO a
withModel m act = withTestConn modelSchema $ \c -> do
  rep <- transact c (modelOps m)
  let eid i = VInt (txTempids rep M.! negate (fromIntegral i))
  act c eid

sortedSet :: Ord a => [a] -> [a]
sortedSet = S.toAscList . S.fromList

queryTests :: IO [Bool]
queryTests = do
  section "Query engine against a brute-force oracle"
  sequence
    [ prop "filter, join, aggregate, negation, multi-pattern" 120 $
        forAll genModel $ \m -> monadicIO $ do
          bad <- run $ withModel m $ \c eid -> do
            let age i = M.lookup i (mAge m)
                tags i = M.findWithDefault S.empty i (mTags m)
                ids = [1 .. mN m]
            q1 <- rowsOf c "[:find ?e ?a :where [?e :p/age ?a] [(> ?a 2)]]"
            let o1 = sortedSet [ [eid i, VInt (fromIntegral a)] | i <- ids, Just a <- [age i], a > 2 ]
            q2 <- rowsOf c "[:find ?e ?f :where [?e :p/link ?f] [?f :p/age ?a] [(>= ?a 3)]]"
            let o2 = sortedSet [ [eid i, eid j] | (i, j) <- S.toList (mLinks m), Just a <- [age j], a >= 3 ]
            q3 <- rowsOf c "[:find ?a (count ?e) :where [?e :p/age ?a]]"
            let o3 = [ [VInt (fromIntegral a), VInt n] | (a, n) <- M.toAscList (M.fromListWith (+) [ (a, 1) | i <- ids, Just a <- [age i] ]) ]
            q4 <- rowsOf c "[:find ?e :where [?e :p/tag \"a\"] [?e :p/tag \"b\"]]"
            let o4 = sortedSet [ [eid i] | i <- ids, "a" `S.member` tags i, "b" `S.member` tags i ]
            q5 <- rowsOf c "[:find ?e :where [?e :p/age _] (not [?e :p/tag \"a\"])]"
            let o5 = sortedSet [ [eid i] | i <- ids, isJust (age i), not ("a" `S.member` tags i) ]
            q6 <- rowsOf c "[:find (sum ?a) (min ?a) (max ?a) :with ?e :where [?e :p/age ?a]]"
            let as = catMaybes [ age i | i <- ids ]
                o6 = [ [VInt (fromIntegral (sum as)), VInt (fromIntegral (minimum as)), VInt (fromIntegral (maximum as))] | not (null as) ]
            q7 <- rowsOf c "[:find ?t (count-distinct ?a) :where [?e :p/tag ?t] [?e :p/age ?a]]"
            let o7 = [ [VStr t, VInt (fromIntegral (S.size (S.fromList [ a | i <- ids, t `S.member` tags i, Just a <- [age i] ])))]
                     | t <- S.toAscList (S.unions (M.elems (mTags m)))
                     , any (\i -> t `S.member` tags i && isJust (age i)) ids ]
            pure [ (n, ex, got) | (n, ex, got) <-
                     [ ("q1", o1, q1), ("q2", o2, q2), ("q3", o3, q3), ("q4", o4, q4)
                     , ("q5", o5, q5), ("q6", o6, q6), ("q7", o7, q7) ], ex /= got ]
          unless (null bad) $
            stop (counterexample ("mismatches: " ++ show [ (n :: String, ex, got) | (n, ex, got) <- bad ]) False)
    , prop "clause order does not change the result (all permutations)" 40 $
        forAll genModel $ \m -> monadicIO $ do
          ok <- run $ withModel m $ \c _ -> do
            let cls = [ "[?e :p/link ?f]", "[?f :p/age ?a]", "[(>= ?a 2)]", "(not [?f :p/tag \"c\"])" ] :: [Text]
            rs <- forM (permutations cls) $ \p ->
                    rowsOf c ("[:find ?e ?f :where " <> T.unwords p <> "]")
            pure (length (nub rs) == 1)
          assert ok
    , unit "inputs: scalar, collection, relation, function binding, :with" $ withTestConn personSchema $ \c -> do
        _ <- transactEdn c "[{:person/name \"A\" :person/age 10} {:person/name \"B\" :person/age 20} {:person/name \"C\" :person/age 20}]"
        r1 <- q c "[:find ?n :in $ [?a ...] :where [?e :person/age ?a] [?e :person/name ?n]]" [InColl [VInt 10, VInt 99]]
        assertEq "collection input" [[VStr "A"]] r1
        r2 <- q c "[:find ?n ?x :in $ [[?n ?x]] :where [?e :person/name ?n]]" [InRel [[VStr "B", VInt 1], [VStr "Z", VInt 2]]]
        assertEq "relation input" [[VStr "B", VInt 1]] r2
        r3 <- q c "[:find ?n ?n2 :where [?e :person/name ?n] [?e :person/age ?a] [(+ ?a 1) ?b] [(str ?n \"!\") ?n2] [(= ?b 21)]]" []
        assertEq "function bindings" [[VStr "B", VStr "B!"], [VStr "C", VStr "C!"]] r3
        r4 <- q c "[:find (count ?a) :with ?e :where [?e :person/age ?a]]" []
        assertEq ":with keeps duplicates" [[VInt 3]] r4
        r5 <- q c "[:find (count ?a) :where [?e :person/age ?a]]" []
        assertEq "without :with, values are a set" [[VInt 2]] r5
    , unit "typed lookup: an int constant matches a double attribute" $
        withTestConn (M.fromList [("m/score", attr TDouble)]) $ \c -> do
          _ <- transactEdn c "[{:m/score 3}]"
          r <- q c "[:find ?e :where [?e :m/score 3]]" []
          assertEq "coerced" 1 (length r)
    , unit "errors: unbound variable, unknown rule, unknown aggregate" $ withTestConn personSchema $ \c -> do
        expectError "unbound" (q c "[:find ?z :where [?e :person/name ?n]]" [])
          (\e -> case e of QueryError _ -> True; _ -> False)
        expectError "unknown rule" (q c "[:find ?e :where (nope ?e)]" [])
          (\e -> case e of QueryError _ -> True; _ -> False)
        expectError "bad aggregate" (q c "[:find (median ?e) :where [?e :person/name _]]" [])
          (\e -> case e of ParseError _ -> True; _ -> False)
    ]

-- ---------------------------------------------------------------------------
-- Range predicates

-- | The comparison semantics the query engine promises (numbers compare across int/double;
-- other types compare only with their own kind).
cmpV :: Value -> Value -> Maybe Ordering
cmpV a b = case (a, b) of
  (VInt x, VInt y)       -> Just (compare x y)
  (VDouble x, VDouble y) -> Just (compare x y)
  (VInt x, VDouble y)    -> Just (compare (fromIntegral x) y)
  (VDouble x, VInt y)    -> Just (compare x (fromIntegral y))
  (VStr x, VStr y)       -> Just (compare x y)
  (VKw x, VKw y)         -> Just (compare x y)
  (VBool x, VBool y)     -> Just (compare x y)
  _                      -> Nothing

holds :: Text -> Value -> Value -> Bool
holds op a b = case op of
  "<"  -> maybe False (== LT) (cmpV a b)
  "<=" -> maybe False (/= GT) (cmpV a b)
  ">"  -> maybe False (== GT) (cmpV a b)
  ">=" -> maybe False (/= LT) (cmpV a b)
  _    -> maybe (a == b) (== EQ) (cmpV a b)

rangeSchema :: Schema
rangeSchema = M.fromList
  [ ("r/any", attr TAny), ("r/long", attr TLong), ("r/dbl", attr TDouble) ]

genAnyValue :: Gen Value
genAnyValue = oneof
  [ VInt <$> choose (-5, 5)
  , VDouble <$> elements [-2.5, -0.5, 0, 0.5, 1.5, 3.5, 4.0]
  , elements [ VStr "", VStr "a", VStr "ab", VStr "b", VStr (T.replicate 500 "b")
             , VStr (T.replicate 600 "a"), VStr (T.replicate 450 "b" <> "z") ]
  , elements [ VKw "a", VKw "b", VKw "ab" ]
  , VBool <$> arbitrary ]

rangeTests :: IO [Bool]
rangeTests = do
  section "Range predicates (AVE range scans)"
  sequence
    [ prop "random comparisons on mixed-type attributes match the oracle" 150 $
        forAll (choose (1, 14)) $ \n ->
        forAll (vectorOf n ((,,) <$> frequency [(4, Just <$> genAnyValue), (1, pure Nothing)]
                                 <*> frequency [(3, Just <$> choose (0, 9)), (1, pure Nothing)]
                                 <*> frequency [(3, Just <$> elements [0, 0.5, 1.5, 2, 3.5, 9]), (1, pure Nothing)])) $ \rows ->
        forAll (choose (1, 2)) $ \nc ->
        forAll (vectorOf nc ((,,) <$> elements ["<", "<=", ">", ">=", "="] <*> genAnyValue <*> arbitrary)) $ \cons ->
        forAll (elements ["r/any", "r/long", "r/dbl"]) $ \attrName ->
        monadicIO $ do
          bad <- run $ withTestConn rangeSchema $ \c -> do
            let ops = [ EntityMap (ETemp (negate (fromIntegral i)))
                          ( [ ("r/any", TVal v) | Just v <- [a] ]
                         ++ [ ("r/long", TVal (VInt l)) | Just l <- [b] ]
                         ++ [ ("r/dbl", TVal (VDouble d)) | Just d <- [dd] ] )
                      | (i, (a, b, dd)) <- zip [1 :: Int ..] rows ]
            rep <- transact c ops
            let eid i = VInt (txTempids rep M.! negate (fromIntegral i))
                valueOf (a, b, dd) = case attrName :: Text of
                  "r/any"  -> a
                  "r/long" -> VInt <$> b
                  _        -> VDouble <$> dd
                names = [ "?c" <> T.pack (show k) | k <- [1 .. nc] ]
                preds = [ if left then "[(" <> op <> " ?v " <> nm <> ")]" else "[(" <> op <> " " <> nm <> " ?v)]"
                        | ((op, _, left), nm) <- zip cons names ]
                query = "[:find ?e ?v :in $ " <> T.unwords names <> " :where [?e :" <> attrName <> " ?v] "
                        <> T.unwords preds <> "]"
                expected = sortedSet
                  [ [eid i, v]
                  | (i, row) <- zip [1 :: Int ..] rows, Just v <- [valueOf row]
                  , and [ if left then holds op v k else holds op k v | (op, k, left) <- cons ] ]
            got <- q c query [ InScalar k | (_, k, _) <- cons ]
            pure (if got == expected then Nothing else Just (query, [ k | (_, k, _) <- cons ], expected, got))
          case bad of
            Nothing -> pure ()
            Just info -> stop (counterexample (show info) False)
    , unit "boundaries are exact: >, >=, <, <=, chains, = and input bounds" $
        withTestConn (M.fromList [("n/a", attr TLong)]) $ \c -> do
          _ <- transact c [ EntityMap ENew [("n/a", TVal (VInt i))] | i <- [0 .. 9] ]
          let ints t = fmap (map (\r -> case r of [VInt x] -> x; _ -> -1)) (q c t [])
          ints "[:find ?a :where [?e :n/a ?a] [(> ?a 3)]]"      >>= assertEq ">"  [4 .. 9]
          ints "[:find ?a :where [?e :n/a ?a] [(>= ?a 3)]]"     >>= assertEq ">=" [3 .. 9]
          ints "[:find ?a :where [?e :n/a ?a] [(< ?a 3)]]"      >>= assertEq "<"  [0 .. 2]
          ints "[:find ?a :where [?e :n/a ?a] [(<= ?a 3)]]"     >>= assertEq "<=" [0 .. 3]
          ints "[:find ?a :where [?e :n/a ?a] [(< 2 ?a 5)]]"    >>= assertEq "chain" [3, 4]
          ints "[:find ?a :where [?e :n/a ?a] [(<= 2 ?a 5)]]"   >>= assertEq "chain inclusive" [2 .. 5]
          ints "[:find ?a :where [?e :n/a ?a] [(= ?a 5)]]"      >>= assertEq "=" [5]
          ints "[:find ?a :where [?e :n/a ?a] [(> 4 ?a)]]"      >>= assertEq "const first" [0 .. 3]
          ints "[:find ?a :where [?e :n/a ?a] [(> ?a 2.5)]]"    >>= assertEq "double bound on ints" [3 .. 9]
          ints "[:find ?a :where [?e :n/a ?a] [(< ?a 2.5)]]"    >>= assertEq "double upper bound" [0 .. 2]
          ints "[:find ?a :where [?e :n/a ?a] [(> ?a 100)]]"    >>= assertEq "beyond the data" []
          ints "[:find ?a :where [?e :n/a ?a] [(> ?a 4)] [(< ?a 3)]]" >>= assertEq "empty intersection" []
          r <- q c "[:find ?a :in $ ?lo ?hi :where [?e :n/a ?a] [(>= ?a ?lo)] [(< ?a ?hi)]]" [InScalar (VInt 2), InScalar (VInt 6)]
          assertEq "input bounds" [[VInt x] | x <- [2 .. 5]] r
          r2 <- q c "[:find ?a :in $ [?lo ...] :where [?e :n/a ?a] [(>= ?a ?lo)] [(< ?a 4)]]" [InColl [VInt 1, VInt 3]]
          assertEq "a collection of bounds (the union of both ranges)" [[VInt x] | x <- [1 .. 3]] r2
    , unit "the planner prefers the narrow range, and explain says so" $
        withTestConn personSchema $ \c -> do
          _ <- transact c [ EntityMap ENew [ ("person/name", TVal (VStr (T.pack ("p" ++ show i))))
                                           , ("person/age", TVal (VInt (fromIntegral (i `mod` 100)))) ]
                          | i <- [1 .. 5000 :: Int] ]
          let query = "[:find ?n :where [?e :person/name ?n] [?e :person/age ?a] [(> ?a 97)]]"
          (rows, plan) <- withDb c $ \db -> explainEdn db query []
          assertEq "100 people are older than 97" 100 (length rows)
          assertThat ("expected a range scan on age first, plan: " ++ show plan)
            (case plan of (l : _) -> "person/age" `T.isInfixOf` l && "range scan" `T.isInfixOf` l; _ -> False)
          withDb c $ \db -> do
            n <- countInRange db "person/age" (Just (Bound (VInt 97) False)) Nothing
            assertEq "range count is exact for an index on whole values" (Just 100) n
            n2 <- countInRange db "person/age" (Just (Bound (VInt 97) True)) (Just (Bound (VInt 98) True))
            assertEq "inclusive both ends" (Just 100) n2
    ]

-- ---------------------------------------------------------------------------
-- Rules

closure :: Int -> S.Set (Int, Int) -> S.Set (Int, Int)
closure _ es = go es
  where go cur = let nxt = S.union cur (S.fromList [ (a, d) | (a, b) <- S.toList cur, (c, d) <- S.toList cur, b == c ])
                 in if nxt == cur then cur else go nxt

ruleTests :: IO [Bool]
ruleTests = do
  section "Recursive rules (fixpoint) against a transitive-closure oracle"
  let rightRec = "[[(reach ?a ?b) [?a :p/link ?b]] [(reach ?a ?b) [?a :p/link ?c] (reach ?c ?b)]]"
      nonLinear = "[[(reach ?a ?b) [?a :p/link ?b]] [(reach ?a ?b) (reach ?a ?c) (reach ?c ?b)]]"
      -- the recursion hides inside an or-join branch, with a branch-local variable ?c
      orRec = "[[(reach ?a ?b) (or-join [?a ?b] [?a :p/link ?b] (and [?a :p/link ?c] (reach ?c ?b)))]]"
  sequence
    [ prop "right-recursive, non-linear and or-join definitions give the closure (cycles included)" 80 $
        forAll genModel $ \m -> monadicIO $ do
          bad <- run $ withModel m $ \c eid -> do
            let expected = sortedSet [ [eid a, eid b] | (a, b) <- S.toList (closure (mN m) (mLinks m)) ]
            fmap catMaybes . forM [("right", rightRec), ("nonlinear", nonLinear), ("or-join", orRec)] $ \(name, rt) -> do
              rules <- rulesEdn rt
              got <- q c "[:find ?x ?y :in $ % :where (reach ?x ?y)]" [InRules rules]
              pure (if got == expected then Nothing else Just (name :: String, expected, got))
          unless (null bad) $ stop (counterexample (show bad) False)
    , prop "rule call with a bound first argument" 60 $
        forAll genModel $ \m -> monadicIO $ do
          bad <- run $ withModel m $ \c eid -> do
            rules <- rulesEdn rightRec
            let cl = closure (mN m) (mLinks m)
            fmap catMaybes . forM [1 .. mN m] $ \i -> do
              got <- q c "[:find ?y :in $ % ?x :where (reach ?x ?y)]" [InRules rules, InScalar (eid i)]
              let expected = sortedSet [ [eid b] | (a, b) <- S.toList cl, a == i ]
              pure (if got == expected then Nothing else Just (i, expected, got))
          unless (null bad) $ stop (counterexample (show bad) False)
    , unit "negation over a rule is stratified: the negated rule is finished first" $
        withTestConn (M.fromList [("p/name", identity (attr TString)), ("p/bad", attr TBool)]) $ \c -> do
          _ <- transactEdn c "[{:p/name \"A\"} {:p/name \"B\" :p/bad true} {:p/name \"C\"}]"
          rules <- rulesEdn "[[(blocked ?x) [?x :p/bad true]] [(ok ?x) [?x :p/name _] (not (blocked ?x))]]"
          r <- q c "[:find ?n :in $ % :where (ok ?x) [?x :p/name ?n]]" [InRules rules]
          assertEq "B is blocked" [[VStr "A"], [VStr "C"]] r
    , unit "negation through recursion is rejected (not stratified)" $
        withTestConn (M.fromList [("g/to", many ref)]) $ \c -> do
          _ <- transactEdn c "[{:db/id -1 :g/to [-2]} {:db/id -2}]"
          rules <- rulesEdn "[[(win ?x) [?x :g/to ?y] (not (win ?y))]]"
          expectError "win/lose game" (q c "[:find ?x :in $ % :where (win ?x)]" [InRules rules])
            (\e -> case e of QueryError m -> "stratified" `T.isInfixOf` T.pack m; _ -> False)
    , unit "mutually recursive rules reach the joint fixpoint" $ withTestConn (M.fromList [("g/to", many ref)]) $ \c -> do
        -- 1 -> 2 -> 3 -> 4 -> 1 : even(x) reaches via odd and vice versa
        _ <- transactEdn c "[{:db/id -1 :g/to [-2]} {:db/id -2 :g/to [-3]} {:db/id -3 :g/to [-4]} {:db/id -4 :g/to [-1]}]"
        rules <- rulesEdn "[[(even ?a ?b) [?a :g/to ?m] (odd ?m ?b)] [(odd ?a ?b) [?a :g/to ?b]] [(odd ?a ?b) [?a :g/to ?m] (even ?m ?b)]]"
        r <- q c "[:find (count ?a) :with ?b :in $ % :where (even ?a ?b)]" [InRules rules]
        assertEq "every node reaches every node in an even number of steps (cycle of length 4)" [[VInt 8]] r
    , unit "rules mix with ordinary patterns and constants" $ withTestConn personSchema $ \c -> do
        _ <- transactEdn c "[{:db/id -1 :person/name \"A\" :person/friend [-2]} {:db/id -2 :person/name \"B\" :person/friend [-3]} {:db/id -3 :person/name \"C\"}]"
        rules <- rulesEdn "[[(fof ?a ?b) [?a :person/friend ?m] [?m :person/friend ?b]]]"
        r <- q c "[:find ?n :in $ % :where [?a :person/name \"A\"] (fof ?a ?b) [?b :person/name ?n]]" [InRules rules]
        assertEq "friend of friend" [[VStr "C"]] r
    ]

-- ---------------------------------------------------------------------------
-- or / or-join / not-join

colorSchema :: Schema
colorSchema = M.fromList
  [ ("c/name", identity (attr TString)), ("c/color", attr TString), ("c/age", attr TLong)
  , ("c/friend", many ref), ("c/vip", attr TBool), ("c/banned", attr TBool) ]

colorData :: Text
colorData = T.unlines
  [ "[{:db/id -1 :c/name \"A\" :c/color \"red\"   :c/age 30 :c/friend [-2 -3]}"
  , " {:db/id -2 :c/name \"B\" :c/color \"blue\"  :c/age 20 :c/vip true}"
  , " {:db/id -3 :c/name \"C\" :c/color \"green\" :c/age 40 :c/banned true}"
  , " {:db/id -4 :c/name \"D\" :c/color \"red\"   :c/age 50 :c/friend [-1]}"
  , " {:db/id -5 :c/name \"E\" :c/age 10}]" ]

orTests :: IO [Bool]
orTests = do
  section "or / or-join / not-join"
  let names t = withTestConn colorSchema $ \c -> do
        _ <- transactEdn c colorData
        rows <- q c t []
        pure [ n | [VStr n] <- rows ]
  sequence
    [ unit "or as a generator (binds ?e itself)" $
        names "[:find ?n :where (or [?e :c/color \"red\"] [?e :c/color \"blue\"]) [?e :c/name ?n]]"
          >>= assertEq "red or blue" ["A", "B", "D"]
    , unit "or-join as a filter on bound variables, with a predicate branch" $
        names "[:find ?n :where [?e :c/name ?n] [?e :c/age ?a] (or-join [?e ?a] [(> ?a 35)] [?e :c/vip true])]"
          >>= assertEq "older than 35, or vip" ["B", "C", "D"]
    , unit "an and-branch" $
        names "[:find ?n :where (or (and [?e :c/color \"red\"] [?e :c/age 30]) [?e :c/vip true]) [?e :c/name ?n]]"
          >>= assertEq "red and 30, or vip" ["A", "B"]
    , unit "or-join keeps branch-local variables local" $
        names "[:find ?n :where [?e :c/name ?n] (or-join [?e] (and [?e :c/friend ?f] [?f :c/banned true]) [?e :c/vip true])]"
          >>= assertEq "friend of a banned person, or vip" ["A", "B"]
    , unit "not-join: the local variable is existential" $
        names "[:find ?n :where [?e :c/name ?n] (not-join [?e] [?e :c/friend ?f] [?f :c/banned true])]"
          >>= assertEq "no banned friend" ["B", "C", "D", "E"]
    , unit "several bindings of a fresh variable are all kept, a repeated one counts once" $ withTestConn colorSchema $ \c -> do
        _ <- transactEdn c colorData
        r <- q c "[:find ?n ?x :where [?e :c/name ?n] (or-join [?e ?x] [?e :c/color ?x] [?e :c/vip ?x])]" []
        assertEq "B has both a color and vip=true"
          (sortedSet [ [VStr "A", VStr "red"], [VStr "B", VStr "blue"], [VStr "B", VBool True]
                     , [VStr "C", VStr "green"], [VStr "D", VStr "red"] ]) r
        r2 <- q c "[:find (count ?e) :with ?n :where [?e :c/name ?n] (or [?e :c/color \"red\"] [?e :c/age 30])]" []
        assertEq "A matches both branches but is one row" [[VInt 2]] r2
    , unit "branches that use different variables are rejected (use or-join)" $ withTestConn colorSchema $ \c ->
        expectError "or var mismatch" (q c "[:find ?e :where (or [?e :c/color \"red\"] [?x :c/color \"blue\"])]" [])
          (\e -> case e of ParseError m -> "same variables" `T.isInfixOf` T.pack m; _ -> False)
    , unit "an or-join variable that a branch leaves unbound is reported" $ withTestConn colorSchema $ \c -> do
        _ <- transactEdn c colorData
        expectError "unbound join var" (q c "[:find ?x :where [?e :c/name _] (or-join [?e ?x] [?e :c/color \"red\"])]" [])
          (\e -> case e of QueryError _ -> True; _ -> False)
    , unit "or and not-join inside rule bodies" $ withTestConn colorSchema $ \c -> do
        _ <- transactEdn c colorData
        rules <- rulesEdn $ T.unlines
          [ "[[(special ?e) (or [?e :c/vip true] [?e :c/banned true])]"
          , " [(plain ?e) [?e :c/name _] (not-join [?e] (special ?e))]]" ]
        r <- q c "[:find ?n :in $ % :where (plain ?e) [?e :c/name ?n]]" [InRules rules]
        assertEq "everyone who is neither vip nor banned" [[VStr "A"], [VStr "D"], [VStr "E"]] r
    , unit "negation through recursion via not-join is rejected" $ withTestConn colorSchema $ \c -> do
        rules <- rulesEdn "[[(win ?x) [?x :c/friend ?y] (not-join [?y] (win ?y))]]"
        expectError "not stratified" (q c "[:find ?x :in $ % :where (win ?x)]" [InRules rules])
          (\e -> case e of QueryError m -> "stratified" `T.isInfixOf` T.pack m; _ -> False)
    , prop "or, or-join and not-join match the oracle; clause order does not matter" 80 $
        forAll genModel $ \m -> monadicIO $ do
          bad <- run $ withModel m $ \c eid -> do
            let age i = M.lookup i (mAge m)
                tags i = M.findWithDefault S.empty i (mTags m)
                ids = [1 .. mN m]
                tag t i = t `S.member` tags i
            o1 <- rowsOf c "[:find ?e :where [?e :p/age _] (or [?e :p/tag \"a\"] [?e :p/tag \"b\"])]"
            let e1 = sortedSet [ [eid i] | i <- ids, isJust (age i), tag "a" i || tag "b" i ]
            o2 <- rowsOf c "[:find ?e :where (or [?e :p/tag \"a\"] [?e :p/tag \"c\"])]"
            let e2 = sortedSet [ [eid i] | i <- ids, tag "a" i || tag "c" i ]
            o3 <- rowsOf c "[:find ?e ?f :where [?e :p/link ?f] (or-join [?f] [?f :p/tag \"a\"] (and [?f :p/age ?x] [(> ?x 3)]))]"
            let e3 = sortedSet [ [eid i, eid j] | (i, j) <- S.toList (mLinks m), tag "a" j || maybe False (> 3) (age j) ]
            o4 <- rowsOf c "[:find ?e :where [?e :p/age _] (not-join [?e] [?e :p/link ?f] [?f :p/tag \"c\"])]"
            let e4 = sortedSet [ [eid i] | i <- ids, isJust (age i), not (or [ tag "c" j | (i', j) <- S.toList (mLinks m), i' == i ]) ]
            let cls = [ "[?e :p/link ?f]", "(or-join [?f] [?f :p/tag \"a\"] [?f :p/tag \"b\"])", "[?f :p/age ?a]", "[(>= ?a 2)]" ] :: [Text]
                e5 = sortedSet [ [eid i, eid j] | (i, j) <- S.toList (mLinks m), tag "a" j || tag "b" j, maybe False (>= 2) (age j) ]
            o5s <- forM (permutations cls) $ \p -> rowsOf c ("[:find ?e ?f :where " <> T.unwords p <> "]")
            pure ([ n | (n, ex, got) <- [("or", e1, o1), ("or generator", e2, o2), ("or-join", e3, o3), ("not-join", e4, o4)], ex /= got ]
                  ++ [ "order" | any (/= e5) o5s ] :: [String])
          unless (null bad) $ stop (counterexample ("mismatches: " ++ show bad) False)
    ]

-- ---------------------------------------------------------------------------
-- Pull

pullSchema :: Schema
pullSchema = M.fromList
  [ ("person/name", identity (attr TString)), ("person/alias", many (attr TString))
  , ("person/age", attr TLong), ("person/friend", many ref), ("person/boss", ref) ]

-- Frege -> Peirce -> De Morgan -> Frege is a cycle; Frege's boss is De Morgan.
pullData :: Text
pullData = T.unlines
  [ "[{:db/id -1 :person/name \"Frege\"     :person/age 70 :person/alias [\"Gottlob\" \"G.\"] :person/friend [-2] :person/boss -3}"
  , " {:db/id -2 :person/name \"Peirce\"    :person/age 75 :person/friend [-3]}"
  , " {:db/id -3 :person/name \"De Morgan\" :person/age 65 :person/friend [-1]}"
  , " {:db/id -4 :person/name \"Boole\"     :person/age 49}]" ]

withPulls :: ((Text -> Text -> IO Pulled) -> (Text -> EntityId) -> Db -> IO a) -> IO a
withPulls act = withTestConn pullSchema $ \c -> do
  _ <- transactEdn c pullData
  withDb c $ \db -> do
    ids <- fmap M.fromList . forM ["Frege", "Peirce", "De Morgan", "Boole"] $ \nm -> do
      r <- lookupRef db "person/name" (VStr nm)
      pure (nm, fromMaybe (error ("no such person: " ++ T.unpack nm)) r)
    let idOf nm = ids M.! nm
        pullBy pat nm = pullEdn db pat (idOf nm)
    act pullBy idOf db

pullTests :: IO [Bool]
pullTests = do
  section "Pull"
  let s' = PScalar . VStr
      i' = PScalar . VInt
      did n = PEntity [("db/id", i' n)]
  sequence
    [ unit "attributes come back in pattern order; missing ones are omitted" $
        withPulls $ \pl _ _ -> do
          r <- pl "[:person/age :person/name :person/boss]" "Boole"
          assertEq "Boole" (PEntity [("person/age", i' 49), ("person/name", s' "Boole")]) r
    , unit "wildcard: :db/id, then every attribute; refs as {:db/id n}, many-valued as lists" $
        withPulls $ \pl idOf _ -> do
          r <- pl "[*]" "Frege"
          let f = idOf "Frege"; p = idOf "Peirce"; d = idOf "De Morgan"
          assertEq "Frege"
            (PEntity [ ("db/id", i' f), ("person/age", i' 70)
                     , ("person/alias", PMany [s' "G.", s' "Gottlob"])
                     , ("person/boss", did d), ("person/friend", PMany [did p]), ("person/name", s' "Frege") ]) r
    , unit "nested patterns follow references (single and many)" $
        withPulls $ \pl _ _ -> do
          r <- pl "[:person/name {:person/friend [:person/name :person/age]} {:person/boss [:person/name]}]" "Frege"
          assertEq "Frege"
            (PEntity [ ("person/name", s' "Frege")
                     , ("person/friend", PMany [PEntity [("person/name", s' "Peirce"), ("person/age", i' 75)]])
                     , ("person/boss", PEntity [("person/name", s' "De Morgan")]) ]) r
    , unit "reverse references (:ns/_attr), with and without a nested pattern" $
        withPulls $ \pl idOf _ -> do
          r <- pl "[:person/name :person/_friend]" "De Morgan"
          assertEq "plain" (PEntity [("person/name", s' "De Morgan"), ("person/_friend", PMany [did (idOf "Peirce")])]) r
          r2 <- pl "[{:person/_friend [:person/name]}]" "De Morgan"
          assertEq "nested" (PEntity [("person/_friend", PMany [PEntity [("person/name", s' "Peirce")]])]) r2
          r3 <- pl "[:person/_friend]" "Boole"
          assertEq "nobody points at Boole" (PEntity []) r3
    , unit ":default, :as and limit" $
        withPulls $ \pl _ _ -> do
          r <- pl "[(:person/age :default 0) (:person/name :as \"n\") (limit :person/alias 1) (:person/boss :default 0)]" "Boole"
          assertEq "Boole" (PEntity [("person/age", i' 49), ("n", s' "Boole"), ("person/boss", i' 0)]) r
          r2 <- pl "[(limit :person/alias 1)]" "Frege"
          assertEq "limit 1" (PEntity [("person/alias", PMany [s' "G."])]) r2
    , unit ":db/id as an attribute" $
        withPulls $ \pl idOf _ -> do
          r <- pl "[:db/id :person/name]" "Boole"
          assertEq "id first" (PEntity [("db/id", i' (idOf "Boole")), ("person/name", s' "Boole")]) r
    , unit "recursion to a depth, and without limit on a cycle" $
        withPulls $ \pl idOf _ -> do
          let f = idOf "Frege"
          r <- pl "[:person/name {:person/friend 2}]" "Frege"
          assertEq "depth 2"
            (PEntity [ ("person/name", s' "Frege"), ("person/friend", PMany [
               PEntity [ ("person/name", s' "Peirce"), ("person/friend", PMany [
                 PEntity [ ("person/name", s' "De Morgan"), ("person/friend", PMany [did f]) ] ]) ] ]) ]) r
          r1 <- pl "[:person/name {:person/friend 1}]" "Frege"
          assertEq "depth 1: one level is expanded, the next is only an id"
            (PEntity [ ("person/name", s' "Frege"), ("person/friend", PMany [
               PEntity [ ("person/name", s' "Peirce"), ("person/friend", PMany [did (idOf "De Morgan")]) ] ]) ]) r1
          r2 <- pl "[:person/name {:person/friend ...}]" "Frege"
          -- Frege -> Peirce -> De Morgan -> Frege: the cycle is cut where it closes
          assertEq "unlimited"
            (PEntity [ ("person/name", s' "Frege"), ("person/friend", PMany [
               PEntity [ ("person/name", s' "Peirce"), ("person/friend", PMany [
                 PEntity [ ("person/name", s' "De Morgan"), ("person/friend", PMany [did f]) ] ]) ] ]) ]) r2
    , unit "pullLookup, rendering and JSON" $
        withPulls $ \_ _ db -> do
          let pat = either error id (parsePullText "[:person/name {:person/friend [:person/name]}]")
          r <- pullLookup db pat "person/name" (VStr "Frege")
          assertEq "rendered" (Just "{:person/name \"Frege\", :person/friend [{:person/name \"Peirce\"}]}") (renderPulled <$> r)
          assertEq "json" (Just (A.object [ "person/name" A..= ("Frege" :: Text)
                                          , "person/friend" A..= [A.object ["person/name" A..= ("Peirce" :: Text)]] ]))
                          (pulledToJSON <$> r)
          none <- pullLookup db pat "person/name" (VStr "nobody")
          assertEq "no such entity" Nothing none
    , unit "(pull ?e pattern) in :find, with other find elements and with aggregates" $
        withTestConn pullSchema $ \c -> do
          _ <- transactEdn c pullData
          r <- qCells c "[:find ?n (pull ?e [:person/age]) :where [?e :person/name ?n] [?e :person/age ?a] [(> ?a 60)]]" []
          assertEq "three people over 60"
            [ [CVal (VStr "De Morgan"), CPull (PEntity [("person/age", i' 65)])]
            , [CVal (VStr "Frege"),     CPull (PEntity [("person/age", i' 70)])]
            , [CVal (VStr "Peirce"),    CPull (PEntity [("person/age", i' 75)])] ] r
          r2 <- qCells c "[:find (pull ?e [:person/name]) (count ?f) :where [?e :person/friend ?f]]" []
          assertEq "grouped by the pulled entity" 3 (length r2)
          assertEq "one friend each" True (all (\row -> case row of [CPull _, CVal (VInt 1)] -> True; _ -> False) r2)
          expectError "plain runner refuses pull"
            (q c "[:find (pull ?e [:person/age]) :where [?e :person/age _]]" [])
            (\e -> case e of QueryError m -> "pull" `T.isInfixOf` T.pack m; _ -> False)
    , unit "bad patterns are reported" $ do
        let bad t = either (const True) (const False) (parsePullText t)
        assertThat "not a vector" (bad ":a")
        assertThat "bad nested value" (bad "[{:a \"x\"}]")
        assertThat "unknown option" (bad "[(:a :bogus 1)]")
        assertThat "number key" (bad "[1]")
    , prop "[*] reproduces every entity of a random model" 80 $
        forAll genModel $ \m -> monadicIO $ do
          bad <- run $ withModel m $ \c eid -> do
            let eidN i = case eid i of VInt n -> n; _ -> 0
            withDb c $ \db -> fmap concat . forM [1 .. mN m] $ \i -> do
              got <- pull db [PWild] (eidN i)
              let ls = sort [ eid j | (i', j) <- S.toList (mLinks m), i' == i ]
                  tg = S.toList (M.findWithDefault S.empty i (mTags m))
                  expected = PEntity $
                    [ ("db/id", PScalar (eid i)) ]
                    ++ [ ("p/age", PScalar (VInt (fromIntegral a))) | Just a <- [M.lookup i (mAge m)] ]
                    ++ [ ("p/link", PMany [ PEntity [("db/id", PScalar j)] | j <- ls ]) | not (null ls) ]
                    ++ [ ("p/tag", PMany [ PScalar (VStr t) | t <- tg ]) | not (null tg) ]
              pure [ (i, expected, got) | got /= expected ]
          unless (null bad) $ stop (counterexample (show (take 1 bad)) False)
    ]

-- ---------------------------------------------------------------------------
-- Planner and estimates

plannerTests :: IO [Bool]
plannerTests = do
  section "Planner and estimates"
  sequence
    [ unit "counts equal scan sizes; sampled distinct estimates are sensible" $
        withTestConn (M.fromList [("x/bucket", attr TLong), ("x/serial", attr TLong)]) $ \c -> do
          let n = 5000 :: Int
          _ <- transact c [ EntityMap ENew [ ("x/bucket", TVal (VInt (fromIntegral (i `mod` 10))))
                                           , ("x/serial", TVal (VInt (fromIntegral i))) ]
                          | i <- [1 .. n] ]
          withDb c $ \db -> do
            cb <- countDatoms db Nothing (Just "x/bucket") Nothing
            assertEq "count bucket" (fromIntegral n) cb
            c3 <- countDatoms db Nothing (Just "x/bucket") (Just (VInt 3))
            assertEq "count bucket=3" (fromIntegral (n `div` 10)) c3
            db1 <- distinctEstimate db "x/bucket"
            assertThat ("bucket distinct ~10, got " ++ show db1) (db1 >= 9 && db1 <= 12)
            db2 <- distinctEstimate db "x/serial"
            assertThat ("serial distinct ~5000, got " ++ show db2) (db2 >= 4000 && db2 <= 5000)
    , unit "the most selective clause runs first" $
        withTestConn personSchema $ \c -> do
          _ <- transact c [ EntityMap ENew [ ("person/name", TVal (VStr (T.pack ("p" ++ show i))))
                                           , ("person/age", TVal (VInt (fromIntegral (i `mod` 100)))) ]
                          | i <- [1 .. 2000 :: Int] ]
          (rows, plan) <- withDb c $ \db -> explainEdn db "[:find ?n :where [?e :person/name ?n] [?e :person/age 65]]" []
          assertEq "20 people aged 65" 20 (length rows)
          assertThat ("plan was: " ++ show plan) (case plan of (l : _) -> "65" `T.isInfixOf` l; _ -> False)
          (_, plan2) <- withDb c $ \db -> explainEdn db "[:find ?n :where [?e :person/age 65] [?e :person/name ?n]]" []
          assertEq "same plan regardless of how it was written" plan (map (T.replace "rows 1 -> 20" "rows 1 -> 20") plan2)
    ]

-- ---------------------------------------------------------------------------
-- EDN

ednTests :: IO [Bool]
ednTests = do
  section "EDN reader"
  sequence
    [ unit "scalars, collections, comments, discard" $ do
        assertEq "mixed"
          (Right (EVec [EKw "a", EInt 1, EFloat 2.5, EStr "s\n", EMap [(EKw "k", EVec [ESym "x", ESym "y"])], ESet [EInt 1, EInt 2], ENil, EBool True]))
          (readEdn "[:a 1 2.5 \"s\\n\" {:k [x y]} #{1 2} nil true ; a comment\n #_ignored ]")
    , unit "keywords may be IRIs" $
        assertEq "iri keyword" (Right (EKw "http://schema.org/name")) (readEdn ":http://schema.org/name")
    , unit "errors are reported, not crashed on" $ do
        assertThat "unclosed" (either (const True) (const False) (readEdn "[1 2"))
        assertThat "odd map" (either (const True) (const False) (readEdn "{:a}"))
        assertThat "tag" (either (const True) (const False) (readEdn "#inst \"2020\""))
    ]

-- ---------------------------------------------------------------------------
-- JSON-LD

jsonLdDoc :: A.Value
jsonLdDoc = A.object
  [ "@context" A..= A.object
      [ "schema" A..= ("http://schema.org/" :: Text)
      , "name"   A..= ("schema:name" :: Text)
      , "age"    A..= ("schema:age" :: Text)
      , "knows"  A..= A.object ["@id" A..= ("schema:knows" :: Text), "@type" A..= ("@id" :: Text)] ]
  , "@graph" A..=
      [ A.object [ "@id" A..= ("http://ex.org/alice" :: Text), "@type" A..= ("schema:Person" :: Text)
                 , "name" A..= ("Alice" :: Text), "age" A..= (30 :: Int)
                 , "knows" A..= [ A.object ["@id" A..= ("http://ex.org/bob" :: Text), "name" A..= ("Bob" :: Text)]
                                , A.String "http://ex.org/carol" ] ]
      , A.object [ "@id" A..= ("http://ex.org/carol" :: Text), "name" A..= ("Carol" :: Text)
                 , "knows" A..= ("http://ex.org/alice" :: Text) ] ] ]

jsonLdTests :: IO [Bool]
jsonLdTests = do
  section "JSON-LD"
  sequence
    [ unit "import maps nodes to entities; refs join; a node defined twice is merged" $ withTestConn M.empty $ \c -> do
        _ <- importJsonLD c jsonLdDoc
        r <- rowsOf c "[:find ?a ?b :where [?x :http://schema.org/name ?a] [?x :http://schema.org/knows ?y] [?y :http://schema.org/name ?b]]"
        assertEq "knows pairs"
          [ [VStr "Alice", VStr "Bob"], [VStr "Alice", VStr "Carol"], [VStr "Carol", VStr "Alice"] ] r
        n <- rowsOf c "[:find (count ?e) :where [?e :jsonld/id _]]"
        assertEq "three nodes (carol was both referenced and defined)" [[VInt 3]] n
    , unit "re-import is idempotent (@id is a unique identity)" $ withTestConn M.empty $ \c -> do
        _ <- importJsonLD c jsonLdDoc
        n1 <- withDb c datomCount
        rep <- importJsonLD c jsonLdDoc
        n2 <- withDb c datomCount
        assertEq "no new datoms" n1 n2
        assertEq "nothing asserted" [] [ d | (True, d) <- txData rep ]
    , unit "export compacts IRIs and embeds referenced nodes up to a depth" $ withTestConn M.empty $ \c -> do
        _ <- importJsonLD c jsonLdDoc
        let ctx = M.fromList [("schema", "http://schema.org/")]
        v <- withDb c $ \db -> exportByIri db defaultExportOptions { eoContext = ctx, eoDepth = 1 } ["http://ex.org/alice"]
        case v of
          A.Object o -> do
            assertEq "@id" (Just (A.String "http://ex.org/alice")) (KM.lookup "@id" o)
            assertEq "@type compacted" (Just (A.String "schema:Person")) (KM.lookup "@type" o)
            assertEq "name" (Just (A.String "Alice")) (KM.lookup "schema:name" o)
            assertEq "age" (Just (A.Number 30)) (KM.lookup "schema:age" o)
            case KM.lookup "schema:knows" o of
              Just (A.Array xs) -> assertEq "two known people" 2 (length xs)
              other -> throwIO (userError ("schema:knows: " ++ show other))
          _ -> throwIO (userError "export is not an object")
        v0 <- withDb c $ \db -> exportByIri db defaultExportOptions { eoDepth = 0 } ["http://ex.org/alice"]
        case v0 of
          A.Object o -> case KM.lookup "http://schema.org/knows" o of
            Just (A.Array xs) -> assertThat "depth 0 gives bare @id refs" (all isBareRef xs)
            other -> throwIO (userError ("knows at depth 0: " ++ show other))
          _ -> throwIO (userError "export is not an object")
    , unit "unsupported constructs are reported" $ withTestConn M.empty $ \c ->
        expectError "@list"
          (importJsonLD c (A.object ["http://ex.org/p" A..= A.object ["@list" A..= ([1, 2] :: [Int])]]))
          (\e -> case e of ParseError _ -> True; _ -> False)
    ]
  where
    isBareRef (A.Object o) = KM.size o == 1 && isJust (KM.lookup "@id" o)
    isBareRef _ = False

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  results <- concat <$> sequence
    [ codecTests, kvTests, storeTests, bigValueTests, queryTests, rangeTests, orTests, pullTests, ruleTests, plannerTests, ednTests, jsonLdTests ]
  let failed = length (filter not results)
  putStrLn ("\n" ++ show (length results - failed) ++ "/" ++ show (length results) ++ " passed")
  when (failed > 0) exitFailure
  _ <- pure (fromMaybe (0 :: Word8) Nothing, sort [1 :: Int])
  pure ()
