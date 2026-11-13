-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests of hosigraph-abi.  Everything here runs without a database, except the last group,
-- which runs when HOSIGRAPH_DTLV (the dtlv executable) or HOSIGRAPH_DATALEVIN_JAR (and
-- HOSIGRAPH_JAVA) says there is one.
--
--  * the shapes of "Hosigraph.Abi.Api" are consistent, and what the encoders write fits them;
--  * every operation, through the dispatcher, with a fake 'Backend' in place of hosigraph;
--  * how a failure of hosigraph or takka is told (status and kind);
--  * how @open@ reads its argument;
--  * the generated files are the ones the description gives.
module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import           System.Environment (lookupEnv)
import           System.Exit
import           System.IO
import           System.Timeout (timeout)
import           Test.QuickCheck (Gen, Result(output), Testable, Args(..), arbitrary, chatty, elements, forAll, isSuccess, listOf1, oneof, quickCheckWithResult, stdArgs, suchThat)

import           Hosigraph
import           Hosigraph.Abi.Api
import           Hosigraph.Abi.Codec
import           Hosigraph.Abi.Files (generatedFiles)
import           Hosigraph.Abi.Handlers
import           Hosigraph.Abi.Library
import           Hosigraph.Manifest (ChunkEntry(..))
import           Setu.Codec (ToEdn(..), FromEdn(..))
import           Setu.Abi (Library(..))
import           Setu.Dispatch
import           Setu.Gen (checkFiles)
import           Setu.Spec (Ty(TVec), checkApi)
import           Setu.Ticket (Status(..))
import           Setu.Validate (renderTyError, validate)
import           Setu.Wire
import           Takka.Edn (Edn(..), readEdn)
import           Takka.Syntax (Pulled(..))
import           Takka.Types (TakkaError(..))

type Test = (String, IO ())

check :: String -> Bool -> IO ()
check msg ok = unless ok (throwIO (userError msg))

checkEq :: (Eq a, Show a) => String -> a -> a -> IO ()
checkEq msg wanted got = unless (wanted == got) $
  throwIO (userError (msg ++ "\n      wanted: " ++ show wanted ++ "\n      got:    " ++ show got))

prop :: Testable p => Int -> p -> IO ()
prop n p = do
  r <- quickCheckWithResult stdArgs { maxSuccess = n, chatty = False } p
  unless (isSuccess r) (throwIO (userError (output r)))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  mexe  <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_DTLV"
  mjar  <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_DATALEVIN_JAR"
  mjava <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_JAVA"
  let java = maybe "java" id mjava
      tag t = map (\(n, a) -> (t ++ " " ++ n, a))
      real = maybe [] (\exe -> tag "[dtlv]" (realTests ("[:dtlv " <> tshow exe <> "]"))) mexe
          ++ maybe [] (\jar -> tag "[jvm]" (realTests ("[:jvm " <> tshow java <> " " <> tshow jar <> "]"))) mjar
  when (null real) $ putStrLn "  (real hosigraph tests skipped: set HOSIGRAPH_DTLV, or HOSIGRAPH_DATALEVIN_JAR and HOSIGRAPH_JAVA)"
  results <- forM (apiTests ++ codecTests ++ opTests ++ failureTests ++ openTests ++ generatedTests ++ real) $ \(name, act) -> do
    r <- try act
    case r of
      Right () -> putStrLn ("  ok    " ++ name) >> pure True
      Left (e :: SomeException) -> putStrLn ("  FAIL  " ++ name ++ "\n      " ++ show e) >> pure False
  let failed = length (filter not results)
  putStrLn (show (length results - failed) ++ " passed, " ++ show failed ++ " failed")
  when (failed > 0) exitFailure
  where
    nonEmpty x = if null x then Nothing else Just x
    tshow :: String -> Text
    tshow = T.pack . show

-- ---------------------------------------------------------------------------
-- Helpers

edn :: Text -> Edn
edn t = either (error . ("test EDN: " ++)) id (readEdn t)

-- | A dispatcher over the given backend.
dispatchOver :: Backend -> Dispatch
dispatchOver b = either (error . show) id (newDispatch api (handlers b))

-- | Ask: the status, and the answer read back.
ask :: Dispatch -> Text -> IO (Status, Reply)
ask d txt = do
  (st, body) <- runRequest d (TE.encodeUtf8 txt)
  case decodeReply body of
    Right r -> pure (st, r)
    Left e  -> throwIO (userError ("the answer is not an envelope: " ++ show e ++ " in " ++ show body))

-- | The same EDN with every map's entries in one order: the order a map is written in does not
-- matter to a reader, and the tests should not hold the encoders to it.
canon :: Edn -> Edn
canon e = case e of
  EMap ps -> EMap (sortOn (show . fst) [ (canon k, canon v) | (k, v) <- ps ])
  EVec xs -> EVec (map canon xs)
  EList xs -> EList (map canon xs)
  ESet xs -> ESet (sortOn show (map canon xs))
  _ -> e

canonReply :: Reply -> Reply
canonReply (ReplyOk e) = ReplyOk (canon e)
canonReply r = r

askOk :: Dispatch -> Text -> Text -> IO ()
askOk d req wanted = do
  (st, r) <- ask d req
  checkEq ("status of " ++ T.unpack req) StOk st
  checkEq ("answer to " ++ T.unpack req) (ReplyOk (canon (edn wanted))) (canonReply r)

askErr :: Dispatch -> Text -> Status -> Text -> IO Text
askErr d req wantedSt wantedKind = do
  (st, r) <- ask d req
  case r of
    ReplyErr k m -> do
      checkEq ("status of " ++ T.unpack req) wantedSt st
      checkEq ("kind of " ++ T.unpack req) wantedKind k
      pure m
    ReplyOk v -> throwIO (userError ("wanted an error, got " ++ show v))

-- | The backend of the tests: it notes what it was asked, and answers with fixed things.
fake :: IORef [String] -> Backend
fake logRef = Backend
  { bWrite = \ops -> note ("write " ++ show ops) >> pure report1
  , bWriteMany = \bs -> note ("write-many " ++ show bs)
      >> pure [Right report2, Left (WriteRejected "the unique value is taken"), Left (BadArgument "empty batch")]
  , bSeal = note "seal" >> pure (Just sealed)
  , bGetEntities = \is -> note ("get-entities " ++ show is) >> pure [entity1, M.empty]
  , bQueryMany = \qs -> note ("query-many " ++ show qs) >> pure [[[VStr "Frege", VInt 70]], []]
  , bQueryCellsMany = \qs -> note ("query-cells-many " ++ show qs) >> pure
      [ [ [ CVal (VStr "Frege"), CPull (PEntity [(Keyword "person/name", PScalar (VStr "Frege")), (Keyword "person/tag", PMany [PScalar (VStr "a"), PScalar (VStr "b")])]) ] ] ]
  , bHistory = \i -> note ("history " ++ show i) >> pure [ev1, ev2]
  , bHistoryTrace = \i -> note ("history-trace " ++ show i) >> pure ([ev1], trace1)
  , bVerify = note "verify" >> pure ["manifest: a signature does not match"]
  , bStats = note "stats" >> pure stats1
  , bRebuildKhana = note "rebuild-khana"
  , bErase = \es -> note ("erase " ++ show es) >> pure eraseRep1
  , bClean = \o -> note ("clean " ++ show (coMaxChunks o)) >> pure cleanRep1
  , bFreeze = \o -> note ("freeze " ++ show (foMaxUnits o)) >> pure freezeRep1
  }
  where note s = atomicModifyIORef' logRef (\xs -> (xs ++ [s], ()))

report1, report2 :: WriteReport
report1 = WriteReport (M.fromList [(1, 1001), (2, 1002)]) 1 2 False
report2 = WriteReport (M.fromList [(1, 2001)]) 3 3 True

entity1 :: M.Map Keyword [Value]
entity1 = M.fromList [("person/name", [VStr "Frege"]), ("person/age", [VInt 70]), ("person/tag", [VStr "a", VKw "b/c"])]

ev1, ev2 :: Event
ev1 = Event 1 KCreate 1001 "person/name" (VStr "Frege")
ev2 = Event 2 KDelete 1001 "person/age" (VInt 70)

chunk1 :: ChunkEntry
chunk1 = ChunkEntry
  { ceName = "c-2026-11-create", ceTier = "hot", ceKind = KCreate, ceMonth = (2026, 11)
  , ceEvents = 4, ceIds = 2, ceMinId = 10, ceMaxId = 20, ceMinSeq = 1, ceMaxSeq = 4
  , ceFiles = [], ceHash = "abcd", ceErase = Nothing }

sealed :: SealReport
sealed = SealReport { srEvents = 4, srChunks = [chunk1], srGeneration = 3 }

trace1 :: LookupTrace
trace1 = LookupTrace
  { ltMonth = (2026, 11), ltChunks = 5, ltSkippedByMonth = 2, ltSkippedByRange = 1, ltSkippedByErase = 0, ltSkippedByFilter = 1
  , ltProbed = [("c-2026-11-create", Star 2), ("c-2026-11-delete", LookAlike), ("old", NotStar), ("empty", EmptySky)]
  , ltTailEvents = 1 }

eraseRep1 :: EraseReport
eraseRep1 = EraseReport { erGeneration = 2, erDropped = ["c-2026-10-create"], erRecorded = ["c-2026-11-create"], erRetracted = 7, erOwned = [1001, 1002] }

cleanRep1 :: CleanReport
cleanRep1 = CleanReport
  { crRewritten = [("c-2026-11-create", Just "c-2026-11-create-2"), ("c-2026-10-delete", Nothing)]
  , crSkipped = ["c-2026-09-create"], crRecordsLeft = 1, crKhanaCleaned = False, crKhanaRebuilt = False }

freezeRep1 :: FreezeReport
freezeRep1 = FreezeReport
  { frFrozen = [((KCreate, (2026, 9)), Just "c-2026-09-create-3"), ((KDelete, (2026, 8)), Nothing)]
  , frSkipped = [(KCreate, (2026, 7))] }

-- | A moment given as the door gives it: milliseconds since 1970.
atMs :: Integer -> UTCTime
atMs ms = posixSecondsToUTCTime (fromInteger ms / 1000)

stats1 :: Stats
stats1 = Stats { stGeneration = 3, stChunks = 5, stSealedSeq = 40, stNextSeq = 45, stTailEvents = 4
               , stTailBytes = 512, stFoldedSeq = 44, stKhanaDatoms = 99 }

withLog :: (IORef [String] -> Dispatch -> IO a) -> IO a
withLog act = do
  ref <- newIORef []
  act ref (dispatchOver (fake ref))

-- ---------------------------------------------------------------------------
-- The shapes

apiTests :: [Test]
apiTests =
  [ ("api: the description is consistent", checkEq "problems" [] (checkApi api))

  , ("api: what the encoders write fits the shapes the description promises", do
      let fits name ty e = either (\err -> throwIO (userError (name ++ ": " ++ T.unpack (renderTyError err)))) pure (validate ty e)
      fits "write report" tyWriteReport (toEdn report1)
      fits "seal report" tySealReport (toEdn (Just sealed))
      fits "seal report (nothing)" tySealReport (toEdn (Nothing :: Maybe SealReport))
      fits "entities" (TVec tyEntity) (toEdn [entity1, M.empty])
      fits "event" tyEvent (toEdn ev1)
      case encTrace ([ev1], trace1) of
        EMap ps | Just t <- lookup (EKw "trace") ps -> fits "trace" tyTrace t
        other -> throwIO (userError ("encTrace: " ++ show other))
      fits "stats" tyStats (toEdn stats1)
      fits "erase report" tyEraseReport (encEraseReport eraseRep1)
      fits "clean report" tyCleanReport (encCleanReport cleanRep1)
      fits "clean report (nothing to do)" tyCleanReport (encCleanReport (CleanReport [] [] 0 False False))
      fits "freeze report" tyFreezeReport (encFreezeReport freezeRep1)
      forM_ [ "[:ids [1 2]]", "[:range 3 9]", "[:between 0 1000]", "[:owned-by [:post/author] [7]]", "[:owned-by-between [:post/author :media/owner] [7] 0 1000]" ] $ \e ->
        fits ("erasure " ++ T.unpack e) tyErasure (edn e)
      fits "cell: a pulled tree" tyCell (toEdn (CPull (PEntity [("a", PMany [PScalar (VInt 1)])])))
      fits "val: a reference" tyVal (EVec [EKw "ref", EVec [EKw "tmp", EInt 1]])
      fits "op" tyOp (EVec [EKw "create", EVec [EKw "tmp", EInt 1], EKw "person/name", EStr "x"])
      fits "input" tyInput (EVec [EKw "rel", EVec [EVec [EInt 1, EStr "a"]]]))
  ]

-- ---------------------------------------------------------------------------
-- Codecs

genValue :: Gen Value
genValue = oneof
  [ VInt <$> arbitrary
  , VDouble <$> arbitrary `suchThat` (\d -> not (isNaN d || isInfinite d))
  , VStr . T.pack <$> arbitrary
  , VBool <$> arbitrary
  , VKw . Keyword . T.pack <$> listOf1 (elements ['a' .. 'z']) ]

codecTests :: [Test]
codecTests =
  [ ("codec: a value goes out and comes back", prop 500 $ forAll genValue $ \v -> fromEdn (toEdn v) == Right v)

  , ("codec: an op is read as written", do
      checkEq "create" (Right (Create (Tmp 1) "person/name" (Lit (VStr "Frege")))) (fromEdn (edn "[:create [:tmp 1] :person/name \"Frege\"]"))
      checkEq "delete a reference" (Right (Delete (Id 7) "person/friend" (RefTo (Tmp 2)))) (fromEdn (edn "[:delete 7 :person/friend [:ref [:tmp 2]]]"))
      checkEq "a bare integer is a literal" (Right (Create (Id 7) "a" (Lit (VInt 5)))) (fromEdn (edn "[:create 7 :a 5]"))
      checkEq "a reference to an existing entity" (Right (Create (Id 7) "a" (RefTo (Id 5)))) (fromEdn (edn "[:create 7 :a [:ref 5]]")))

  , ("codec: inputs", do
      checkEq "scalar" (Right (InScalar (VStr "x"))) (fromEdn (edn "[:scalar \"x\"]"))
      checkEq "coll" (Right (InColl [VInt 1, VInt 2])) (fromEdn (edn "[:coll [1 2]]"))
      checkEq "rel" (Right (InRel [[VInt 1, VStr "a"], [VInt 2, VStr "b"]])) (fromEdn (edn "[:rel [[1 \"a\"] [2 \"b\"]]]")))

  , ("codec: a pulled tree keeps its order, and a scalar is not a vector", do
      checkEq "tree" (canon (edn "[:pull {:person/name \"Frege\" :person/tag [\"a\" \"b\"]}]"))
        (canon (toEdn (CPull (PEntity [("person/name", PScalar (VStr "Frege")), ("person/tag", PMany [PScalar (VStr "a"), PScalar (VStr "b")])]))))
      checkEq "value" (EInt 3) (toEdn (CVal (VInt 3))))

  , ("codec: an erasure is read as written, and a time is milliseconds since 1970", do
      checkEq "ids" (Right (EraseIds [7, 8])) (fromEdn (edn "[:ids [7 8]]"))
      checkEq "range" (Right (EraseRange 10 20)) (fromEdn (edn "[:range 10 20]"))
      checkEq "between" (Right (EraseBetween (atMs 1700000000123) (atMs 1700000009000))) (fromEdn (edn "[:between 1700000000123 1700000009000]"))
      checkEq "owned-by" (Right (EraseOwnedBy ["post/author", "media/owner"] [5] Nothing)) (fromEdn (edn "[:owned-by [:post/author :media/owner] [5]]"))
      checkEq "owned-by-between" (Right (EraseOwnedBy ["post/author"] [5] (Just (atMs 1000, atMs 2500)))) (fromEdn (edn "[:owned-by-between [:post/author] [5] 1000 2500]"))
      checkEq "a moment before 1970" (Right (EraseBetween (atMs (-1500)) (atMs 0))) (fromEdn (edn "[:between -1500 0]"))
      checkEq "no tag of ours" (Left "not an erasure" :: Either Text Erasure) (fromEdn (edn "[:everything]")))

  , ("codec: the options of clean and freeze may be left out, and so may their one field", do
      checkEq "clean, no args" (Right (coMaxChunks defaultCleanOptions)) (coMaxChunks <$> decCleanOptions ENil)
      checkEq "clean, empty map" (Right (coMaxChunks defaultCleanOptions)) (coMaxChunks <$> decCleanOptions (edn "{}"))
      checkEq "clean, nil" (Right (coMaxChunks defaultCleanOptions)) (coMaxChunks <$> decCleanOptions (edn "{:max-chunks nil}"))
      checkEq "clean, 3" (Right 3) (coMaxChunks <$> decCleanOptions (edn "{:max-chunks 3}"))
      checkEq "freeze, no args" (Right (foMaxUnits defaultFreezeOptions)) (foMaxUnits <$> decFreezeOptions ENil)
      checkEq "freeze, 2" (Right 2) (foMaxUnits <$> decFreezeOptions (edn "{:max-units 2}")))

  , ("codec: what a failed batch of write-many is told as", do
      checkEq "rejected" (edn "[:error :write-rejected \"no\"]") (encWriteResult (Left (WriteRejected "no")))
      checkEq "bad argument" (edn "[:error :bad-argument \"empty\"]") (encWriteResult (Left (BadArgument "empty")))
      checkEq "corrupt" (edn "[:error :corrupt \"x\"]") (encWriteResult (Left (Corrupt "x"))))
  ]

-- ---------------------------------------------------------------------------
-- Operations, through the dispatcher, over a fake backend

opTests :: [Test]
opTests =
  [ ("op write: ops are read, and the ids the temps got come back", withLog $ \ref d -> do
      askOk d "{:op :write :args {:ops [[:create [:tmp 1] :person/name \"Frege\"] [:create [:tmp 1] :person/friend [:ref [:tmp 2]]] [:delete 7 :person/age 70]]}}"
              "{:temps [[1 1001] [2 1002]] :first-seq 1 :last-seq 2 :sealed false}"
      calls <- readIORef ref
      checkEq "what the backend was asked"
        ["write " ++ show [ Create (Tmp 1) "person/name" (Lit (VStr "Frege"))
                          , Create (Tmp 1) "person/friend" (RefTo (Tmp 2))
                          , Delete (Id 7) "person/age" (Lit (VInt 70)) ]]
        calls)

  , ("op write: a request that is not the shape is refused before the backend is asked", withLog $ \ref d -> do
      _ <- askErr d "{:op :write :args {:ops [[:update 1 :a 2]]}}" StProtocol "bad-args"
      _ <- askErr d "{:op :write :args {:ops [[:create [:tmp \"x\"] :a 2]]}}" StProtocol "bad-args"
      _ <- askErr d "{:op :write :args {:ops [[:create 1 :a [1 2]]]}}" StProtocol "bad-args"
      _ <- askErr d "{:op :write :args {:ops \"x\"}}" StProtocol "bad-args"
      _ <- askErr d "{:op :write}" StProtocol "bad-args"
      checkEq "the backend was not asked" [] =<< readIORef ref)

  , ("op write-many: each batch has its own answer, and a refused one says why", withLog $ \_ d ->
      askOk d "{:op :write-many :args {:batches [[[:create [:tmp 1] :a 1]] [[:create [:tmp 1] :a 1]] []]}}"
              "[[:ok {:temps [[1 2001]] :first-seq 3 :last-seq 3 :sealed true}] [:error :write-rejected \"the unique value is taken\"] [:error :bad-argument \"empty batch\"]]")

  , ("op seal", withLog $ \_ d ->
      askOk d "{:op :seal}"
        "{:events 4 :generation 3 :chunks [{:name \"c-2026-11-create\" :tier \"hot\" :kind :create :month \"2026-11\" :events 4 :ids 2 :min-id 10 :max-id 20 :min-seq 1 :max-seq 4 :hash \"abcd\"}]}")

  , ("op seal: nil when there was nothing to seal", do
      ref <- newIORef []
      let d = dispatchOver (fake ref) { bSeal = pure Nothing }
      askOk d "{:op :seal}" "nil")

  , ("op get-entities", withLog $ \ref d -> do
      askOk d "{:op :get-entities :args {:ids [1001 5]}}" "[{:person/age [70] :person/name [\"Frege\"] :person/tag [\"a\" :b/c]} {}]"
      checkEq "what the backend was asked" ["get-entities [1001,5]"] =<< readIORef ref)

  , ("op query-many: texts and inputs, in order", withLog $ \ref d -> do
      askOk d "{:op :query-many :args {:queries [{:q \"[:find ?n ?a :in $ ?x :where [?e :person/name ?n]]\" :inputs [[:scalar \"Frege\"] [:coll [1 2]]]} {:q \"[:find ?e :where [?e :a 1]]\"}]}}"
              "[[[\"Frege\" 70]] []]"
      checkEq "what the backend was asked"
        ["query-many " ++ show [ ("[:find ?n ?a :in $ ?x :where [?e :person/name ?n]]" :: Text, [InScalar (VStr "Frege"), InColl [VInt 1, VInt 2]])
                               , ("[:find ?e :where [?e :a 1]]", []) ]]
        =<< readIORef ref)

  , ("op query-cells-many: a pulled tree comes as [:pull tree]", withLog $ \_ d ->
      askOk d "{:op :query-cells-many :args {:queries [{:q \"[:find ?n (pull ?e [*]) :where [?e :person/name ?n]]\"}]}}"
              "[[[\"Frege\" [:pull {:person/name \"Frege\" :person/tag [\"a\" \"b\"]}]]]]")

  , ("op history", withLog $ \ref d -> do
      askOk d "{:op :history :args {:id 1001}}"
              "[{:seq 1 :kind :create :id 1001 :attr :person/name :val \"Frege\"} {:seq 2 :kind :delete :id 1001 :attr :person/age :val 70}]"
      checkEq "what the backend was asked" ["history 1001"] =<< readIORef ref)

  , ("op history-trace", withLog $ \_ d ->
      askOk d "{:op :history-trace :args {:id 1001}}"
              ("{:events [{:seq 1 :kind :create :id 1001 :attr :person/name :val \"Frege\"}] :trace {:month \"2026-11\" :chunks 5 :skipped-by-month 2 :skipped-by-range 1 :skipped-by-erase 0 :skipped-by-filter 1 "
               <> ":probed [{:chunk \"c-2026-11-create\" :says :star :found 2} {:chunk \"c-2026-11-delete\" :says :look-alike :found nil} {:chunk \"old\" :says :not-star :found nil} {:chunk \"empty\" :says :empty-sky :found nil}] :tail-events 1}}"))

  , ("op verify, stats, rebuild-khana", withLog $ \ref d -> do
      askOk d "{:op :verify}" "[\"manifest: a signature does not match\"]"
      askOk d "{:op :stats}" "{:generation 3 :chunks 5 :sealed-seq 40 :next-seq 45 :tail-events 4 :tail-bytes 512 :folded-seq 44 :khana-datoms 99}"
      askOk d "{:op :rebuild-khana}" "nil"
      checkEq "what the backend was asked" ["verify", "stats", "rebuild-khana"] =<< readIORef ref)

  , ("op erase: every kind of erasure is read, and the report comes back", withLog $ \ref d -> do
      askOk d "{:op :erase :args {:erasures [[:ids [7 8]] [:range 10 20] [:between 1000 2500] [:owned-by [:post/author :media/owner] [5]] [:owned-by-between [:post/author] [5] 1000 2500]]}}"
              "{:generation 2 :dropped [\"c-2026-10-create\"] :recorded [\"c-2026-11-create\"] :retracted 7 :owned [1001 1002]}"
      checkEq "what the backend was asked"
        ["erase " ++ show [ EraseIds [7, 8], EraseRange 10 20, EraseBetween (atMs 1000) (atMs 2500)
                          , EraseOwnedBy ["post/author", "media/owner"] [5] Nothing
                          , EraseOwnedBy ["post/author"] [5] (Just (atMs 1000, atMs 2500)) ]]
        =<< readIORef ref)

  , ("op erase: no erasure at all is a call that is made", withLog $ \ref d -> do
      askOk d "{:op :erase :args {:erasures []}}" "{:generation 2 :dropped [\"c-2026-10-create\"] :recorded [\"c-2026-11-create\"] :retracted 7 :owned [1001 1002]}"
      checkEq "what the backend was asked" ["erase []"] =<< readIORef ref)

  , ("op clean: with its argument, or without any", withLog $ \ref d -> do
      let wanted = "{:rewritten [{:chunk \"c-2026-11-create\" :into \"c-2026-11-create-2\"} {:chunk \"c-2026-10-delete\" :into nil}] :skipped [\"c-2026-09-create\"] :records-left 1 :khana-cleaned false :khana-rebuilt false}"
      askOk d "{:op :clean}" wanted
      askOk d "{:op :clean :args nil}" wanted
      askOk d "{:op :clean :args {}}" wanted
      askOk d "{:op :clean :args {:max-chunks 3}}" wanted
      checkEq "what the backend was asked" (map (\n -> "clean " ++ n) [show (maxBound :: Int), show (maxBound :: Int), show (maxBound :: Int), "3"]) =<< readIORef ref)

  , ("op freeze: with its argument, or without any", withLog $ \ref d -> do
      let wanted = "{:frozen [{:kind :create :month \"2026-09\" :into \"c-2026-09-create-3\"} {:kind :delete :month \"2026-08\" :into nil}] :skipped [{:kind :create :month \"2026-07\"}]}"
      askOk d "{:op :freeze}" wanted
      askOk d "{:op :freeze :args {:max-units 2}}" wanted
      checkEq "what the backend was asked" ["freeze " ++ show (maxBound :: Int), "freeze 2"] =<< readIORef ref)

  , ("op erase, clean, freeze: what is not the shape is refused before the backend is asked", withLog $ \ref d -> do
      forM_ [ "{:op :erase}"                                                   -- the erasures are not optional
            , "{:op :erase :args {:erasures [[:all]]}}"                        -- no such tag
            , "{:op :erase :args {:erasures [[:between 1000]]}}"               -- one time is not a span
            , "{:op :erase :args {:erasures [[:between 1000 \"2000\"]]}}"      -- a time is a number
            , "{:op :erase :args {:erasures [[:owned-by [\"post/author\"] [5]]]}}"   -- an attribute is a keyword
            , "{:op :erase :args {:erasures [[:ids [1]]] :soon true}}"
            , "{:op :clean :args {:max-chunk 3}}"                              -- a mistyped key is not passed in silence
            , "{:op :clean :args {:max-chunks \"3\"}}"
            , "{:op :freeze :args [1]}"
            , "{:op :freeze :args {:max-units 1.5}}" ] $ \req ->
        void (askErr d req StProtocol "bad-args")
      checkEq "the backend was not asked" [] =<< readIORef ref)

  , ("op erase: what hosigraph refuses is the application's refusal, with its kind", do
      ref <- newIORef []
      let d = dispatchOver (fake ref) { bErase = \_ -> throwIO (BadArgument "erase: the time span ends before it starts") }
      m <- askErr d "{:op :erase :args {:erasures [[:between 2000 1000]]}}" StApp "bad-argument"
      check ("the message: " ++ T.unpack m) ("time span" `T.isInfixOf` m))

  , ("op: arguments given to an operation that takes none are refused", withLog $ \ref d -> do
      _ <- askErr d "{:op :stats :args {:x 1}}" StProtocol "bad-args"
      _ <- askErr d "{:op :get-entities :args {:ids [1] :surprise 2}}" StProtocol "bad-args"
      checkEq "the backend was not asked" [] =<< readIORef ref)

  , ("op: ping and abi are answered whatever the backend", withLog $ \_ d -> do
      askOk d "{:op :ping :args [1 \"é\"]}" "[1 \"é\"]"
      (st, r) <- ask d "{:op :abi}"
      checkEq "status" StOk st
      case r of
        ReplyOk (EMap ps) -> do
          checkEq "name" (Just (EStr "hosi")) (lookup (EKw "name") ps)
          checkEq "version" (Just (EInt (toInteger (apiVersion' )))) (lookup (EKw "version") ps)
        other -> throwIO (userError (show other)))
  ]
  where apiVersion' = 1 :: Int

-- ---------------------------------------------------------------------------
-- How a failure is told

failureTests :: [Test]
failureTests =
  [ ("failure: what hosigraph refuses on purpose is HOSI_ERR_APP with its kind", do
      forM_ [ (WriteRejected "m1", "write-rejected"), (Corrupt "m2", "corrupt"), (BadArgument "m3", "bad-argument") ] $ \(e, kind) -> do
        ref <- newIORef []
        let d = dispatchOver (fake ref) { bStats = throwIO e }
        m <- askErr d "{:op :stats}" StApp kind
        check "the message is kept" (T.pack (drop 0 (case e of WriteRejected s -> s; Corrupt s -> s; BadArgument s -> s)) == m))

  , ("failure: what takka refuses on purpose is HOSI_ERR_APP too", do
      forM_ [ (TxError "t", "tx-error"), (QueryError "q", "query-error"), (SchemaError "s", "schema-error"), (ParseError "p", "parse-error") ] $ \(e, kind) -> do
        ref <- newIORef []
        let d = dispatchOver (fake ref) { bStats = throwIO e }
        _ <- askErr d "{:op :stats}" StApp kind
        pure ())

  , ("failure: a query that cannot be read is refused at the door as parse-error; the backend is not asked", do
      -- the real backend throws a DtlvError for it, a bare string: it would be answered as INTERNAL
      ref <- newIORef []
      let asked = \_ -> throwIO (ErrorCall "the backend was asked")
          d = dispatchOver (fake ref) { bQueryMany = asked, bQueryCellsMany = asked }
      forM_ ["query-many", "query-cells-many"] $ \op -> do
        m <- askErr d ("{:op :" <> op <> " :args {:queries [{:q \"[:find ?n :where\"}]}}") StApp "parse-error"
        check "it says which query" ("query 0" `T.isInfixOf` m)
        check "the backend was not asked" (not ("the backend" `T.isInfixOf` m)))

  , ("failure: an I/O error is HOSI_ERR_IO, and any other exception is HOSI_ERR_INTERNAL", do
      ref <- newIORef []
      let d1 = dispatchOver (fake ref) { bStats = throwIO (userError "disk gone") }
          d2 = dispatchOver (fake ref) { bStats = throwIO (ErrorCall "oops") }
      _ <- askErr d1 "{:op :stats}" StIo "io"
      m <- askErr d2 "{:op :stats}" StInternal "exception"
      check "it says what" ("oops" `T.isInfixOf` m))

  , ("failure: a result that hosigraph made but the shape does not allow is the library's mistake", do
      ref <- newIORef []
      let nan = 0 / 0 :: Double
          d = dispatchOver (fake ref) { bGetEntities = \_ -> pure [M.fromList [("a", [VDouble nan])]] }
      _ <- askErr d "{:op :get-entities :args {:ids [1]}}" StInternal "bad-result"
      pure ())

  , ("failure: a connection opened without a database refuses every operation but ping and abi", do
      let d = dispatchOver unattached
      forM_ ["{:op :stats}", "{:op :seal}", "{:op :verify}", "{:op :rebuild-khana}", "{:op :history :args {:id 1}}", "{:op :write :args {:ops []}}"
                 , "{:op :erase :args {:erasures []}}", "{:op :clean}", "{:op :freeze}"] $ \req ->
        void (askErr d req StApp "bad-argument")
      askOk d "{:op :ping :args 1}" "1")

  , ("failure: guarded lets an asynchronous exception through", do
      r <- timeout 2000000 (try (guarded (throwIO ThreadKilled)))
      case r of
        Just (Left ThreadKilled) -> pure ()
        other -> throwIO (userError ("wanted ThreadKilled to pass through, got " ++ show (fmap (either show (const "a result")) other))))
  ]

-- ---------------------------------------------------------------------------
-- open

openTests :: [Test]
openTests =
  [ ("open: {} opens nothing", case parseOpen "{}" of
      Right a -> checkEq "dir" Nothing (oaDir a)
      Left e  -> throwIO (userError (T.unpack e)))

  , ("open: the whole argument is read", case parseOpen
        "{:dir \"/var/lib/hosi\" :schema {:person/name {:type :string :unique :identity} :person/friend {:type :ref :card :many} :x {}} :config {:khana [:jvm \"java\" \"/opt/d.jar\"] :sync false :seal-events 50 :seal-age-ms 1000 :block-events 8 :reader-cache-bytes 1024 :cold-after-ms 86400000 :cold-block-events 64}}" of
      Left e  -> throwIO (userError (T.unpack e))
      Right a -> do
        checkEq "dir" (Just "/var/lib/hosi") (oaDir a)
        checkEq "schema" (M.fromList [ ("person/name", identity (attr TString)), ("person/friend", many ref), ("x", attr TAny) ]) (oaSchema a)
        let c = oaConfig a
        checkEq "khana" (KhanaDatalevinJvm "java" "/opt/d.jar") (hcKhanaBackend c)
        checkEq "sync" False (hcSync c)
        checkEq "seal-events" 50 (hcSealEvents c)
        checkEq "seal-age-ms" (Just 1000) (hcSealAgeMs c)
        checkEq "block-events" 8 (hcBlockEvents c)
        checkEq "reader-cache-bytes" 1024 (hcReaderCacheBytes c)
        checkEq "cold-after-ms" (Just 86400000) (hcColdAfterMs c)
        checkEq "cold-block-events" 64 (hcColdBlockEvents c))

  , ("open: what is left out is hosigraph's default", case parseOpen "{:dir \"d\" :config {:khana [:dtlv \"/bin/dtlv\"]}}" of
      Left e  -> throwIO (userError (T.unpack e))
      Right a -> do
        let c = oaConfig a; dc = defaultConfig
        checkEq "khana" (KhanaDtlv "/bin/dtlv") (hcKhanaBackend c)
        checkEq "sync" (hcSync dc) (hcSync c)
        checkEq "seal-events" (hcSealEvents dc) (hcSealEvents c)
        checkEq "block-events" (hcBlockEvents dc) (hcBlockEvents c)
        checkEq "cold-after-ms: nothing is cold unless asked" Nothing (hcColdAfterMs c)
        checkEq "cold-block-events" (hcColdBlockEvents dc) (hcColdBlockEvents c)
        checkEq "schema" M.empty (oaSchema a))

  , ("open: what is not the shape is said at once, with where", do
      forM_ [ ("not edn (", "not EDN"), ("{:dir 5}", ":dir"), ("{:dir \"d\" :surprise 1}", "surprise")
            , ("{:dir \"d\" :schema {:a {:type :text}}}", "type"), ("{:dir \"d\" :config {:khana [:s3 \"x\"]}}", "khana")
            , ("{:dir \"d\" :config {:sync 1}}", "sync"), ("[1 2]", "map")
            , ("{:dir \"d\" :config {:cold-after-ms \"1d\"}}", "cold-after-ms") ] $ \(txt, needle) ->
        case parseOpen txt of
          Left e  -> check ("the message names " ++ needle ++ ": " ++ T.unpack e) (T.pack needle `T.isInfixOf` e)
          Right _ -> throwIO (userError ("accepted: " ++ T.unpack txt)))

  , ("open: a backend that is not there is HOSI_ERR_APP; no database at all is a connection that answers ping", do
      r1 <- libConnect library "{:dir \"/tmp/hosi-abi-never\" :config {:khana [:dtlv \"/nonexistent/dtlv\"]}}"
      case r1 of
        Left (Failure st kind _) -> checkEq "status, kind" (StApp, "bad-argument") (st, kind)
        Right _ -> throwIO (userError "opened with a backend that is not there")
      r2 <- libConnect library "{}"
      case r2 of
        Left f -> throwIO (userError (show f))
        Right (d, release) -> askOk d "{:op :ping :args 1}" "1" >> release
      r3 <- libConnect library "{:dir 5}"
      case r3 of
        Left (Failure st kind _) -> checkEq "status, kind" (StProtocol, "bad-open") (st, kind)
        Right _ -> throwIO (userError "opened with a bad argument"))
  ]

-- ---------------------------------------------------------------------------
-- The generated files

generatedTests :: [Test]
generatedTests =
  [ ("generated: gen/ is what the description gives (run the generator with --out if this fails)", do
      bad <- checkFiles "gen" generatedFiles
      checkEq "files that differ" [] bad) ]

-- ---------------------------------------------------------------------------
-- Real hosigraph (needs a Datalevin)

withTmp :: (FilePath -> IO a) -> IO a
withTmp act = do
  base <- getTemporaryDirectory
  (p, h) <- openTempFile base "hosigraph-abi-test"
  hClose h
  removeFile p
  createDirectory p
  act p `finally` removePathForcibly p

-- | The operations through the dispatcher over a real Hosi (the khana backend given as the EDN
-- of @:khana@).  Not run when no Datalevin is set up.
realTests :: Text -> [Test]
realTests khana =
  [ ("real: write, read now, read the past, seal, verify, rebuild", withTmp $ \dir -> do
      let uri = "{:dir " <> T.pack (show dir) <> " :schema {:person/name {:type :string :unique :identity} :person/age {:type :long} :person/friend {:type :ref :card :many}} :config {:khana "
                <> khana <> "}}"
      r <- libConnect library uri
      (d, release) <- either (\f -> throwIO (userError (show f))) pure r
      (do
        (st, w) <- ask d "{:op :write :args {:ops [[:create [:tmp 1] :person/name \"Frege\"] [:create [:tmp 1] :person/age 70] [:create [:tmp 2] :person/name \"Russell\"] [:create [:tmp 2] :person/friend [:ref [:tmp 1]]]]}}"
        checkEq "write status" StOk st
        ids <- case w of
          ReplyOk (EMap ps) | Just (EVec ts) <- lookup (EKw "temps") ps -> pure [ i | EVec [_, EInt i] <- ts ]
          other -> throwIO (userError ("write: " ++ show other))
        (frege, russell) <- case ids of
          [a, b] -> pure (a, b)
          _      -> throwIO (userError ("wanted two ids, got " ++ show ids))
        (st2, ents) <- ask d ("{:op :get-entities :args {:ids [" <> T.pack (show frege) <> " " <> T.pack (show russell) <> "]}}")
        checkEq "get-entities status" StOk st2
        case ents of
          ReplyOk (EVec [EMap a, EMap b]) -> do
            checkEq "frege's name" (Just (EVec [EStr "Frege"])) (lookup (EKw "person/name") a)
            checkEq "russell's friend" (Just (EVec [EInt (toInteger frege)])) (lookup (EKw "person/friend") b)
          other -> throwIO (userError ("get-entities: " ++ show other))
        (st3, q) <- ask d "{:op :query-many :args {:queries [{:q \"[:find ?n :where [?e :person/name ?n]]\"}]}}"
        checkEq "query status" StOk st3
        case q of
          ReplyOk (EVec [EVec rows]) -> checkEq "two names" 2 (length rows)
          other -> throwIO (userError ("query-many: " ++ show other))
        (st4, h) <- ask d ("{:op :history :args {:id " <> T.pack (show frege) <> "}}")
        checkEq "history status" StOk st4
        case h of
          ReplyOk (EVec evs) -> checkEq "frege has two events" 2 (length evs)
          other -> throwIO (userError ("history: " ++ show other))
        (st5, s) <- ask d "{:op :seal}"
        checkEq "seal status" StOk st5
        check "something was sealed" (s /= ReplyOk ENil)
        askOk d "{:op :verify}" "[]"
        askOk d "{:op :rebuild-khana}" "nil"
        askOk d "{:op :verify}" "[]"
        (st6, _) <- ask d "{:op :stats}"
        checkEq "stats status" StOk st6
        (st7, broken) <- ask d "{:op :query-many :args {:queries [{:q \"[:find ?n :where\"}]}}"
        checkEq "a query that does not parse is the application's refusal" StApp st7
        case broken of
          ReplyErr k _ -> check ("the kind of a broken query: " ++ T.unpack k) (k `elem` ["parse-error", "query-error"])
          ReplyOk v    -> throwIO (userError ("a broken query was answered: " ++ show v)))
        `finally` release)

  , ("real: erase what a friend owns, clean it off the disk, freeze the quiet month (through the door)", withTmp $ \dir -> do
      let uri = "{:dir " <> T.pack (show dir) <> " :schema {:person/name {:type :string :unique :identity} :person/friend {:type :ref :card :many}} :config {:khana "
                <> khana <> " :cold-after-ms 1 :cold-block-events 4}}"
          field' k r = case r of
            ReplyOk (EMap ps) | Just v <- lookup (EKw k) ps -> pure v
            other -> throwIO (userError ("no :" ++ T.unpack k ++ " in " ++ show other))
          idText = T.pack . show
          okReply (st, r) what = checkEq (what ++ ": status") StOk st >> pure r
      r <- libConnect library uri
      (d, release) <- either (\f -> throwIO (userError (show f))) pure r
      (do
        w <- ask d "{:op :write :args {:ops [[:create [:tmp 1] :person/name \"Frege\"] [:create [:tmp 2] :person/name \"Russell\"] [:create [:tmp 2] :person/friend [:ref [:tmp 1]]] [:create [:tmp 3] :person/name \"Cantor\"]]}}" >>= (`okReply` "write")
        ids <- field' "temps" w >>= \v -> case v of
          EVec ts -> pure [ i | EVec [_, EInt i] <- ts ]
          other   -> throwIO (userError ("temps: " ++ show other))
        (frege, russell, cantor) <- case ids of
          [a, b, c] -> pure (a, b, c)
          _         -> throwIO (userError ("wanted three ids, got " ++ show ids))
        s <- ask d "{:op :seal}" >>= (`okReply` "seal")
        month <- case s of
          ReplyOk (EMap ps) | Just (EVec cs) <- lookup (EKw "chunks") ps
                            , [m] <- [ m | EMap c <- cs, lookup (EKw "kind") c == Just (EKw "create"), Just m <- [lookup (EKw "month") c] ] -> pure m
          other -> throwIO (userError ("seal: " ++ show other))

        -- Russell is Frege's friend: what [:person/friend] has Frege as its value is Russell, and only him
        e <- ask d ("{:op :erase :args {:erasures [[:owned-by [:person/friend] [" <> idText frege <> "]]]}}") >>= (`okReply` "erase")
        checkEq "the owned" (EVec [EInt (toInteger russell)]) =<< field' "owned" e
        gen <- field' "generation" e
        check ("a new generation: " ++ show gen) (gen /= EInt 0)
        askOk d ("{:op :history :args {:id " <> idText russell <> "}}") "[]"
        (hf, past) <- ask d ("{:op :history :args {:id " <> idText frege <> "}}")
        checkEq "frege's past: status" StOk hf
        case past of
          ReplyOk (EVec [_]) -> pure ()
          other -> throwIO (userError ("frege's past is whole, one event: " ++ show other))
        ents <- ask d ("{:op :get-entities :args {:ids [" <> idText russell <> " " <> idText cantor <> "]}}") >>= (`okReply` "get-entities")
        case ents of
          ReplyOk (EVec [EMap gone, EMap kept]) -> do
            checkEq "russell is gone from now" [] gone
            check "cantor is not" (not (null kept))
          other -> throwIO (userError ("get-entities: " ++ show other))
        askOk d "{:op :verify}" "[]"

        -- the bytes leave the disk
        c1 <- ask d "{:op :clean}" >>= (`okReply` "clean")
        checkEq "no record is left" (EInt 0) =<< field' "records-left" c1
        checkEq "one chunk was written again" 1 . length . (\v -> case v of EVec xs -> xs; _ -> []) =<< field' "rewritten" c1
        checkEq "the khana was made new" (EBool True) =<< field' "khana-cleaned" c1
        askOk d "{:op :clean}" "{:rewritten [] :skipped [] :records-left 0 :khana-cleaned false :khana-rebuilt false}"
        askOk d "{:op :verify}" "[]"

        -- nobody has read the month since: after a moment it goes cold, and the past is as it was
        threadDelay 50000
        f1 <- ask d "{:op :freeze}" >>= (`okReply` "freeze")
        case f1 of
          ReplyOk (EMap ps) | Just (EVec [EMap u]) <- lookup (EKw "frozen") ps -> do
            checkEq "the unit" (Just (EKw "create"), Just month) (lookup (EKw "kind") u, lookup (EKw "month") u)
            check "it became a chunk" (case lookup (EKw "into") u of Just (EStr _) -> True; _ -> False)
          other -> throwIO (userError ("freeze: " ++ show other))
        checkEq "nothing was skipped" (EVec []) =<< field' "skipped" f1
        askOk d "{:op :freeze}" "{:frozen [] :skipped []}"
        (hf2, evs) <- ask d ("{:op :history :args {:id " <> idText frege <> "}}")
        checkEq "frege's past, from a cold chunk" StOk hf2
        case evs of
          ReplyOk (EVec [EMap ev]) -> checkEq "frege's name" (Just (EStr "Frege")) (lookup (EKw "val") ev)
          other -> throwIO (userError ("history: " ++ show other))
        askOk d ("{:op :history :args {:id " <> idText russell <> "}}") "[]"
        askOk d "{:op :verify}" "[]")
        `finally` release)
  ]

-- ---------------------------------------------------------------------------
