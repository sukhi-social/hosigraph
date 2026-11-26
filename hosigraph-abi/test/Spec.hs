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
import qualified Data.ByteString as BS
import           Data.IORef
import           Data.List (sort, sortOn)
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
import           Hosigraph.Abi.Json
import           Hosigraph.Abi.Stdio (Format(..), runStdio)
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
import qualified Takka.Types as K

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
      real = maybe [] (\exe -> tag "[dtlv-repl]" (realTests ("[:dtlv-repl " <> tshow exe <> "]"))) mexe
          ++ maybe [] (\jar -> tag "[jvm]" (realTests ("[:jvm " <> tshow java <> " " <> tshow jar <> "]"))) mjar
  when (null real) $ putStrLn "  (real hosigraph tests skipped: set HOSIGRAPH_DTLV, or HOSIGRAPH_DATALEVIN_JAR and HOSIGRAPH_JAVA)"
  results <- forM (apiTests ++ codecTests ++ opTests ++ failureTests ++ openTests ++ stdioTests ++ jsonTests ++ generatedTests ++ real) $ \(name, act) -> do
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
  { bWrite = \cs ops -> note ("write " ++ show ops ++ (if null cs then "" else " when " ++ show cs))
      >> pure (if any ((== 13) . condIdT) cs then Left [ c | c <- cs, condIdT c == 13 ] else Right report1)
  , bWriteMany = \bs -> note ("write-many " ++ show bs)
      >> pure [Right report2, Left (WriteRejected "the unique value is taken"), Left (BadArgument "empty batch")]
  , bSeal = note "seal" >> pure (Just sealed)
  , bGetEntities = \is -> note ("get-entities " ++ show is) >> pure [entity1, M.empty]
  , bHighest = \a c n -> note ("highest " ++ show a ++ " " ++ show c ++ " " ++ show n) >> pure [Datom 1002 a (VInt 75), Datom 1001 a (VInt 75)]
  , bHighestIn = \a ks c n -> note ("highest-in " ++ show a ++ " " ++ show ks ++ " " ++ show c ++ " " ++ show n)
      >> pure [Datom 1002 a (VTuple [Just (VStr "b"), Just (VInt 75)]), Datom 1001 a (VTuple [Just (VStr "a"), Just (VInt 75)])]
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

condIdT :: Cond -> HosiId
condIdT c = case c of Holds i _ _ -> i; Lacks i _ _ -> i; Unset i _ -> i

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

  , ("op write: :when is read, and handed to the backend with the ops", withLog $ \ref d -> do
      askOk d "{:op :write :args {:ops [[:create 5 :post/likes 4]] :when [[:holds 5 :post/likes 3] [:lacks 5 :post/liked-by 9] [:unset 5 :post/pinned]]}}"
              "{:temps [[1 1001] [2 1002]] :first-seq 1 :last-seq 2 :sealed false}"
      calls <- readIORef ref
      checkEq "what the backend was asked"
        ["write " ++ show [Create (Id 5) "post/likes" (Lit (VInt 4))]
          ++ " when " ++ show [Holds 5 "post/likes" (VInt 3), Lacks 5 "post/liked-by" (VInt 9), Unset 5 "post/pinned"]]
        calls
      -- :when left out, or nil, or empty: the same as write
      _ <- askOk d "{:op :write :args {:ops [[:create 5 :a 1]] :when nil}}" "{:temps [[1 1001] [2 1002]] :first-seq 1 :last-seq 2 :sealed false}"
      _ <- askOk d "{:op :write :args {:ops [[:create 5 :a 1]] :when []}}" "{:temps [[1 1001] [2 1002]] :first-seq 1 :last-seq 2 :sealed false}"
      calls2 <- readIORef ref
      checkEq "no conditions: asked with none" (replicate 2 ("write " ++ show [Create (Id 5) "a" (Lit (VInt 1))])) (drop 1 calls2))

  , ("op write: a condition that does not hold is HOSI_ERR_APP, kind conflict, and the message is the failed ones as EDN", withLog $ \ref d -> do
      m <- askErr d "{:op :write :args {:ops [[:create 13 :post/likes 4]] :when [[:holds 5 :post/likes 3] [:holds 13 :post/likes 3] [:unset 13 :post/pinned]]}}" StApp "conflict"
      checkEq "the failed conditions, in the order given" (edn "[[:holds 13 :post/likes 3] [:unset 13 :post/pinned]]") (edn m)
      calls <- readIORef ref
      checkEq "the backend was asked once" 1 (length calls))

  , ("op write: a :when that is not the shape is refused before the backend is asked", withLog $ \ref d -> do
      _ <- askErr d "{:op :write :args {:ops [[:create 1 :a 2]] :when [[:equals 1 :a 2]]}}" StProtocol "bad-args"
      _ <- askErr d "{:op :write :args {:ops [[:create 1 :a 2]] :when [[:holds [:tmp 1] :a 2]]}}" StProtocol "bad-args"
      _ <- askErr d "{:op :write :args {:ops [[:create 1 :a 2]] :when [[:holds 1 :a]]}}" StProtocol "bad-args"
      _ <- askErr d "{:op :write :args {:ops [[:create 1 :a 2]] :when [[:unset 1 :a 2]]}}" StProtocol "bad-args"
      _ <- askErr d "{:op :write :args {:ops [[:create 1 :a 2]] :when \"x\"}}" StProtocol "bad-args"
      checkEq "the backend was not asked" [] =<< readIORef ref)

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

  , ("op highest: the attribute, where to start and how many; the answer is the places, in the order the backend gave them", withLog $ \ref d -> do
      askOk d "{:op :highest :args {:attr :person/age :limit 2}}" "[{:val 75 :id 1002} {:val 75 :id 1001}]"
      askOk d "{:op :highest :args {:attr :person/age :limit 2 :after {:val 75 :id 1001}}}" "[{:val 75 :id 1002} {:val 75 :id 1001}]"
      askOk d "{:op :highest :args {:attr :person/age :limit 2 :after nil}}" "[{:val 75 :id 1002} {:val 75 :id 1001}]"
      checkEq "what the backend was asked"
        [ "highest :person/age Nothing 2", "highest :person/age Just (VInt 75,1001) 2", "highest :person/age Nothing 2" ] =<< readIORef ref)

  , ("op highest: arguments of the wrong shape are refused before the backend is asked", withLog $ \ref d -> do
      forM_ [ "{:op :highest}"
            , "{:op :highest :args {:attr :person/age}}"                                  -- how many is not optional
            , "{:op :highest :args {:limit 2}}"
            , "{:op :highest :args {:attr \"person/age\" :limit 2}}"                      -- an attribute is a keyword
            , "{:op :highest :args {:attr :person/age :limit \"2\"}}"
            , "{:op :highest :args {:attr :person/age :limit 2 :after {:val 75}}}"          -- a place is a value and an id
            , "{:op :highest :args {:attr :person/age :limit 2 :after {:id 1001}}}"
            , "{:op :highest :args {:attr :person/age :limit 2 :after 75}}"
            , "{:op :highest :args {:attr :person/age :limit 2 :after {:val 75 :id 1001 :more 1}}}"
            , "{:op :highest :args {:attr :person/age :limit 2 :soon true}}" ] $ \req ->
        void (askErr d req StProtocol "bad-args")
      checkEq "the backend was not asked" [] =<< readIORef ref)

  , ("op highest: what hosigraph refuses is the application's refusal, with its kind", do
      ref <- newIORef []
      let d = dispatchOver (fake ref) { bHighest = \_ _ _ -> throwIO (BadArgument "highest: the count is 0, and has to be 1 or more") }
      m <- askErr d "{:op :highest :args {:attr :person/age :limit 0}}" StApp "bad-argument"
      check ("the message: " ++ T.unpack m) ("has to be 1 or more" `T.isInfixOf` m))

  , ("op highest-in: the attribute, the keys, where to start and how many; the answer is the keyed places, in the order the backend gave them", withLog $ \ref d -> do
      askOk d "{:op :highest-in :args {:attr :note/by+at :keys [\"a\" \"b\"] :limit 2}}" "[{:key \"b\" :val 75 :id 1002} {:key \"a\" :val 75 :id 1001}]"
      askOk d "{:op :highest-in :args {:attr :note/by+at :keys [\"a\"] :limit 2 :after {:val 75 :id 1001}}}" "[{:key \"b\" :val 75 :id 1002} {:key \"a\" :val 75 :id 1001}]"
      askOk d "{:op :highest-in :args {:attr :note/by+at :keys [] :limit 2 :after nil}}" "[{:key \"b\" :val 75 :id 1002} {:key \"a\" :val 75 :id 1001}]"
      checkEq "what the backend was asked"
        [ "highest-in :note/by+at [VStr \"a\",VStr \"b\"] Nothing 2"
        , "highest-in :note/by+at [VStr \"a\"] Just (VInt 75,1001) 2"
        , "highest-in :note/by+at [] Nothing 2" ] =<< readIORef ref)

  , ("op highest-in: arguments of the wrong shape are refused before the backend is asked", withLog $ \ref d -> do
      forM_ [ "{:op :highest-in}"
            , "{:op :highest-in :args {:attr :note/by+at :limit 2}}"                              -- the keys are not optional
            , "{:op :highest-in :args {:attr :note/by+at :keys [\"a\"]}}"                         -- nor is how many
            , "{:op :highest-in :args {:keys [\"a\"] :limit 2}}"
            , "{:op :highest-in :args {:attr \"note/by+at\" :keys [\"a\"] :limit 2}}"             -- an attribute is a keyword
            , "{:op :highest-in :args {:attr :note/by+at :keys \"a\" :limit 2}}"                   -- the keys are a vector
            , "{:op :highest-in :args {:attr :note/by+at :keys [\"a\"] :limit \"2\"}}"
            , "{:op :highest-in :args {:attr :note/by+at :keys [\"a\"] :limit 2 :after {:val 75}}}"
            , "{:op :highest-in :args {:attr :note/by+at :keys [\"a\"] :limit 2 :after {:val 75 :id 1001 :key \"a\"}}}"   -- a place has no key: the last answer is not one
            , "{:op :highest-in :args {:attr :note/by+at :keys [\"a\"] :limit 2 :soon true}}" ] $ \req ->
        void (askErr d req StProtocol "bad-args")
      checkEq "the backend was not asked" [] =<< readIORef ref)

  , ("op highest-in: what hosigraph refuses is the application's refusal, with its kind", do
      ref <- newIORef []
      let d = dispatchOver (fake ref) { bHighestIn = \_ _ _ _ -> throwIO (BadArgument "highestIn: x is not a tuple of two attributes, (key, order)") }
      m <- askErr d "{:op :highest-in :args {:attr :note/at :keys [1] :limit 2}}" StApp "bad-argument"
      check ("the message: " ++ T.unpack m) ("not a tuple" `T.isInfixOf` m))

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
      forM_ [ (WriteRejected "m1", "write-rejected"), (Corrupt "m2", "corrupt"), (BadArgument "m3", "bad-argument")
            , (WorldBusy "m4", "world-busy"), (BackendMissing "m5", "backend-missing"), (SchemaRefused "m6", "schema-refused") ] $ \(e, kind) -> do
        ref <- newIORef []
        let d = dispatchOver (fake ref) { bStats = throwIO e }
        m <- askErr d "{:op :stats}" StApp kind
        check "the message is kept" (T.pack (drop 0 (case e of WriteRejected s -> s; Corrupt s -> s; BadArgument s -> s; WorldBusy s -> s; BackendMissing s -> s; SchemaRefused s -> s)) == m))

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

  , ("failure: :order-by / :limit on a find that Datalevin would answer whole, without a word, is refused at the door too", do
      ref <- newIORef []
      let asked = \_ -> throwIO (ErrorCall "the backend was asked")
          d = dispatchOver (fake ref) { bQueryMany = asked, bQueryCellsMany = asked }
      forM_ ["query-many", "query-cells-many"] $ \op -> do
        m <- askErr d ("{:op :" <> op <> " :args {:queries [{:q \"[:find ?a . :where [?e :person/age ?a] :order-by [?a]]\"}]}}") StApp "parse-error"
        check "it says what" (":order-by" `T.isInfixOf` m)
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
                 , "{:op :erase :args {:erasures []}}", "{:op :clean}", "{:op :freeze}"
                 , "{:op :highest :args {:attr :a :limit 1}}", "{:op :highest-in :args {:attr :a :keys [] :limit 1}}"] $ \req ->
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
        "{:dir \"/var/lib/hosi\" :schema {:person/name {:type :string :unique :identity} :person/friend {:type :ref :card :many} :x {} :f/pair {:unique :identity :tuple [:f/a :f/b]}} :config {:khana [:jvm \"java\" \"/opt/d.jar\"] :sync false :seal-events 50 :seal-age-ms 1000 :block-events 8 :reader-cache-bytes 1024 :cold-after-ms 86400000 :cold-block-events 64}}" of
      Left e  -> throwIO (userError (T.unpack e))
      Right a -> do
        checkEq "dir" (Just "/var/lib/hosi") (oaDir a)
        checkEq "schema" (M.fromList [ ("person/name", identity (attr TString)), ("person/friend", many ref), ("x", attr TAny), ("f/pair", identity (tuple ["f/a", "f/b"])) ]) (oaSchema a)
        let c = oaConfig a
        checkEq "khana" (KhanaDatalevinJvm "java" "/opt/d.jar") (hcKhanaBackend c)
        checkEq "sync" False (hcSync c)
        checkEq "seal-events" 50 (hcSealEvents c)
        checkEq "seal-age-ms" (Just 1000) (hcSealAgeMs c)
        checkEq "block-events" 8 (hcBlockEvents c)
        checkEq "reader-cache-bytes" 1024 (hcReaderCacheBytes c)
        checkEq "cold-after-ms" (Just 86400000) (hcColdAfterMs c)
        checkEq "cold-block-events" 64 (hcColdBlockEvents c))

  , ("open: what is left out is hosigraph's default", case parseOpen "{:dir \"d\" :config {:khana [:dtlv-repl \"/bin/dtlv\"]}}" of
      Left e  -> throwIO (userError (T.unpack e))
      Right a -> do
        let c = oaConfig a; dc = defaultConfig
        checkEq "khana" (KhanaDtlvRepl "/bin/dtlv") (hcKhanaBackend c)
        checkEq "sync" (hcSync dc) (hcSync c)
        checkEq "seal-events" (hcSealEvents dc) (hcSealEvents c)
        checkEq "block-events" (hcBlockEvents dc) (hcBlockEvents c)
        checkEq "cold-after-ms: nothing is cold unless asked" Nothing (hcColdAfterMs c)
        checkEq "cold-block-events" (hcColdBlockEvents dc) (hcColdBlockEvents c)
        checkEq "schema" M.empty (oaSchema a))

  , ("open: the ways to run the khana are read, and each means what it says", do
      let khana txt = either (Left . T.unpack) (Right . hcKhanaBackend . oaConfig) (parseOpen ("{:dir \"d\" :config {:khana " <> txt <> "}}"))
      checkEq "dtlv is not a way any more" True (either (const True) (const False) (khana "[:dtlv \"/bin/dtlv\"]"))
      checkEq "dtlv-repl" (Right (KhanaDtlvRepl "/bin/dtlv")) (khana "[:dtlv-repl \"/bin/dtlv\"]")
      checkEq "jvm" (Right (KhanaDatalevinJvm "java" "/opt/d.jar")) (khana "[:jvm \"java\" \"/opt/d.jar\"]")
      forM_ ["[:dtlv-repl]", "[:dtlv-repl \"a\" \"b\"]", "[:dtlv-repl 1]", ":dtlv-repl"] $ \txt ->
        case khana txt of
          Left e  -> check ("the message names khana: " ++ e) ("khana" `T.isInfixOf` T.pack e)
          Right b -> throwIO (userError ("accepted " ++ T.unpack txt ++ " as " ++ show b)))

  , ("open: what is not the shape is said at once, with where", do
      forM_ [ ("not edn (", "not EDN"), ("{:dir 5}", ":dir"), ("{:dir \"d\" :surprise 1}", "surprise")
            , ("{:dir \"d\" :schema {:a {:type :text}}}", "type"), ("{:dir \"d\" :config {:khana [:s3 \"x\"]}}", "khana")
            , ("{:dir \"d\" :config {:sync 1}}", "sync"), ("[1 2]", "map")
            , ("{:dir \"d\" :config {:cold-after-ms \"1d\"}}", "cold-after-ms")
            , ("{:dir \"d\" :schema {:a {:tuple :b}}}", "tuple"), ("{:dir \"d\" :schema {:a {:tuple [1 2]}}}", "tuple") ] $ \(txt, needle) ->
        case parseOpen txt of
          Left e  -> check ("the message names " ++ needle ++ ": " ++ T.unpack e) (T.pack needle `T.isInfixOf` e)
          Right _ -> throwIO (userError ("accepted: " ++ T.unpack txt)))

  , ("open: a backend that is not there is HOSI_ERR_APP; no database at all is a connection that answers ping", do
      r1 <- libConnect library "{:dir \"/tmp/hosi-abi-never\" :config {:khana [:dtlv-repl \"/nonexistent/dtlv\"]}}"
      case r1 of
        Left (Failure st kind _) -> checkEq "status, kind" (StApp, "backend-missing") (st, kind)
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
-- hosigraph stdio

-- | Open with the text, hand over the bytes as stdin, and give back the exit code and the lines of stdout.
stdioRun :: Text -> BS.ByteString -> IO (ExitCode, [Text])
stdioRun = stdioRunAs FormatEdn

stdioRunAs :: Format -> Text -> BS.ByteString -> IO (ExitCode, [Text])
stdioRunAs fmt open input = withTmp $ \dir -> do
  let fin = dir ++ "/in"; fout = dir ++ "/out"
  BS.writeFile fin input
  code <- withBinaryFile fin ReadMode $ \i -> withBinaryFile fout WriteMode $ \o -> runStdio fmt open i o
  out <- BS.readFile fout
  pure (code, T.lines (TE.decodeUtf8 out))

stdioTests :: [Test]
stdioTests =
  [ ("stdio: one line in, one line out, in order; a blank line is skipped; \\r\\n is a line end; a bad line is answered and the next goes on", do
      (code, ls) <- stdioRun "{}" (TE.encodeUtf8 "{:op :ping :args 1}\n\n{:op :ping :args 2}\r\nnot edn (\n{:op :nope}\n{:op :ping :args 3}")
      checkEq "exit" ExitSuccess code
      checkEq "answers" 5 (length ls)
      checkEq "the first two" ["{:ok 1}", "{:ok 2}"] (take 2 ls)
      check "the bad line is bad-request" ("bad-request" `T.isInfixOf` (ls !! 2))
      check "the unknown op is unknown-op" ("unknown-op" `T.isInfixOf` (ls !! 3))
      checkEq "the last line has no newline and is still answered" "{:ok 3}" (ls !! 4))

  , ("stdio: a newline inside a string stays inside the line, and so does what is not ASCII", do
      (_, ls) <- stdioRun "{}" (TE.encodeUtf8 "{:op :ping :args \"a\\nb \26085\"}\n")
      checkEq "one line back" ["{:ok \"a\\nb \26085\"}"] ls)

  , ("stdio: bytes that are not UTF-8 are answered as a bad request, not a crash", do
      (code, ls) <- stdioRun "{}" (BS.pack [0xff, 0xfe, 10] <> "{:op :ping}\n")
      checkEq "exit" ExitSuccess code
      checkEq "two answers" 2 (length ls)
      check "the first says UTF-8" ("UTF-8" `T.isInfixOf` (ls !! 0)))

  , ("stdio: without a database only ping and abi answer", do
      (_, ls) <- stdioRun "{}" "{:op :stats}\n{:op :abi}\n"
      check "stats is refused" ("bad-argument" `T.isInfixOf` (ls !! 0))
      check "abi names hosi" ("\"hosi\"" `T.isInfixOf` (ls !! 1)))

  , ("stdio: an open that is not the shape is the one line of stdout, and exit 1", do
      (code, ls) <- stdioRun "{:dir 5}" "{:op :ping}\n"
      checkEq "exit" (ExitFailure 1) code
      checkEq "one line" 1 (length ls)
      check "it says bad-open" ("bad-open" `T.isInfixOf` (ls !! 0)))
  ]


-- | A JSON request as the EDN it stands for (maps in one order).
jsonAsEdn :: JsonOptions -> Text -> Either Text (Text, Edn)
jsonAsEdn o txt = case jsonRequest api o (TE.encodeUtf8 txt) of
  Left e -> Left e
  Right (n, bs) -> Right (n, canon (edn (TE.decodeUtf8 bs)))

jsonTests :: [Test]
jsonTests =
  let plain = JsonOptions False M.empty
      schema = M.fromList
        [ (K.Keyword "a/score", K.defaultAttrSchema { K.asType = K.TDouble })
        , (K.Keyword "a/n", K.defaultAttrSchema { K.asType = K.TLong })
        , (K.Keyword "a/to", K.defaultAttrSchema { K.asType = K.TRef }) ]
      withSchema = JsonOptions False schema
      readsAs o j e = case jsonAsEdn o j of
        Right (_, got) -> checkEq (T.unpack j) (canon (edn e)) got
        Left why       -> throwIO (userError (T.unpack j ++ ": " ++ T.unpack why))
      refusedWith o j needle = case jsonAsEdn o j of
        Left why -> check (T.unpack j ++ ": " ++ T.unpack why) (needle `T.isInfixOf` why)
        Right r  -> throwIO (userError ("accepted: " ++ show r))
      answers o op e = TE.decodeUtf8 (jsonReply api o op (TE.encodeUtf8 e))
  in
  [ ("json: a request reads by the shape: tags, keywords and ids", do
      readsAs plain "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",[\"tmp\",1],\"a/name\",\"Frege\"],[\"delete\",12,\"a/f\",[\"ref\",[\"tmp\",1]]]]}}"
        "{:op :write :args {:ops [[:create [:tmp 1] :a/name \"Frege\"] [:delete 12 :a/f [:ref [:tmp 1]]]]}}"
      readsAs plain "{\"op\":\"stats\"}" "{:op :stats}"
      readsAs plain "{\"op\":\":stats\"}" "{:op :stats}"
      readsAs plain "{\"op\":\"get-entities\",\"args\":{\"ids\":[1,\"58047583928451072\"]}}" "{:op :get-entities :args {:ids [1 58047583928451072]}}"
      readsAs plain "{\"op\":\"erase\",\"args\":{\"erasures\":[[\"owned-by\",[\"a/owner\"],[5]],[\"between\",1,2]]}}"
        "{:op :erase :args {:erasures [[:owned-by [:a/owner] [5]] [:between 1 2]]}}")

  , ("json: a value that starts with a colon is a keyword; ::x is the string :x", do
      readsAs plain "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",1,\"a/m\",\":calm\"],[\"create\",1,\"a/s\",\"::x\"],[\"create\",1,\"a/t\",\"plain\"]]}}"
        "{:op :write :args {:ops [[:create 1 :a/m :calm] [:create 1 :a/s \":x\"] [:create 1 :a/t \"plain\"]]}}")

  , ("json: 1 and 1.0: the schema of the attribute decides, then how the number is written", do
      readsAs withSchema "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",1,\"a/score\",3],[\"create\",1,\"a/score\",3.5]]}}"
        "{:op :write :args {:ops [[:create 1 :a/score 3.0] [:create 1 :a/score 3.5]]}}"
      readsAs withSchema "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",1,\"a/n\",\"58047583928451072\"],[\"create\",1,\"a/to\",[\"ref\",\"7\"]]]}}"
        "{:op :write :args {:ops [[:create 1 :a/n 58047583928451072] [:create 1 :a/to [:ref 7]]]}}"
      -- an attribute the schema does not name: a fraction or an exponent is a double, a whole number is an integer
      readsAs plain "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",1,\"a/x\",1.0],[\"create\",1,\"a/x\",1],[\"create\",1,\"a/x\",1.5]]}}"
        "{:op :write :args {:ops [[:create 1 :a/x 1.0] [:create 1 :a/x 1] [:create 1 :a/x 1.5]]}}"
      -- where the shape asks for a double, a whole number is one
      readsAs plain "{\"op\":\"ping\",\"args\":[1,1.0]}" "{:op :ping :args [1 1.0]}")

  , ("json: {\"type\":..., \"value\":...} says the type of a value, and the schema does not change what was said", do
      readsAs plain "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",1,\"a/x\",{\"type\":\"double\",\"value\":1}],[\"create\",1,\"a/x\",{\"type\":\"long\",\"value\":\"58047583928451072\"}],[\"create\",1,\"a/x\",{\"type\":\"string\",\"value\":\":not-a-keyword\"}],[\"create\",1,\"a/x\",{\"type\":\"keyword\",\"value\":\"calm\"}]]}}"
        "{:op :write :args {:ops [[:create 1 :a/x 1.0] [:create 1 :a/x 58047583928451072] [:create 1 :a/x \":not-a-keyword\"] [:create 1 :a/x :calm]]}}"
      -- the author said long: the attribute is a :double, and it is hosigraph's own check that will say so
      readsAs withSchema "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",1,\"a/score\",{\"type\":\"long\",\"value\":3}]]}}"
        "{:op :write :args {:ops [[:create 1 :a/score 3]]}}"
      readsAs plain "{\"op\":\"ping\",\"args\":{\"type\":\"double\",\"value\":2}}" "{:op :ping :args 2.0}"
      -- where an object is a map of its own, it is a map
      readsAs plain "{\"op\":\"query-many\",\"args\":{\"queries\":[{\"type\":\"double\",\"value\":1}]}}"
        "{:op :query-many :args {:queries [{:type \"double\" :value 1}]}}"
      refusedWith plain "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",1,\"a/x\",{\"type\":\"decimal\",\"value\":1}]]}}" "unknown type"
      refusedWith plain "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",1,\"a/x\",{\"type\":\"double\",\"value\":\"1\"}]]}}" "double")

  , ("json: what is not a request is said, with where", do
      refusedWith plain "not json {" "not JSON"
      refusedWith plain "[1]" "object"
      refusedWith plain "{\"args\":1}" "\"op\""
      refusedWith plain "{\"op\":\"stats\",\"id\":1}" "unknown key"
      refusedWith plain "{\"op\":\"write\",\"args\":{\"ops\":\"x\"}}" "/args/ops"
      refusedWith plain "{\"op\":\"get-entities\",\"args\":{\"ids\":[1.5]}}" "fraction"
      refusedWith plain "{\"op\":\"get-entities\",\"args\":{\"ids\":[\"12a\"]}}" "integer")

  , ("json: an answer is written by the shape: bare keywords where a keyword is asked, 1.0 stays 1.0", do
      checkEq "stats" "{\"ok\":{\"generation\":0,\"chunks\":1,\"sealed-seq\":2,\"next-seq\":3,\"tail-events\":4,\"tail-bytes\":5,\"folded-seq\":6,\"khana-datoms\":7}}"
        (answers plain "stats" "{:ok {:generation 0 :chunks 1 :sealed-seq 2 :next-seq 3 :tail-events 4 :tail-bytes 5 :folded-seq 6 :khana-datoms 7}}")
      checkEq "history" "{\"ok\":[{\"seq\":1,\"kind\":\"create\",\"id\":9,\"attr\":\"a/m\",\"val\":\":calm\"},{\"seq\":2,\"kind\":\"delete\",\"id\":9,\"attr\":\"a/x\",\"val\":1.0}]}"
        (answers plain "history" "{:ok [{:seq 1 :kind :create :id 9 :attr :a/m :val :calm} {:seq 2 :kind :delete :id 9 :attr :a/x :val 1.0}]}")
      checkEq "a string that starts with a colon is written with one more" "{\"ok\":[[[\"::x\"]]]}"
        (answers plain "query-many" "{:ok [[[\":x\"]]]}")
      checkEq "entities: attribute names are keys" "{\"ok\":[{\"a/m\":[\":calm\"],\"a/n\":[2.0,\"s\"]}]}"
        (answers plain "get-entities" "{:ok [{:a/m [:calm] :a/n [2.0 \"s\"]}]}")
      checkEq "a tagged cell" "{\"ok\":[[[[\"pull\",{\"a/n\":[1]}]]]]}"
        (answers plain "query-cells-many" "{:ok [[[[:pull {:a/n [1]}]]]]}")
      checkEq "an error" "{\"error\":\"conflict\",\"message\":\"say \\\"x\\\"\"}"
        (answers plain "write" "{:error :conflict :message \"say \\\"x\\\"\"}"))

  , ("json: ids past 2^53 are strings when asked, and only then", do
      let big = "{:ok {:temps [[1 58047583928451072] [2 9007199254740991]] :first-seq 1 :last-seq 1 :sealed false}}"
      checkEq "as numbers" "{\"ok\":{\"temps\":[[1,58047583928451072],[2,9007199254740991]],\"first-seq\":1,\"last-seq\":1,\"sealed\":false}}"
        (answers plain "write" big)
      checkEq "as strings" "{\"ok\":{\"temps\":[[1,\"58047583928451072\"],[2,9007199254740991]],\"first-seq\":1,\"last-seq\":1,\"sealed\":false}}"
        (answers plain { joIdsAsStrings = True } "write" big))

  , ("json: through stdio, one line in and one line out, and a bad line is answered", do
      (code, ls) <- stdioRunAs (FormatJson False) "{}" (TE.encodeUtf8 "{\"op\":\"ping\",\"args\":{\"k\":[1,1.0,\":a\",\"b\"]}}\nnot json\n{\"op\":\"abi\"}\n{\"op\":\"nope\"}\n")
      checkEq "exit" ExitSuccess code
      checkEq "ping comes back as it went" "{\"ok\":{\"k\":[1,1.0,\":a\",\"b\"]}}" (ls !! 0)
      check "bad line" ("\"error\":\"bad-request\"" `T.isInfixOf` (ls !! 1))
      check "abi" ("\"name\":\"hosi\"" `T.isInfixOf` (ls !! 2))
      check "unknown op" ("unknown-op" `T.isInfixOf` (ls !! 3)))
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
  [ ("real: through JSON stdio, 3 is 3.0 for a double attribute, and ids come back as strings when asked", withTmp $ \dir -> do
      let open = "{:dir " <> T.pack (show dir) <> " :schema {:a/name {:type :string} :a/score {:type :double} :a/any {:type :any :card :many}} :config {:khana " <> khana <> "}}"
      (c, ls) <- stdioRunAs (FormatJson True) open (TE.encodeUtf8 (T.unlines
        [ "{\"op\":\"write\",\"args\":{\"ops\":[[\"create\",[\"tmp\",1],\"a/name\",\"F\"],[\"create\",[\"tmp\",1],\"a/score\",3],[\"create\",[\"tmp\",1],\"a/any\",1.0],[\"create\",[\"tmp\",1],\"a/any\",1]]}}"
        , "{\"op\":\"query-many\",\"args\":{\"queries\":[{\"q\":\"[:find ?e ?s :where [?e :a/score ?s]]\"}]}}" ]))
      checkEq "exit" ExitSuccess c
      check ("the id is a string: " ++ T.unpack (ls !! 0)) ("\"temps\":[[1,\"" `T.isInfixOf` (ls !! 0))
      check ("3 became 3.0: " ++ T.unpack (ls !! 1)) (",3.0]]]}" `T.isSuffixOf` (ls !! 1)))

  , ("real: through stdio, a write and a query, and the database is there when opened again", withTmp $ \dir -> do
      let open = "{:dir " <> T.pack (show dir) <> " :schema {:person/name {:type :string :unique :identity}} :config {:khana " <> khana <> "}}"
          q = "{:op :query-many :args {:queries [{:q \"[:find ?n :where [?e :person/name ?n]]\"}]}}\n"
      (c1, l1) <- stdioRun open (TE.encodeUtf8 ("{:op :write :args {:ops [[:create [:tmp 1] :person/name \"Frege\"]]}}\n" <> q))
      checkEq "exit" ExitSuccess c1
      checkEq "answers" 2 (length l1)
      check "the write is ok" ("{:ok {:temps" `T.isPrefixOf` (l1 !! 0))
      checkEq "the name is there" "{:ok [[[\"Frege\"]]]}" (l1 !! 1)
      (c2, l2) <- stdioRun open (TE.encodeUtf8 q)
      checkEq "exit again" ExitSuccess c2
      checkEq "still there after close and open" ["{:ok [[[\"Frege\"]]]}"] l2)

  , ("real: write, read now, read the past, seal, verify, rebuild", withTmp $ \dir -> do
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

  , ("real: highest pages through the door, one place after another; a tuple comes back through it", withTmp $ \dir -> do
      let uri = "{:dir " <> T.pack (show dir) <> " :schema {:person/age {:type :long} :follow/from {:type :ref} :follow/to {:type :ref} :follow/pair {:type :any :unique :identity :tuple [:follow/from :follow/to]}} :config {:khana "
                <> khana <> "}}"
          idText :: Show a => a -> Text
          idText = T.pack . show
      r <- libConnect library uri
      (d, release) <- either (\f -> throwIO (userError (show f))) pure r
      (do
        let ages = [70, 75, 75, 49, 30] :: [Integer]
        (stw, w) <- ask d ("{:op :write :args {:ops [" <> T.intercalate " " [ "[:create [:tmp " <> idText n <> "] :person/age " <> idText a <> "]" | (n, a) <- zip [1 :: Int ..] ages ] <> "]}}")
        checkEq "write status" StOk stw
        ids <- case w of
          ReplyOk (EMap ps) | Just (EVec ts) <- lookup (EKw "temps") ps -> pure [ i | EVec [_, EInt i] <- ts ]
          other -> throwIO (userError ("write: " ++ show other))
        checkEq "five ids" 5 (length ids)
        let top = reverse (sort (zip ages ids))                       -- the largest value, then the largest id, first
            placeEdn (a, i) = EMap [(EKw "val", EInt a), (EKw "id", EInt i)]
            afterText (a, i) = " :after {:val " <> idText a <> " :id " <> idText i <> "}"
            page :: Maybe (Integer, Integer) -> IO [Edn]
            page after = do
              (st, rep) <- ask d ("{:op :highest :args {:attr :person/age :limit 2" <> maybe "" afterText after <> "}}")
              checkEq "highest status" StOk st
              case rep of
                ReplyOk (EVec ps) -> pure ps
                other -> throwIO (userError ("highest: " ++ show other))
        p1 <- page Nothing
        checkEq "the first page" (map placeEdn (take 2 top)) p1
        p2 <- page (Just (top !! 1))
        checkEq "the page after it" (map placeEdn (take 2 (drop 2 top))) p2
        p3 <- page (Just (top !! 3))
        checkEq "the last page is short" (map placeEdn (drop 4 top)) p3
        p4 <- page (Just (top !! 4))
        checkEq "and after it, nothing" [] p4
        -- what the library refuses is the caller's: kind bad-argument
        _ <- askErr d "{:op :highest :args {:attr :person/age :limit 0}}" StApp "bad-argument"
        _ <- askErr d "{:op :highest :args {:attr :nobody/has :limit 1}}" StApp "bad-argument"
        _ <- askErr d "{:op :highest :args {:attr :person/age :limit 1 :after {:val \"x\" :id 1}}}" StApp "bad-argument"
        -- a tuple is read through the door like any value: a vector
        let (a1, a2) = (ids !! 0, ids !! 1)
        (stf, fw) <- ask d ("{:op :write :args {:ops [[:create [:tmp 1] :follow/from [:ref " <> idText a1 <> "]] [:create [:tmp 1] :follow/to [:ref " <> idText a2 <> "]]]}}")
        checkEq "follow status" StOk stf
        fid <- case fw of
          ReplyOk (EMap ps) | Just (EVec [EVec [_, EInt i]]) <- lookup (EKw "temps") ps -> pure i
          other -> throwIO (userError ("follow: " ++ show other))
        (ste, ents) <- ask d ("{:op :get-entities :args {:ids [" <> idText fid <> "]}}")
        checkEq "get-entities status (a tuple is an answer the shape allows)" StOk ste
        case ents of
          ReplyOk (EVec [EMap m]) -> checkEq "the pair of the entity" (Just (EVec [EVec [EInt a1, EInt a2]])) (lookup (EKw "follow/pair") m)
          other -> throwIO (userError ("get-entities: " ++ show other))
        (stq, q) <- ask d "{:op :query-many :args {:queries [{:q \"[:find ?p :where [?e :follow/pair ?p]]\"}]}}"
        checkEq "query status" StOk stq
        checkEq "the pair, found by a query" (ReplyOk (EVec [EVec [EVec [EVec [EInt a1, EInt a2]]]])) q
        -- ...and asked for in a condition: it holds, and the pair the other way round does not
        let pairText x y = "[" <> idText x <> " " <> idText y <> "]"
            whenText x y = "{:op :write :args {:ops [[:create [:tmp 1] :person/age 20]] :when [[:holds " <> idText fid <> " :follow/pair " <> pairText x y <> "]]}}"
        (stc, _) <- ask d (whenText a1 a2)
        checkEq "a condition on the pair that holds" StOk stc
        _ <- askErr d (whenText a2 a1) StApp "conflict"
        void (askErr d "{:op :highest :args {:attr :follow/pair :limit 1}}" StApp "bad-argument"))
        `finally` release)

  , ("real: highest-in merges the keys' runs through the door, a page after a page; what cannot be walked is refused", withTmp $ \dir -> do
      let uri = "{:dir " <> T.pack (show dir) <> " :schema {:note/by {:type :string} :note/at {:type :long} :note/by+at {:type :any :tuple [:note/by :note/at]}} :config {:khana "
                <> khana <> "}}"
          idText :: Show a => a -> Text
          idText = T.pack . show
      r <- libConnect library uri
      (d, release) <- either (\f -> throwIO (userError (show f))) pure r
      (do
        let notes = [("a", 10), ("b", 30), ("a", 30), ("c", 20), ("b", 5), ("a", 5)] :: [(Text, Integer)]
        (stw, w) <- ask d ("{:op :write :args {:ops [" <> T.intercalate " "
          [ "[:create [:tmp " <> idText n <> "] :note/by " <> T.pack (show (T.unpack k)) <> "] [:create [:tmp " <> idText n <> "] :note/at " <> idText t <> "]"
          | (n, (k, t)) <- zip [1 :: Int ..] notes ] <> "]}}")
        checkEq "write status" StOk stw
        ids <- case w of
          ReplyOk (EMap ps) | Just (EVec ts) <- lookup (EKw "temps") ps -> pure [ i | EVec [_, EInt i] <- ts ]
          other -> throwIO (userError ("write: " ++ show other))
        checkEq "six ids" 6 (length ids)
        let wanted ks = reverse (sort [ (t, i, k) | ((k, t), i) <- zip notes ids, k `elem` ks ])      -- the largest time, then the largest id, first
            keyedEdn (t, i, k) = EMap [(EKw "key", EStr k), (EKw "val", EInt t), (EKw "id", EInt i)]
            keysText ks = "[" <> T.unwords [ T.pack (show (T.unpack k)) | k <- ks ] <> "]"
            afterText (t, i, _) = " :after {:val " <> idText t <> " :id " <> idText i <> "}"
            page :: [Text] -> Maybe (Integer, Integer, Text) -> IO [Edn]
            page ks after = do
              (st, rep) <- ask d ("{:op :highest-in :args {:attr :note/by+at :keys " <> keysText ks <> " :limit 2" <> maybe "" afterText after <> "}}")
              checkEq "highest-in status" StOk st
              case rep of
                ReplyOk (EVec ps) -> pure ps
                other -> throwIO (userError ("highest-in: " ++ show other))
            ab = wanted ["a", "b"]
        p1 <- page ["a", "b"] Nothing
        checkEq "the first page: the newest of a and b, whoever wrote it" (map keyedEdn (take 2 ab)) p1
        p2 <- page ["a", "b"] (Just (ab !! 1))
        checkEq "the page after it" (map keyedEdn (take 2 (drop 2 ab))) p2
        p3 <- page ["a", "b"] (Just (ab !! 3))
        checkEq "the last page is short" (map keyedEdn (drop 4 ab)) p3
        p4 <- page ["a", "b"] (Just (ab !! 4))
        checkEq "and after it, nothing" [] p4
        checkEq "one key, and a key nobody has" (map keyedEdn (take 2 (wanted ["c"]))) =<< page ["c", "nobody"] Nothing
        checkEq "no keys" [] =<< page [] Nothing
        -- what the library refuses is the caller's: kind bad-argument
        _ <- askErr d "{:op :highest-in :args {:attr :note/by+at :keys [\"a\"] :limit 0}}" StApp "bad-argument"
        _ <- askErr d "{:op :highest-in :args {:attr :nobody/has :keys [\"a\"] :limit 1}}" StApp "bad-argument"
        _ <- askErr d "{:op :highest-in :args {:attr :note/at :keys [1] :limit 1}}" StApp "bad-argument"
        _ <- askErr d "{:op :highest-in :args {:attr :note/by+at :keys [1] :limit 1}}" StApp "bad-argument"
        void (askErr d "{:op :highest-in :args {:attr :note/by+at :keys [\"a\"] :limit 1 :after {:val \"x\" :id 1}}}" StApp "bad-argument"))
        `finally` release)

  , ("real: write with :when through the door; a stale condition is a conflict that names it, and changes nothing", withTmp $ \dir -> do
      let uri = "{:dir " <> T.pack (show dir) <> " :schema {:person/name {:type :string :unique :identity} :person/age {:type :long}} :config {:khana " <> khana <> "}}"
      r <- libConnect library uri
      (d, release) <- either (\f -> throwIO (userError (show f))) pure r
      (do
        (st, w) <- ask d "{:op :write :args {:ops [[:create [:tmp 1] :person/name \"Frege\"] [:create [:tmp 1] :person/age 70]]}}"
        checkEq "write status" StOk st
        frege <- case w of
          ReplyOk (EMap ps) | Just (EVec [EVec [_, EInt i]]) <- lookup (EKw "temps") ps -> pure (T.pack (show i))
          other -> throwIO (userError ("write: " ++ show other))
        let age n = "{:op :write :args {:ops [[:create " <> frege <> " :person/age " <> n <> "]] :when [[:holds " <> frege <> " :person/age 70]]}}"
            ageNow = ask d ("{:op :get-entities :args {:ids [" <> frege <> "]}}")
        (st1, _) <- ask d (age "71")
        checkEq "the current value: written" StOk st1
        m <- askErr d (age "99") StApp "conflict"
        checkEq "the stale one is named" (edn ("[[:holds " <> frege <> " :person/age 70]]")) (edn m)
        (_, now) <- ageNow
        case now of
          ReplyOk (EVec [EMap a]) -> checkEq "the age is 71" (Just (EVec [EInt 71])) (lookup (EKw "person/age") a)
          other -> throwIO (userError ("get-entities: " ++ show other))
        m2 <- askErr d ("{:op :write :args {:ops [[:create " <> frege <> " :person/age 5]] :when [[:unset " <> frege <> " :person/name]]}}") StApp "conflict"
        checkEq "Unset on an attribute that has a value" (edn ("[[:unset " <> frege <> " :person/name]]")) (edn m2)
        askOk d "{:op :verify}" "[]")
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
