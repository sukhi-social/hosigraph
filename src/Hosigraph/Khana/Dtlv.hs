-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Khana.Dtlv
-- Description : A khana kept in a Datalevin database, reached through the @dtlv@ command-line tool.
--
-- Datalevin ships a native command, @dtlv@.  @dtlv exec@ reads Clojure code from standard
-- input, runs it against Datalevin, and prints the value of every form it evaluated.  This
-- module uses exactly that, one process per operation:
--
-- > (let [c (get-conn "<dir>" <schema>)]
-- >   (try [:ok (binding [*print-namespace-maps* false] (pr-str (do <body>)))]
-- >        (catch Exception e [:err (str (ex-message e))])
-- >        (finally (close c))))
--
-- * The answer is one EDN vector on the last line: @[:ok "<what the body returned>"]@ or
--   @[:err "<message>"]@.  Because the body's value is turned into text /inside/ the @try@,
--   lazy results (a pull, say) are read before the connection is closed.
-- * A write is one @transact!@, so it keeps the "all of it goes in, or none of it" promise.
-- * With the native @dtlv@ there is no long-lived process and nothing to shut down: 'Dtlv' is
--   just a path, a directory and a schema.  (A start costs a couple of hundred milliseconds.)
-- * The same bodies can instead go to a resident JVM that runs the Datalevin standalone jar
--   ("Hosigraph.Khana.Sidecar"): a 'Runner' decides.  The JVM keeps the connection open, so an
--   operation costs well under a millisecond, and it does not share @dtlv exec@'s habit of
--   resolving the aggregates @max@ / @min@ / @distinct@ to @clojure.core@'s.
--
-- This module does not depend on "Hosigraph.Khana"; it only knows facts, schemas and queries.
module Hosigraph.Khana.Dtlv
  ( Dtlv(..), Runner(..), DtlvError(..), DtlvOp(..)
  , openDtlv, closeDtlv, dtlvTransact, dtlvEntity, dtlvDatoms, dtlvDatomCount, dtlvQueryCells
    -- * Many in one round trip
  , dtlvMulti, dtlvEntities, dtlvEntityAndCount, dtlvQueryMany
    -- * Taking out whole entities
  , dtlvRetractRanges
    -- * A new file
  , dtlvCompactTo
    -- * Text we send and read (pure; exposed for the tests)
  , ednStr, ednKw, ednVal, schemaEdn, tameEdn
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as B
import           Data.Char (isAlpha, isAlphaNum, ord)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           Numeric (showHex)
import           System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import           System.IO (hClose)
import           System.Process

import           Takka.Edn
import           Takka.Syntax (Cell(..), FindShape(..), Input(..), Pulled(..), Query(..), parseQuery)
import           Takka.Types
import           Hosigraph.Khana.Sidecar

-- | Anything that went wrong while talking to @dtlv@ (including Datalevin refusing a transaction).
newtype DtlvError = DtlvError String

instance Show DtlvError where show (DtlvError s) = "dtlv: " ++ s

instance Exception DtlvError

-- | Who runs the code we send.
data Runner
  = Native FilePath         -- ^ the @dtlv@ executable: one process per operation
  | Resident Sidecar        -- ^ a JVM that stays up (shared by every database that uses it)

-- | A Datalevin database: who runs it, the directory, and the schema it is opened with.
data Dtlv = Dtlv
  { dtlvRunner :: Runner
  , dtlvDir    :: FilePath      -- ^ absolute
  , dtlvSchema :: Schema
  }

-- | One fact to put in or take out.
data DtlvOp
  = DAdd     !EntityId !Keyword !Value
  | DRetract !EntityId !Keyword !Value

-- | Open (creating if needed) the database in a directory.  Running a first, empty
-- operation here finds out early whether @dtlv@ starts and whether the directory is usable.
openDtlv :: Runner -> FilePath -> Schema -> IO Dtlv
openDtlv runner dir schema = do
  createDirectoryIfMissing True dir
  absDir <- makeAbsolute dir
  let d = Dtlv runner absDir schema
  _ <- run d "nil"
  pure d

-- | Let go of the database.  With a resident JVM the connection is closed (so the directory can be
-- moved or removed); the JVM itself stays up for the other databases.  With the native @dtlv@
-- nothing stays open between operations.
closeDtlv :: Dtlv -> IO ()
closeDtlv d = case dtlvRunner d of
  Native _     -> pure ()
  Resident sc  -> void (sidecarRequestIfRunning sc ("{:op :drop :dir " <> ednStr (T.pack (dtlvDir d)) <> "}"))
                    `catch` \(_ :: SidecarError) -> pure ()

-- | Apply facts in one atomic transaction (in the order given).
dtlvTransact :: Dtlv -> [DtlvOp] -> IO ()
dtlvTransact _ [] = pure ()
dtlvTransact d ops = do
  forms <- either (throwIO . DtlvError) pure (mapM opEdn ops)
  void (runWith d (Just ("[" <> T.unwords forms <> "]")) ("(" <> transactFn d <> " c data) nil"))

opEdn :: DtlvOp -> Either String Text
opEdn op = case op of
  DAdd e a v     -> form ":db/add" e a v
  DRetract e a v -> form ":db/retract" e a v
  where form h e a v = (\vt -> "[" <> h <> " " <> T.pack (show e) <> " " <> ednKw a <> " " <> vt <> "]") <$> ednVal v

-- | Every attribute and value of one entity, values in index order.
dtlvEntity :: Dtlv -> EntityId -> IO (M.Map Keyword [Value])
dtlvEntity d e = entityOf =<< readReply d ("(mapv (fn [x] [(:a x) (:v x)]) (datoms @c :eav " <> T.pack (show e) <> "))")

-- | Many entities in one round trip (the answers are in the order asked; a repeated id is asked once
-- per place it appears).  The ids travel as data, so a long list is read, not compiled.
dtlvEntities :: Dtlv -> [EntityId] -> IO [M.Map Keyword [Value]]
dtlvEntities _ [] = pure []
dtlvEntities d es = do
  e <- readReplyWith d ("[" <> T.unwords (map (T.pack . show) es) <> "]")
         "(mapv (fn [e] (mapv (fn [x] [(:a x) (:v x)]) (datoms @c :eav e))) data)"
  case e of
    EVec xs | length xs == length es -> mapM entityOf xs
    _ -> bad ("entities: " ++ T.unpack (T.take 200 (renderEdn e)))

-- | Take out every fact of every entity whose id is in one of the ranges (inclusive), and put one more
-- fact in with the last of them.  The facts are found by a seek in the index (from the range's start, while
-- the id is still inside), not by a scan, and taken out inside the database, so nothing big travels.
-- Only the entity's /own/ facts go (@:db/retract@ of each): a fact of /another/ entity that holds this id as a
-- value stays, a number that points at nothing (@:db/retractEntity@ would take those too).
--
-- Ranges go in groups, one transaction each; the extra fact is in the last one.  If we die in between, the
-- extra fact is missing: that is how the caller knows to do it again, and taking out a fact that is gone
-- is no error.  Returns the number of facts taken out.
dtlvRetractRanges :: Dtlv -> [(EntityId, EntityId)] -> DtlvOp -> IO Integer
dtlvRetractRanges d ranges extra = do
  extraForm <- either (throwIO . DtlvError) pure (opEdn extra)
  case groupsOf 2000 ranges of
    [] -> dtlvTransact d [extra] >> pure 0
    gs -> sum <$> forM (zip [1 :: Int ..] gs) (\(n, g) -> do
      let lastGroup = n == length gs
          dat  = "[" <> T.unwords [ "[" <> T.pack (show lo) <> " " <> T.pack (show hi) <> "]" | (lo, hi) <- g ] <> "]"
          body = "(let [ops (vec (mapcat (fn [[lo hi]] (map (fn [x] [:db/retract (:e x) (:a x) (:v x)]) "
                 <> "(take-while (fn [x] (<= (:e x) hi)) (seek-datoms @c :eav lo)))) data))] "
                 <> (if lastGroup then "(" <> tx <> " c (conj ops " <> extraForm <> "))" else "(when (seq ops) (" <> tx <> " c ops))")
                 <> " (count ops))"
      r <- readReplyWith d dat body
      case r of
        EInt k -> pure k
        _      -> bad ("a count of retracted facts: " ++ T.unpack (T.take 200 (renderEdn r))))
  where
    tx = transactFn d
    groupsOf _ [] = []
    groupsOf m xs = let (a, b) = splitAt m xs in a : groupsOf m b

-- | Copy the database, /compacted/, into a directory that does not exist yet or is empty.  Datalevin (LMDB) keeps the
-- bytes of a retracted fact in its file until the page is used again; a compacted copy writes only what is alive, so the
-- bytes of what was taken out are not in it.  It is a copy of one snapshot, with no replay.  The destination is not
-- opened or changed afterwards: that is for the caller.
--
-- Native: @dtlv -d DIR -c copy NEW@.  Resident: @(copy (db c) NEW true)@.  The native @dtlv@ says \"Copy error: ...\" and
-- still exits with 0 (when the destination is not empty, say), so the exit code is not believed: the words are looked
-- for, and the copy must hold a @data.mdb@.  A failure is a 'DtlvError'.
dtlvCompactTo :: Dtlv -> FilePath -> IO ()
dtlvCompactTo d dst = do
  absDst <- makeAbsolute dst
  case dtlvRunner d of
    Native exe -> do
      (code, out, err) <- runBytes exe ["-d", dtlvDir d, "-c", "copy", absDst] B.empty
      let said = TE.decodeUtf8With TE.lenientDecode (out <> err)
      when (code /= ExitSuccess || "Copy error" `T.isInfixOf` said) $
        bad ("compact copy: " ++ T.unpack (T.take 400 said))
    Resident _ -> void (run d ("(copy (db c) " <> ednStr (T.pack absDst) <> " true) nil"))
  made <- doesFileExist (absDst </> "data.mdb")
  unless made $ bad "compact copy: the copy has no data.mdb"

-- | One entity's bookkeeping together with the number of datoms: two answers, one round trip, one snapshot.
dtlvEntityAndCount :: Dtlv -> EntityId -> IO (M.Map Keyword [Value], Integer)
dtlvEntityAndCount d e = do
  rs <- dtlvMulti d [ "(mapv (fn [x] [(:a x) (:v x)]) (datoms db :eav " <> T.pack (show e) <> "))"
                    , "(count-datoms db nil nil nil)" ]
  case rs of
    [m, EInt n] -> (\x -> (x, n)) <$> entityOf m
    _           -> bad ("an entity and a count: " ++ T.unpack (T.take 200 (renderEdn (EVec rs))))

entityOf :: Edn -> IO (M.Map Keyword [Value])
entityOf e = do
  rows <- asRows e
  ps <- forM rows $ \r -> case r of
    [EKw a, v] -> (\x -> (Keyword a, [x])) <$> value v
    _          -> bad ("an entity row: " ++ show r)
  pure (M.fromListWith (flip (++)) ps)

-- | Several bodies in one round trip.  Each sees the same snapshot as @db@ (so the answers agree with
-- each other, as they would inside one snapshot of the in-process khana); the answers come back in the order given.
-- If any body fails the whole call fails.
dtlvMulti :: Dtlv -> [Text] -> IO [Edn]
dtlvMulti _ [] = pure []
dtlvMulti d bodies = do
  e <- readReply d ("(let [db @c] [" <> T.unwords bodies <> "])")
  case e of
    EVec xs | length xs == length bodies -> pure xs
    _ -> bad ("a batch answer of the wrong shape: " ++ T.unpack (T.take 200 (renderEdn e)))

-- | Every fact in the database.
dtlvDatoms :: Dtlv -> IO [Datom]
dtlvDatoms d = do
  rows <- asRows =<< readReply d "(mapv (fn [x] [(:e x) (:a x) (:v x)]) (datoms @c :eav))"
  forM rows $ \r -> case r of
    [EInt e, EKw a, v] -> Datom (fromInteger e) (Keyword a) <$> value v
    _                  -> bad ("a datom row: " ++ show r)

dtlvDatomCount :: Dtlv -> IO Integer
dtlvDatomCount d = do
  e <- readReply d "(count-datoms @c nil nil nil)"
  case e of
    EInt n -> pure n
    _      -> bad ("a count: " ++ show e)

-- | A Datalog query written as EDN, run by Datalevin.  The rows have the same shape as the in-process khana's:
-- a list of rows, also for @:find ?x .@ (one row, or none) and @:find [?x ...]@ (one cell per row).
-- A map in a cell is a pull result.  'InRules' cannot be sent (see below).
dtlvQueryCells :: Dtlv -> Text -> [Input] -> IO [[Cell]]
dtlvQueryCells d txt inputs = do
  (shape, call) <- prepareQuery "@c" txt inputs
  e     <- readReply d call
  rows  <- either (throwIO . DtlvError) pure (shapeRows shape e)
  forM rows (mapM cell)

-- | Many queries in one round trip, all against one snapshot.  Everything that can be refused here
-- (a query our parser rejects, a rule set, a value with no EDN spelling) is refused /before/
-- anything is sent.  If Datalevin itself rejects one, the whole call fails.
dtlvQueryMany :: Dtlv -> [(Text, [Input])] -> IO [[[Cell]]]
dtlvQueryMany _ [] = pure []
dtlvQueryMany d qs = do
  prepared <- mapM (uncurry (prepareQuery "db")) qs
  es <- dtlvMulti d (map snd prepared)
  forM (zip (map fst prepared) es) $ \(shape, e) -> do
    rows <- either (throwIO . DtlvError) pure (shapeRows shape e)
    forM rows (mapM cell)

-- | The shape of a query's answer, and the code that asks it (against the database named by the first argument).
prepareQuery :: Text -> Text -> [Input] -> IO (FindShape, Text)
prepareQuery dbName txt inputs = do
  shape <- either (throwIO . DtlvError . ("query: " ++)) (pure . qShape) (readEdn txt >>= parseQuery)
  args  <- either (throwIO . DtlvError) pure (mapM inputEdn inputs)
  pure (shape, "(q '" <> txt <> " " <> dbName <> T.concat (map (" " <>) args) <> ")")

cell :: Edn -> IO Cell
cell x = case x of
  EMap _ -> either (throwIO . DtlvError) (pure . CPull) (pulled x)
  _      -> CVal <$> value x

inputEdn :: Input -> Either String Text
inputEdn i = case i of
  InScalar v -> ednVal v
  InColl vs  -> vec <$> mapM ednVal vs
  InRel rs   -> vec <$> mapM (fmap vec . mapM ednVal) rs
  InRules _  -> Left "rules (InRules) cannot be sent to dtlv: write them into the query instead"
  where vec xs = "[" <> T.unwords xs <> "]"

shapeRows :: FindShape -> Edn -> Either String [[Edn]]
shapeRows shape e = case shape of
  Rel    -> items e >>= mapM row
  Scalar -> pure (case e of ENil -> []; x -> [[x]])
  Coll   -> map (: []) <$> items e
  where
    items x = case x of
      ESet xs  -> Right xs
      EVec xs  -> Right xs
      EList xs -> Right xs
      ENil     -> Right []
      _        -> Left ("a query result that is not a collection: " ++ T.unpack (renderEdn x))
    row x = case x of
      EVec xs  -> Right xs
      EList xs -> Right xs
      _        -> Left ("a result row that is not a tuple: " ++ T.unpack (renderEdn x))

pulled :: Edn -> Either String Pulled
pulled e = case e of
  EMap ps  -> PEntity <$> forM ps (\(k, v) -> case k of
                EKw t -> (,) (Keyword t) <$> pulled v
                _     -> Left ("a pull key that is not a keyword: " ++ T.unpack (renderEdn k)))
  EVec xs  -> PMany <$> mapM pulled xs
  EList xs -> PMany <$> mapM pulled xs
  ESet xs  -> PMany <$> mapM pulled xs
  _        -> PScalar <$> ednToValue e

-- ---------------------------------------------------------------------------
-- Running dtlv

script :: Dtlv -> Maybe Text -> Text -> Text
script d dat body = T.unlines
  [ "(let [c (get-conn " <> ednStr (T.pack (dtlvDir d)) <> " " <> schemaEdn (dtlvSchema d) <> ")"
    <> maybe "" (\x -> " data " <> x) dat <> "]"
  , "  (try [:ok (binding [*print-namespace-maps* false] (pr-str (do " <> body <> ")))]"
  , "       (catch Exception e [:err (str (ex-message e))])"
  , "       (finally (close c))))" ]

-- | How the resident JVM opens a khana: with Datalevin's write-ahead log, in its /relaxed/ durability profile.
--
-- The khana can be built again from the events (and the folded seq is in the same transaction as the facts, so a
-- khana that lost its last few writes is caught up when it opens), so it does not need to wait for LMDB's own sync
-- at every write; the tail log is the one that is fsynced.  (Measured: one write per transaction, about 3 times
-- faster.)  The segments and the retention are kept small so the log does not grow on the disk.
--
-- /What this changes for erase./ With the log, the bytes of a retracted fact stay in @txlog\/@ (and in
-- @snapshots\/@) until the khana is made a new file; without it they stay in @data.mdb@ until LMDB happens to reuse the page.
-- 'dtlvCompactTo' copies neither, so @clean@ still leaves them nowhere.
--
-- Only the resident JVM gets this.  The native @dtlv@ is one process per operation and is left as it was.
walOpts :: Text
walOpts = "{:wal? true :wal-durability-profile :relaxed :wal-segment-max-bytes 4194304 :wal-retention-bytes 8388608 :wal-retention-ms 60000}"

-- | The function that applies a transaction and comes back when it is done.  @transact-ack!@ does the same as
-- @transact!@ and does not build the report we throw away.  (Resident only; the native @dtlv@ keeps @transact!@.)
transactFn :: Dtlv -> Text
transactFn d = case dtlvRunner d of
  Resident _ -> "transact-ack!"
  Native _   -> "transact!"

-- | Run a body with the connection @c@ in scope; return the text of its value.
run :: Dtlv -> Text -> IO Text
run d = runWith d Nothing

-- | Like 'run', with some EDN data the body sees as @data@.  (Under the native @dtlv@ it is written
-- into the code; with the resident JVM it travels as data, so a big one is not compiled.)
runWith :: Dtlv -> Maybe Text -> Text -> IO Text
runWith d dat body = case dtlvRunner d of
  Native exe  -> runNative exe d dat body
  Resident sc -> runResident sc d dat body

runNative :: FilePath -> Dtlv -> Maybe Text -> Text -> IO Text
runNative exe d dat body = do
  (code, out, err) <- runBytes exe ["exec"] (TE.encodeUtf8 (script d dat body))
  let outT   = TE.decodeUtf8With TE.lenientDecode out
      errT   = TE.decodeUtf8With TE.lenientDecode err
      lastLn = case filter (not . T.null . T.strip) (T.lines outT) of [] -> ""; ls -> last ls
  case readEdn (tameEdn lastLn) of
    Right (EVec [EKw "ok", EStr s])  -> pure s
    Right (EVec [EKw "err", EStr m]) -> throwIO (DtlvError (T.unpack m))
    _ -> throwIO (DtlvError ("an answer we did not expect (" ++ describe code ++ "): " ++ T.unpack (T.take 400 (outT <> errT))))
  where describe code = "exit " ++ show (code :: ExitCode)

runResident :: Sidecar -> Dtlv -> Maybe Text -> Text -> IO Text
runResident sc d dat body = do
  line <- sidecarRequest sc request `catch` \(SidecarError m) -> throwIO (DtlvError m)
  case readEdn (tameEdn line) of
    Right (EMap kvs)
      | Just (EStr s) <- lookup (EKw "ok") kvs  -> pure s
      | Just (EStr m) <- lookup (EKw "err") kvs -> throwIO (DtlvError (T.unpack m))
    _ -> throwIO (DtlvError ("an answer we did not expect: " ++ T.unpack (T.take 400 line)))
  where
    request = "{:dir " <> ednStr (T.pack (dtlvDir d)) <> " :schema " <> ednStr (schemaEdn (dtlvSchema d))
              <> " :opts " <> walOpts <> " :body " <> ednStr body <> maybe "" (\x -> " :data " <> x) dat <> "}"

-- | Run a body and read its value as EDN.
readReply :: Dtlv -> Text -> IO Edn
readReply d = readReplyWith' d Nothing

-- | Like 'readReply', with data the body sees as @data@.
readReplyWith :: Dtlv -> Text -> Text -> IO Edn
readReplyWith d dat = readReplyWith' d (Just dat)

readReplyWith' :: Dtlv -> Maybe Text -> Text -> IO Edn
readReplyWith' d dat body = do
  s <- runWith d dat body
  either (\m -> bad ("cannot read dtlv's answer: " ++ m)) pure (readEdn (tameEdn s))

asRows :: Edn -> IO [[Edn]]
asRows e = case e of
  EVec xs -> forM xs $ \x -> case x of
    EVec r -> pure r
    _      -> bad ("a row that is not a vector: " ++ T.unpack (renderEdn x))
  _ -> bad ("rows expected: " ++ T.unpack (renderEdn e))

value :: Edn -> IO Value
value = either bad pure . ednToValue

bad :: String -> IO a
bad = throwIO . DtlvError

-- | Run a process with bytes on standard input; collect standard output and standard error.
-- (Both are drained in their own threads, and the input is written meanwhile, so a chatty
-- child cannot make us wait forever.)
runBytes :: FilePath -> [String] -> B.ByteString -> IO (ExitCode, B.ByteString, B.ByteString)
runBytes exe args input = do
  (Just hin, Just hout, Just herr, ph) <-
    createProcess (proc exe args) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  outV <- newEmptyMVar
  errV <- newEmptyMVar
  let drain h v = void . forkIO $ (B.hGetContents h `catch` \(_ :: IOException) -> pure B.empty) >>= putMVar v
  drain hout outV
  drain herr errV
  (B.hPut hin input >> hClose hin) `catch` \(_ :: IOException) -> pure ()    -- the child may leave early
  out  <- takeMVar outV
  err  <- takeMVar errV
  code <- waitForProcess ph
  pure (code, out, err)

-- ---------------------------------------------------------------------------
-- EDN we write

-- | A string literal that Clojure's reader accepts (and our own EDN reader, for tests).
ednStr :: Text -> Text
ednStr t = "\"" <> T.concatMap esc t <> "\""
  where
    esc c
      | c == '"'  = "\\\""
      | c == '\\' = "\\\\"
      | c == '\n' = "\\n"
      | c == '\r' = "\\r"
      | c == '\t' = "\\t"
      | c < ' ' || c == '\DEL' = T.pack ("\\u" ++ replicate (4 - length h) '0' ++ h)
      | otherwise = T.singleton c
      where h = showHex (ord c) ""

-- | A keyword.  Plain names are written as @:ns/name@; anything unusual (full IRIs for JSON-LD, say)
-- goes through @(keyword "...")@, which the reader never refuses.
ednKw :: Keyword -> Text
ednKw (Keyword t)
  | plain t   = ":" <> t
  | otherwise = "(keyword " <> ednStr t <> ")"
  where
    plain s = case T.splitOn "/" s of
      [a]    -> part a
      [a, b] -> part a && part b
      _      -> False
    part p = case T.uncons p of
      Nothing        -> False
      Just (c, rest) -> (isAlpha c || c == '_') && T.all (\x -> isAlphaNum x || x `elem` ("._-*+!?<>=$%&" :: String)) rest

-- | A value.  NaN and the infinities have no EDN spelling and are refused.
ednVal :: Value -> Either String Text
ednVal v = case v of
  VInt n    -> Right (T.pack (show n))
  VDouble x | isNaN x || isInfinite x -> Left ("cannot store " ++ show x)
            | otherwise               -> Right (T.pack (show x))
  VStr s    -> Right (ednStr s)
  VBool b   -> Right (if b then "true" else "false")
  VKw k     -> Right (ednKw k)

-- | The schema as a Datalevin schema map.  Our @TAny@ is an attribute with no declared type.
schemaEdn :: Schema -> Text
schemaEdn sch = "{" <> T.intercalate " " [ ednKw k <> " " <> one s | (k, s) <- M.toList sch ] <> "}"
  where
    one (AttrSchema t c u) = "{" <> T.unwords (catMaybes [typ t, card c, uniq u]) <> "}"
    typ t = case t of
      TAny     -> Nothing
      TString  -> Just ":db/valueType :db.type/string"
      TLong    -> Just ":db/valueType :db.type/long"
      TDouble  -> Just ":db/valueType :db.type/double"
      TBool    -> Just ":db/valueType :db.type/boolean"
      TKeyword -> Just ":db/valueType :db.type/keyword"
      TRef     -> Just ":db/valueType :db.type/ref"
    card c = case c of One -> Nothing; Many -> Just ":db/cardinality :db.cardinality/many"
    uniq u = case u of
      NotUnique      -> Nothing
      UniqueIdentity -> Just ":db/unique :db.unique/identity"
      UniqueValue    -> Just ":db/unique :db.unique/value"

-- | Clojure's printer writes a form feed and a backspace inside a string as @\\f@ and @\\b@, which
-- our small EDN reader does not know.  Spell those two as @\\uXXXX@ (and leave every other
-- escape, including @\\\\f@, alone).
tameEdn :: Text -> Text
tameEdn t
  | T.isInfixOf "\\f" t || T.isInfixOf "\\b" t = T.pack (go (T.unpack t))
  | otherwise                                   = t          -- nothing to spell differently: no copy (a big dump is mostly this)
  where
    go s = case s of
      ('\\' : 'f' : r) -> "\\u000c" ++ go r
      ('\\' : 'b' : r) -> "\\u0008" ++ go r
      ('\\' : c : r)   -> '\\' : c : go r
      (c : r)          -> c : go r
      []               -> []
