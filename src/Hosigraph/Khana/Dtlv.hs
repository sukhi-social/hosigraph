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
  , openDtlv, attachDtlv, closeDtlv, dtlvTransact, dtlvEntity, dtlvDatoms, dtlvDatomPage, dtlvDatomCount, dtlvHighest, dtlvHighestIn, dtlvQueryCells
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

-- | Like 'openDtlv', without the first, empty operation: for a database that is only looked at (the look is its
-- first operation, so nothing is spent on a start that only checks), and let go of straight after.
attachDtlv :: Runner -> FilePath -> Schema -> IO Dtlv
attachDtlv runner dir schema = do
  createDirectoryIfMissing True dir
  absDir <- makeAbsolute dir
  pure (Dtlv runner absDir schema)

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
  void (runWithC True d (Just ("[" <> T.unwords forms <> "]")) ("(" <> transactFn d <> " c data) nil"))

opEdn :: DtlvOp -> Either String Text
opEdn op = case op of
  DAdd e a v     -> form ":db/add" e a v
  DRetract e a v -> form ":db/retract" e a v
  where form h e a v = (\vt -> "[" <> h <> " " <> T.pack (show e) <> " " <> ednKw a <> " " <> vt <> "]") <$> ednVal v

-- | Every attribute and value of one entity, values in index order.
dtlvEntity :: Dtlv -> EntityId -> IO (M.Map Keyword [Value])
dtlvEntity d e = entityOf =<< readReplyC d (Just (T.pack (show e))) "(mapv (fn [x] [(:a x) (:v x)]) (datoms @c :eav data))"

-- | Many entities in one round trip (the answers are in the order asked; a repeated id is asked once
-- per place it appears).  The ids travel as data, so a long list is read, not compiled.
dtlvEntities :: Dtlv -> [EntityId] -> IO [M.Map Keyword [Value]]
dtlvEntities _ [] = pure []
dtlvEntities d es = do
  e <- readReplyC d (Just ("[" <> T.unwords (map (T.pack . show) es) <> "]"))
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
          body = "(let [skip " <> derived <> " " <> grabFn <> " ops (vec (mapcat (fn [[lo hi]] (map (fn [x] [:db/retract (:e x) (:a x) (:v x)]) "
                 <> "(remove (fn [x] (contains? skip (:a x))) (grab lo hi)))) data))] "
                 <> (if lastGroup then "(" <> tx <> " c (conj ops " <> extraForm <> "))" else "(when (seq ops) (" <> tx <> " c ops))")
                 <> " (count ops))"
      r <- readReplyWith d dat body
      case r of
        EInt k -> pure k
        _      -> bad ("a count of retracted facts: " ++ T.unpack (T.take 200 (renderEdn r))))
  where
    tx = transactFn d
    -- The facts of the ids lo..hi, a page at a time.  @seek-datoms@ without a count makes every fact after @lo@ before
    -- @take-while@ sees one (162 ms for one range in 20,000 entities, 0.4 ms with a count, and the heap grows with it:
    -- the same trap 'dtlvDatomPage' tells of), so the count is given: a page is read, and it stops at the first fact
    -- past @hi@ or at the end of the data.  A full page may end inside an entity: that entity goes to the next page,
    -- which starts at it; one entity with more facts than a page makes the page twice as big.  Pages grow as long as the
    -- range goes on.
    grabFn = T.unwords
      [ "grab (fn [lo hi] (loop [from lo n 512 out []]"
      , "(let [xs (vec (seek-datoms @c :eav from nil nil n))"
      , "inr (vec (take-while (fn [x] (<= (:e x) hi)) xs))]"
      , "(if (or (< (count inr) (count xs)) (< (count xs) n))"
      , "(into out inr)"
      , "(let [le (:e (peek inr)) whole (vec (take-while (fn [x] (< (:e x) le)) inr))]"
      , "(if (empty? whole) (recur from (* 2 n) out) (recur le (min 8192 (* 2 n)) (into out whole))))))))" ]
    -- a tuple is made by the khana from its parts: it goes when they go, and Datalevin refuses to take it out by itself
    derived = "#{" <> T.unwords [ ednKw k | (k, a) <- M.toList (dtlvSchema d), not (null (asTuple a)) ] <> "}"
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

-- | A page of the facts: whole entities only, at most @n@ facts (more, if a single entity has more), in the order of
-- the index.  It starts at entity @lo@ (the first entity there is, for 'Nothing') and returns where the next page starts
-- ('Nothing': this one reached the end).  A seek in the index that is given its count: Datalevin reads only that many
-- (without the count, @datoms@ and @seek-datoms@ make every fact after the start before @take@ sees one: 4GB of heap, on real data).
-- Two pages from different databases that start at the same entity and stop at the same entity hold the same facts
-- if the databases agree; that is what lets "Hosigraph.verify" compare two khanas without holding either of them.
dtlvDatomPage :: Dtlv -> Maybe EntityId -> Int -> IO ([Datom], Maybe EntityId)
dtlvDatomPage d lo n0 = go (max 1 n0)
  where
    from = "(seek-datoms @c :eav " <> maybe "nil" (T.pack . show) lo <> " nil nil "
    go n = do
      rows <- asRows =<< readReply d ("(mapv (fn [x] [(:e x) (:a x) (:v x)]) " <> from <> T.pack (show (n + 1)) <> "))")
      ds <- forM rows $ \r -> case r of
        [EInt e, EKw a, v] -> Datom (fromInteger e) (Keyword a) <$> value v
        _                  -> bad ("a datom row: " ++ show r)
      case drop n ds of
        []      -> pure (ds, Nothing)                       -- fewer than n + 1: that was all of it
        (x : _) ->
          -- the (n + 1)th fact opens an entity that may have begun before it: that entity goes to the next page
          case takeWhile ((< dE x) . dE) ds of
            []   -> go (n * 2)                              -- one entity with more than n facts: ask for a bigger page
            page -> pure (page, Just (dE x))

-- | The facts of one attribute from the high end of its index: at most @n@, the largest value first and, among equal
-- values, the largest entity id first.  With @Just (v, e)@ it starts after that place (the fact @(e, v)@ itself is not
-- in the answer), so the last fact of one answer is the cursor of the next.
--
-- It is Datalevin's @rseek-datoms@ on @:ave@ with a count: it reads about @n@ facts and not every fact of the attribute.
-- Datalevin's bound on the entity id applies to every value of the range and not only to the first (tried), so the
-- start is given as the value alone, and the facts at that value that the cursor has passed are dropped here, in the
-- same round trip and from the same snapshot; when the ties at the cursor's value are more than the count, it asks again for
-- twice as many.  The order is the one the index keeps for the attribute's declared type (a number by size, a string by
-- its bytes); an attribute with no declared type is in no order anyone could use, and the caller refuses it.
dtlvHighest :: Dtlv -> Keyword -> Maybe (Value, EntityId) -> Int -> IO [Datom]
dtlvHighest d a cur n0 = do
  (from, skipE, m0) <- case cur of
    Nothing     -> pure ("nil", "nil", n)
    Just (v, e) -> do
      vt <- either bad pure (ednVal v)
      pure (vt, T.pack (show e), n + 1)
  -- the body is the same text at every call (what changes travels as data), so the resident JVM compiles it once
  rows <- asRows =<< readReplyC d
    (Just ("{:a " <> ednKw a <> " :from " <> from <> " :e " <> skipE <> " :n " <> T.pack (show n) <> " :m0 " <> T.pack (show m0) <> "}"))
    (T.unwords
    [ "(let [{:keys [a from e n m0]} data db @c"
    , "skip? (if e (fn [x] (and (= (:v x) from) (>= (:e x) e))) (fn [_] false))]"
    , "(loop [m m0]"
    , "(let [rows (vec (rseek-datoms db :ave a from nil m))"
    , "kept (vec (drop-while skip? rows))]"
    , "(if (or (>= (count kept) n) (< (count rows) m))"
    , "(mapv (fn [x] [(:e x) (:a x) (:v x)]) (take n kept))"
    , "(recur (* 2 m))))))" ])
  forM rows $ \r -> case r of
    [EInt e, EKw k, v] -> Datom (fromInteger e) (Keyword k) <$> value v
    _                  -> bad ("a datom row: " ++ show r)
  where n = max 1 n0

-- | The facts of a two-part tuple attribute @(key, order)@ from the high end of the order part, for the given keys only:
-- at most @n@, the largest order value first and, among equal values, the largest entity id first, whichever key they
-- are under.  With @Just (v, e)@ it starts after that place, as 'dtlvHighest' does.  The facts are the tuple's: their
-- value is @VTuple [key, order]@.
--
-- It does not read the facts of the keys it is given; it reads the head of each key's run in the tuple's index (a seek
-- each) and merges the runs lazily, taking the next of a key only when that key's head was taken, so the cost is about
-- one seek per key plus one per fact answered, and does not depend on how the facts are spread among the keys.  (A walk
-- from the top of the order that looks each fact's key up has no such bound: it reads every fact newer than the @n@th
-- one that passes, and when the keys are quiet that is all of them.)
--
-- The tuple's index puts a key's facts together in order of the order part (tried with keys that are prefixes of one
-- another, and not ASCII).  The order part has to be a long, because the walk of a key starts at the largest one.
-- An entity that has the key and not the order part has a half-made tuple, @[key nil]@, at the low end of that key's
-- run: it has no place in the order, and the walk of the key ends there.
-- Facts that tie (the same key and order, which a tuple that is not unique allows) are told apart by the entity id, so
-- the next fact of a key is found by asking for the facts at the same order value again and dropping the ones taken,
-- never by "the order value minus one", which loses the ties.
dtlvHighestIn :: Dtlv -> Keyword -> [Value] -> Maybe (Value, EntityId) -> Int -> IO [Datom]
dtlvHighestIn d a keys cur n0 = do
  ks <- either bad (pure . T.unwords) (mapM ednVal keys)
  (hi, skipE) <- case cur of
    Nothing     -> pure ("9223372036854775807", "nil")
    Just (v, e) -> do
      vt <- either bad pure (ednVal v)
      pure (vt, T.pack (show e))
  rows <- asRows =<< readReplyC d
    (Just ("{:a " <> ednKw a <> " :n " <> T.pack (show n) <> " :hi " <> hi <> " :skip " <> skipE <> " :ks [" <> ks <> "]}"))
    (T.unwords
    [ "(let [db @c {:keys [a n ks]} data hi0 (:hi data) skip0 (:skip data)"
    , "desc (fn [x y] (compare [(nth y 0) (nth y 1)] [(nth x 0) (nth x 1)]))"
    , "head (fn [k hi skip-e]"
    , "(loop [m 2]"
    , "(let [rs (vec (rseek-datoms db :ave a [k hi] nil m))"
    , "x (first (drop-while (fn [x] (and skip-e (= k (first (:v x))) (= (second (:v x)) hi) (>= (:e x) skip-e))) rs))]"
    , "(cond (and x (= k (first (:v x))) (some? (second (:v x)))) [(second (:v x)) (:e x) k]"
    , "x nil"
    , "(< (count rs) m) nil"
    , ":else (recur (* 2 m))))))]"
    , "(loop [pq (into (sorted-set-by desc) (keep (fn [k] (head k hi0 skip0)) (distinct ks))) out []]"
    , "(if (or (= (count out) n) (empty? pq))"
    , "out"
    , "(let [top (first pq) [o e k] top nx (head k o e) pq (disj pq top)]"
    , "(recur (if nx (conj pq nx) pq) (conj out [e a [k o]]))))))" ])
  forM rows $ \r -> case r of
    [EInt e, EKw k, v] -> Datom (fromInteger e) (Keyword k) <$> value v
    _                  -> bad ("a datom row: " ++ show r)
  where n = max 1 n0

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
  (shape, dat, body) <- prepareQueryD txt inputs
  e     <- readReplyC d (Just dat) body
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

-- | 'prepareQuery' for one query against @c@: the code is the same for the same query text, and the inputs go as
-- data, so the resident JVM compiles it once.
prepareQueryD :: Text -> [Input] -> IO (FindShape, Text, Text)
prepareQueryD txt inputs = do
  shape <- either (throwIO . DtlvError . ("query: " ++)) (pure . qShape) (readEdn txt >>= parseQuery)
  args  <- either (throwIO . DtlvError) pure (mapM inputEdn inputs)
  -- @q@ stays in the call position, with each input as @(nth data i)@: the native @dtlv exec@ takes @q@ only there
  -- (@(apply q ...)@ or @(let [f q] ...)@ is "Unable to resolve symbol: q"), and the text is still the same for the
  -- same query and the same number of inputs.
  pure (shape, "[" <> T.unwords args <> "]", "(q '" <> txt <> " @c" <> T.concat [ " (nth data " <> T.pack (show i) <> ")" | i <- [0 .. length args - 1] ] <> ")")

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
runWith = runWithC False

-- | 'runWith' for a body whose text is the same at every call (what changes travels as @data@).  The resident JVM
-- compiles such a body once and keeps the function; Clojure's @eval@ makes new classes at every call, and that was most
-- of what a small call cost (1.5 ms against 0.05 ms for the same work in a function compiled before).  The native
-- @dtlv@ runs a script and has nothing to keep, so it ignores the flag.
runWithC :: Bool -> Dtlv -> Maybe Text -> Text -> IO Text
runWithC cached d dat body = case dtlvRunner d of
  Native exe  -> runNative exe d dat body
  Resident sc -> runResident cached sc d dat body

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

runResident :: Bool -> Sidecar -> Dtlv -> Maybe Text -> Text -> IO Text
runResident cached sc d dat body = do
  line <- sidecarRequest sc request `catch` \(SidecarError m) -> throwIO (DtlvError m)
  case readEdn (tameEdn line) of
    Right (EMap kvs)
      | Just (EStr s) <- lookup (EKw "ok") kvs  -> pure s
      | Just (EStr m) <- lookup (EKw "err") kvs -> throwIO (DtlvError (T.unpack m))
    _ -> throwIO (DtlvError ("an answer we did not expect: " ++ T.unpack (T.take 400 line)))
  where
    -- the attributes whose words are searched: the JVM gives each of them its analyzer ('Hosigraph.Khana.Sidecar.sidecarSource')
    search = case fulltextAttrs (dtlvSchema d) of
      [] -> ""
      ks -> " :search [" <> T.unwords [ ednStr (kwText k) | k <- ks ] <> "]"
    request = "{:dir " <> ednStr (T.pack (dtlvDir d)) <> " :schema " <> ednStr (schemaEdn (dtlvSchema d))
              <> " :opts " <> walOpts <> search <> " :body " <> ednStr body <> maybe "" (\x -> " :data " <> x) dat <> (if cached then " :cache true" else "") <> "}"

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

-- | 'readReplyWith'' for a body that does not change from call to call (see 'runWithC').
readReplyC :: Dtlv -> Maybe Text -> Text -> IO Edn
readReplyC d dat body = do
  s <- runWithC True d dat body
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
  VTuple xs -> (\ps -> "[" <> T.unwords ps <> "]") <$> mapM (maybe (Right "nil") ednVal) xs

-- | The attributes of a schema whose words are searched.
fulltextAttrs :: Schema -> [Keyword]
fulltextAttrs sch = [ k | (k, a) <- M.toList sch, asFulltext a ]

-- | The schema as a Datalevin schema map.  Our @TAny@ is an attribute with no declared type.
schemaEdn :: Schema -> Text
schemaEdn sch = "{" <> T.intercalate " " [ ednKw k <> " " <> one s | (k, s) <- M.toList sch ] <> "}"
  where
    one (AttrSchema t c u ps ft) = "{" <> T.unwords (catMaybes [typ t, card c, uniq u, parts ps, words_ ft]) <> "}"
    words_ ft = if ft then Just ":db/fulltext true :db.fulltext/autoDomain true" else Nothing
    parts ps = if null ps then Nothing else Just (":db/tupleAttrs [" <> T.unwords (map ednKw ps) <> "]")
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
