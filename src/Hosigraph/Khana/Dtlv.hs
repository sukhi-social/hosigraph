-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Khana.Dtlv
-- Description : A khana kept in a Datalevin database, reached through a resident @dtlv repl@ or a resident JVM.
--
-- Datalevin ships a native command, @dtlv@.  Its @repl@ reads Clojure code from standard input and stays up
-- ("Hosigraph.Khana.Repl"), and the same code can go to a resident JVM that runs the Datalevin standalone jar
-- ("Hosigraph.Khana.Sidecar"): a 'Runner' decides.  (@dtlv exec@, one process for each operation, is not used any more.)
-- One operation is a body in a form like
--
-- > (let [c (conn! "<dir>" <schema> <opts>)]
-- >   (try [:ok (binding [*print-namespace-maps* false] (pr-str (do <body>)))]
-- >        (catch Exception e [:err (str (ex-message e))])))
--
-- * The answer is one EDN vector on the last line: @[:ok "<what the body returned>"]@ or
--   @[:err "<message>"]@.  Because the body's value is turned into text /inside/ the @try@,
--   lazy results (a pull, say) are read before anything is let go of.
-- * A write is one transaction, so it keeps the "all of it goes in, or none of it" promise.
-- * The connection stays open between operations and is let go of by 'closeDtlv'.
--
-- This module does not depend on "Hosigraph.Khana"; it only knows facts, schemas and queries.
module Hosigraph.Khana.Dtlv
  ( Dtlv(..), Runner(..), DtlvError(..), DtlvOp(..), Words(..), wordsAttr
  , openDtlv, openDtlvWith, attachDtlv, closeDtlv, dtlvTransact, dtlvEntity, dtlvDatoms, dtlvDatomPage, dtlvDatomCount, dtlvHighest, dtlvHighestIn, dtlvQueryCells
    -- * Many in one round trip
  , dtlvMulti, dtlvEntities, dtlvEntityAndCount, dtlvQueryMany
    -- * Taking out whole entities
  , dtlvRetractRanges
    -- * A new file
  , dtlvCompactTo
    -- * Text we send and read (pure; exposed for the tests)
  , ednStr, ednKw, ednVal, schemaEdn, tameEdn
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Char (isAlpha, isAlphaNum, ord)
import           Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           Data.IORef
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Numeric (showHex)
import           System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import           System.FilePath ((</>))

import           Takka.Edn
import           Takka.Syntax (Cell(..), FindShape(..), Input(..), Pulled(..), Query(..), parseQuery)
import           Takka.Types
import           Hosigraph.Khana.Repl
import           Hosigraph.Khana.Sidecar

-- | Anything that went wrong while talking to @dtlv@ (including Datalevin refusing a transaction).
newtype DtlvError = DtlvError String

instance Show DtlvError where show (DtlvError s) = "dtlv: " ++ s

instance Exception DtlvError

-- | Who runs the code we send.
data Runner
  = Resident Sidecar        -- ^ a JVM that stays up (shared by every database that uses it)
  | NativeRepl DtlvRepl     -- ^ the native @dtlv repl@, one process that stays up (shared by every database that uses it)

-- | A Datalevin database: who runs it, the directory, and the schema it is opened with.
data Dtlv = Dtlv
  { dtlvRunner :: Runner
  , dtlvDir    :: FilePath      -- ^ absolute
  , dtlvSchema :: Schema
  , dtlvWords  :: Maybe Words   -- ^ words made outside Datalevin for some attributes (see 'Words')
  }

-- | Fulltext attributes whose words are made /outside/ Datalevin (a Japanese morphological analyzer, say), which no analyzer that Datalevin can
-- freeze into its file can be.  For each such attribute @a@ the database has a second one, @a.words@ ('wordsAttr'), that holds the
-- words of the text joined by a space, and is the one Datalevin indexes (with its default analyzer, which cuts at the space).
-- A fact of @a@ that goes in or out takes its words with it ('dtlvTransact'), and a @fulltext@ call on @a@ in a query
-- is made a call on @a.words@ for the words of its text ('dtlvQueryCells').  @a@ itself is an ordinary attribute with the text as written.
data Words = Words
  { wdTokenize :: [Text] -> IO [Text]       -- ^ the words of each text (joined by a space), in the order given
  , wdAttrs    :: M.Map Keyword Keyword     -- ^ the attribute with the text, and the attribute with its words
  , wdMark     :: Text                      -- ^ what makes these words (it is in the schema mark: other words, another khana)
  }

-- | The attribute that holds the words of the text in @a@.
wordsAttr :: Keyword -> Keyword
wordsAttr (Keyword t) = Keyword (t <> ".words")

-- | One fact to put in or take out.
data DtlvOp
  = DAdd     !EntityId !Keyword !Value
  | DRetract !EntityId !Keyword !Value

-- | Open (creating if needed) the database in a directory.  Running a first, empty
-- operation here finds out early whether @dtlv@ starts and whether the directory is usable.
openDtlv :: Runner -> FilePath -> Schema -> IO Dtlv
openDtlv runner = openDtlvWith runner Nothing

-- | 'openDtlv' for a schema some of whose fulltext attributes have their words made outside ('Words').  The schema is the
-- one Datalevin gets: it has the @.words@ attributes, and the attributes with the text are not fulltext.
openDtlvWith :: Runner -> Maybe Words -> FilePath -> Schema -> IO Dtlv
openDtlvWith runner ws dir schema = do
  createDirectoryIfMissing True dir
  absDir <- makeAbsolute dir
  let d = Dtlv runner absDir schema ws
  _ <- run d "nil"
  pure d

-- | Like 'openDtlv', without the first, empty operation: for a database that is only looked at (the look is its
-- first operation, so nothing is spent on a start that only checks), and let go of straight after.
attachDtlv :: Runner -> FilePath -> Schema -> IO Dtlv
attachDtlv runner dir schema = do
  createDirectoryIfMissing True dir
  absDir <- makeAbsolute dir
  pure (Dtlv runner absDir schema Nothing)

-- | Let go of the database.  With a resident JVM the connection is closed (so the directory can be
-- moved or removed); the JVM itself stays up for the other databases.  The same for the repl.
closeDtlv :: Dtlv -> IO ()
closeDtlv d = case dtlvRunner d of
  Resident sc  -> void (sidecarRequestIfRunning sc ("{:op :drop :dir " <> ednStr (T.pack (dtlvDir d)) <> "}"))
                    `catch` \(_ :: SidecarError) -> pure ()
  NativeRepl r -> void (replRequestIfRunning r ("(do (drop! " <> ednStr (T.pack (dtlvDir d)) <> ") :dropped)"))
                    `catch` \(_ :: DtlvReplError) -> pure ()

-- | Apply facts in one atomic transaction (in the order given).
dtlvTransact :: Dtlv -> [DtlvOp] -> IO ()
dtlvTransact _ [] = pure ()
dtlvTransact d ops0 = do
  mapM_ (maybe (pure ()) (throwIO . DtlvError) . notAUuid (dtlvSchema d)) ops0
  ops   <- wordsOps d ops0
  forms <- either (throwIO . DtlvError) pure (mapM opEdn ops)
  void (runWithC True d (Just ("[" <> T.unwords forms <> "]")) ("(" <> transactFn d <> " c data) nil"))

-- | A uuid attribute takes a UUID and nothing else.  This is checked before anything is sent: in Datalevin 1.2.10 a
-- retract with a string on such an attribute stops every later write until the database is opened again.  ('Hosigraph.write'
-- refuses such a batch before it is logged; this is for the other ways here: a log folded under another schema, say.)
notAUuid :: Schema -> DtlvOp -> Maybe String
notAUuid sch op = case op of
  DAdd e a v     -> check e a v
  DRetract e a v -> check e a v
  where
    check e a v = case (asType <$> M.lookup a sch, v) of
      (Just TUuid, VUuid _ _) -> Nothing
      (Just TUuid, _)         -> Just ("entity " ++ show e ++ ": " ++ show a ++ " holds UUIDs, and " ++ T.unpack (renderValue v) ++ " is not one")
      _                       -> Nothing

-- | With 'Words': after each fact of an attribute that has them, the fact of its words (the same kind, the same entity), so
-- the words of a text that is replaced or taken out are replaced or taken out with it.  One round trip to the tokenizer for all of them.
wordsOps :: Dtlv -> [DtlvOp] -> IO [DtlvOp]
wordsOps d ops = case dtlvWords d of
  Nothing -> pure ops
  Just w  -> do
    let text op = case op of
          DAdd _ a (VStr t)     | M.member a (wdAttrs w) -> Just t
          DRetract _ a (VStr t) | M.member a (wdAttrs w) -> Just t
          _                                              -> Nothing
        texts = [ t | Just t <- map text ops ]
    if null texts then pure ops else do
      made <- wdTokenize w texts
      when (length made /= length texts) (throwIO (DtlvError "the tokenizer answered with another number of texts than it was given"))
      pure (weave w ops made)
  where
    weave _ [] _ = []
    weave w (op : rest) ws = case (op, ws) of
      (DAdd e a (VStr _), x : xs)     | Just a' <- M.lookup a (wdAttrs w) -> op : DAdd e a' (VStr x) : weave w rest xs
      (DRetract e a (VStr _), x : xs) | Just a' <- M.lookup a (wdAttrs w) -> op : DRetract e a' (VStr x) : weave w rest xs
      _                                                                    -> op : weave w rest ws

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
-- It is @(copy (db c) NEW true)@.  The copy must hold a @data.mdb@.  A failure is a 'DtlvError'.
dtlvCompactTo :: Dtlv -> FilePath -> IO ()
dtlvCompactTo d dst = do
  absDst <- makeAbsolute dst
  case dtlvRunner d of
    Resident _   -> void (run d ("(copy (db c) " <> ednStr (T.pack absDst) <> " true) nil"))
    NativeRepl _ -> void (run d ("(copy (db c) " <> ednStr (T.pack absDst) <> " true) nil"))
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
dtlvQueryCells d txt0 inputs0 = do
  (txt, inputs) <- wordsQuery d txt0 inputs0
  (shape, dat, body) <- prepareQueryD (dataIsEvaluated d) txt inputs
  e     <- readReplyC d (Just dat) body
  rows  <- either (throwIO . DtlvError) pure (shapeRows shape e)
  forM rows (mapM cell)

-- | Many queries in one round trip, all against one snapshot.  Everything that can be refused here
-- (a query our parser rejects, a rule set, a value with no EDN spelling) is refused /before/
-- anything is sent.  If Datalevin itself rejects one, the whole call fails.
dtlvQueryMany :: Dtlv -> [(Text, [Input])] -> IO [[[Cell]]]
dtlvQueryMany _ [] = pure []
dtlvQueryMany d qs0 = do
  qs <- mapM (uncurry (wordsQuery d)) qs0
  prepared <- mapM (uncurry (prepareQuery "db")) qs
  es <- dtlvMulti d (map snd prepared)
  forM (zip (map fst prepared) es) $ \(shape, e) -> do
    rows <- either (throwIO . DtlvError) pure (shapeRows shape e)
    forM rows (mapM cell)

-- | The shape of a query's answer, and the code that asks it (against the database named by the first argument).
prepareQuery :: Text -> Text -> [Input] -> IO (FindShape, Text)
prepareQuery dbName txt inputs = do
  shape <- either (throwIO . DtlvError . ("query: " ++)) (pure . qShape) (readEdn txt >>= parseQuery)
  args  <- either (throwIO . DtlvError) pure (mapM (inputEdn True) inputs)
  pure (shape, "(q '" <> txt <> " " <> dbName <> T.concat (map (" " <>) args) <> ")")

-- | 'prepareQuery' for one query against @c@: the code is the same for the same query text, and the inputs go as
-- data, so the resident JVM compiles it once.
prepareQueryD :: Bool -> Text -> [Input] -> IO (FindShape, Text, Text)
prepareQueryD evaluated txt inputs = do
  shape <- either (throwIO . DtlvError . ("query: " ++)) (pure . qShape) (readEdn txt >>= parseQuery)
  args  <- either (throwIO . DtlvError) pure (mapM (inputEdn evaluated) inputs)
  -- @q@ stays in the call position, with each input as @(nth data i)@: the native @dtlv exec@ takes @q@ only there
  -- (@(apply q ...)@ or @(let [f q] ...)@ is "Unable to resolve symbol: q"), and the text is still the same for the
  -- same query and the same number of inputs.
  pure (shape, "[" <> T.unwords args <> "]", "(q '" <> txt <> " @c" <> T.concat [ " (nth data " <> T.pack (show i) <> ")" | i <- [0 .. length args - 1] ] <> ")")

-- | With 'Words': a query that calls @fulltext@ on an attribute that has them is sent as a call on its words attribute, for the
-- words of the text it searches for.  The text is a string written in the query, or a scalar @:in@ input (which is then made
-- into its words, wherever else the query uses it).  A call with no attribute searches every fulltext attribute, and its text is
-- made into words too (every one of them has its words made the same way).  Any other query is sent as it was written.
--
-- A text of /several words/ finds the entities that have all of them.  Datalevin's own search finds the ones that have any (and ranks
-- them by how many and how rare), so the call stays as it is, with its @:top@ (Datalevin's: the best 10 when none is written), and what
-- it finds is checked in the query: the words of the entity come back as @?v@, and one clause for each word asks whether it is one of
-- them.  The answer is the ones of the best @:top@ that have every word, and a @:top@ as big as the number of entities makes it all of
-- them.  (A call for each word, joined on @?e@, would be all of them whatever the @:top@, and it would pay for every entity that has any
-- one of the words, however few the answer: measured, 1.2 s for a word of 50,000 entities, again for each word.)
-- A word is a word of the tokenizer, as it made it: a word that Datalevin's analyzer drops from a search (@at@) is still asked for.
-- The checks need the row as @[[?e ?a ?v]]@ (the three may be @_@, or the row may be shorter), and they are clauses beside the call: a call
-- of several words inside @or@, @not@ or any other form that is not a clause of the query itself is refused, not searched as if it were one word.
--
-- A call that binds @?a@ and @?v@ gets the words attribute and the words, not the attribute and the text: bind @?e@ and ask
-- for the text of the entity.
wordsQuery :: Dtlv -> Text -> [Input] -> IO (Text, [Input])
wordsQuery d txt inputs = case dtlvWords d of
  Just w | "fulltext" `T.isInfixOf` txt -> do
    q   <- either (\m -> throwIO (DtlvError ("query: " ++ m))) pure (readEdn txt)
    wk  <- Walk w (inputSlots q) inputs <$> newIORef False <*> newIORef M.empty <*> newIORef 0
    q'  <- wordsWalk wk q
    did <- readIORef (wkChanged wk)
    if not did then pure (txt, inputs) else do
      made <- readIORef (wkMade wk)
      pure (renderEdn q', foldr (uncurry setScalar) inputs (M.toList made))
  _ -> pure (txt, inputs)

-- | Which input each variable of @:in@ is (the source @$@ is not an input: the database is given apart).
inputSlots :: Edn -> M.Map Text Int
inputSlots q = M.fromList [ (v, i) | (i, ESym v) <- zip [0 ..] consumed, "?" `T.isPrefixOf` v ]
  where
    consumed = filter (\x -> case x of ESym v -> not ("$" `T.isPrefixOf` v); _ -> True) specs
    specs = case q of
      EVec xs  -> takeWhile (\x -> case x of EKw _ -> False; _ -> True) (drop 1 (dropWhile (/= EKw "in") xs))
      EMap kvs -> case lookup (EKw "in") kvs of Just (EVec ys) -> ys; _ -> []
      _        -> []

-- | What the rewrite of one query knows and keeps.
data Walk = Walk
  { wkWords   :: Words
  , wkSlots   :: M.Map Text Int          -- ^ which input each variable of @:in@ is
  , wkInputs  :: [Input]
  , wkChanged :: IORef Bool              -- ^ a call was made a call on words
  , wkMade    :: IORef (M.Map Int Text)  -- ^ the inputs that were made into their words, by place
  , wkFresh   :: IORef Int               -- ^ for the names of the variables the checks add
  }

-- | The query with each @fulltext@ call on words made a call on them.  The clauses of the query itself may search for several words
-- ('wordsClause'); a call anywhere else may search for one.
wordsWalk :: Walk -> Edn -> IO Edn
wordsWalk wk q = case q of
  EVec xs  -> EVec <$> wordsClauses wk xs
  EMap kvs -> EMap <$> mapM (\(k, v) -> case (k, v) of
                                (EKw "where", EVec cs) -> (,) k . EVec <$> wordsClauses wk cs
                                _                      -> (,) k <$> wordsNested wk v) kvs
  _        -> wordsNested wk q

wordsClauses :: Walk -> [Edn] -> IO [Edn]
wordsClauses wk = fmap concat . mapM one
  where
    one c = case c of
      EVec [EList (ESym "fulltext" : args), bnd] -> wordsClause wk args bnd
      _                                          -> (: []) <$> wordsNested wk c

-- | One @[(fulltext $ ...) binding]@ clause of the query, as the clauses it becomes: the call on the words, and, for a text of several
-- words, a check of each word on the words of the entity.
wordsClause :: Walk -> [Edn] -> Edn -> IO [Edn]
wordsClause wk args bnd = case callOnWords (wkWords wk) args of
  Nothing -> pure [EVec [EList (ESym "fulltext" : args), bnd]]
  Just (rebuild, q) -> do
    (q', ws) <- wordsText wk q
    let call = EList (ESym "fulltext" : rebuild q')
    case ws of
      _ : _ : _ -> do
        (bnd', v) <- checkedBinding wk bnd
        p <- freshName wk "?fulltext-p"
        pure ( EVec [call, bnd']
             : EVec [EList [ESym "str", EStr " ", ESym v, EStr " "], ESym p]
             : [ EVec [EList [ESym "clojure.string/includes?", ESym p, EStr (" " <> x <> " ")]] | x <- ws ] )
      _ -> pure [EVec [call, bnd]]

-- | Any other place of the query: a @fulltext@ call on words is made a call on them there too, for one word only.
wordsNested :: Walk -> Edn -> IO Edn
wordsNested wk e = case e of
  EList (ESym "fulltext" : args) | Just (rebuild, q) <- callOnWords (wkWords wk) args -> do
    (q', ws) <- wordsText wk q
    when (length ws > 1) $ throwIO (DtlvError "a fulltext call that searches for several words must be a clause of the query itself, not inside or, not or another form: its words are checked by clauses beside it")
    pure (EList (ESym "fulltext" : rebuild q'))
  EList xs -> EList <$> mapM (wordsNested wk) xs
  EVec xs  -> EVec  <$> mapM (wordsNested wk) xs
  ESet xs  -> ESet  <$> mapM (wordsNested wk) xs
  EMap kv  -> EMap  <$> mapM (\(k, v) -> (,) <$> wordsNested wk k <*> wordsNested wk v) kv
  _        -> pure e

-- | The arguments of a @fulltext@ call that is on words: how to put them together again for a new text, and the text.
callOnWords :: Words -> [Edn] -> Maybe (Edn -> [Edn], Edn)
callOnWords w args = case args of
  src@(ESym "$") : EKw a : q : more | Just a' <- M.lookup (Keyword a) (wdAttrs w) -> Just (\q' -> src : EKw (kwText a') : q' : more, q)
  src@(ESym "$") : q : more         | notKw q                                    -> Just (\q' -> src : q' : more, q)
  _                                                                              -> Nothing
  where notKw x = case x of EKw _ -> False; _ -> True

-- | What a call searches for, as its words: the text to put in the call (a string written in the query is made into its words; an @:in@
-- variable stays, and its input is made into them, 'wkMade') and the words, each once.
wordsText :: Walk -> Edn -> IO (Edn, [Text])
wordsText wk q = do
  writeIORef (wkChanged wk) True
  case q of
    EStr s -> do
      t <- tokenized s
      pure (EStr t, wordsIn t)
    ESym v | Just i <- M.lookup v (wkSlots wk) -> do
      made <- readIORef (wkMade wk)
      t <- case M.lookup i made of
        Just t  -> pure t
        Nothing -> case drop i (wkInputs wk) of
          InScalar (VStr s) : _ -> do
            t <- tokenized s
            modifyIORef' (wkMade wk) (M.insert i t)
            pure t
          _ -> throwIO (DtlvError "the :in input that a fulltext call searches for must be a string")
      pure (q, wordsIn t)
    _ -> throwIO (DtlvError "a fulltext call on an attribute with words must search for a string written in the query, or for a scalar :in input")
  where
    tokenized s = do
      ws <- wdTokenize (wkWords wk) [s]
      case ws of
        [t] -> pure t
        _   -> throwIO (DtlvError "the tokenizer answered with another number of texts than it was given")
    wordsIn t = nubOrd (filter (not . T.null) (T.splitOn " " t))

-- | The binding of a call whose words are checked: a relation with the words in its third place, as a variable (the one written there, or
-- a new one for @_@ or for a row that is shorter), and the name of that variable.
checkedBinding :: Walk -> Edn -> IO (Edn, Text)
checkedBinding wk bnd = case bnd of
  EVec [EVec (e : rest)] -> do
    let (a, rest1) = case rest of x : xs -> (x, xs); [] -> (ESym "_", [])
    (v, rest2) <- case rest1 of
      []                                -> newV []
      ESym "_" : xs                     -> newV xs
      ESym s : xs | "?" `T.isPrefixOf` s -> pure (s, xs)
      _                                 -> throwIO (DtlvError bindMsg)
    pure (EVec [EVec (e : a : ESym v : rest2)], v)
  _ -> throwIO (DtlvError bindMsg)
  where
    newV xs = do { n <- freshName wk "?fulltext-v"; pure (n, xs) }
    bindMsg = "a fulltext call that searches for several words must bind its rows as [[?e ?a ?v]] (any of them may be _, and the row may be shorter): its words are checked beside it"

freshName :: Walk -> Text -> IO Text
freshName wk base = do
  n <- atomicModifyIORef' (wkFresh wk) (\k -> (k + 1, k + 1))
  pure (base <> T.pack (show n))

-- | Input number @i@ (a string), as the words it was made into.
setScalar :: Int -> Text -> [Input] -> [Input]
setScalar i t ins = case splitAt i ins of
  (before, InScalar (VStr _) : after) -> before ++ InScalar (VStr t) : after
  _                                   -> ins

cell :: Edn -> IO Cell
cell x = case x of
  EMap _ -> either (throwIO . DtlvError) (pure . CPull) (pulled x)
  _      -> CVal <$> value x

-- | An input as the text of a form. 'True': the form is /evaluated/ (@dtlv exec@ code), so rules text is quoted;
-- 'False': it travels as data, which is read and not evaluated, so it is left as written.
inputEdn :: Bool -> Input -> Either String Text
inputEdn evaluated i = case i of
  InScalar v -> ednVal v
  InColl vs  -> vec <$> mapM ednVal vs
  InRel rs   -> vec <$> mapM (fmap vec . mapM ednVal) rs
  InRules _  -> Left "rules (InRules) cannot be sent to dtlv as parsed rules; send them as EDN text (InRulesEdn)"
  -- evaluated, the rule heads would be calls: quoted, they are lists of symbols. As data they are read as they are.
  InRulesEdn t -> Right (if evaluated then "'" <> t else t)
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

-- | Is the @data@ of 'runWith' evaluated?  Under the resident @dtlv repl@ it is written into the
-- code; the resident JVM reads it as data.
dataIsEvaluated :: Dtlv -> Bool
dataIsEvaluated d = case dtlvRunner d of
  Resident _ -> False
  _          -> True

-- | The code for one operation, for the resident @dtlv repl@ ('NativeRepl'): the connection is kept (@conn!@ finds it, 'Hosigraph.Khana.Repl.setup' made it) and let go of
-- by 'closeDtlv'.
script :: Dtlv -> Maybe Text -> Text -> Text
script d dat body = T.unlines
  [ "(let [c (conn! " <> ednStr (T.pack (dtlvDir d)) <> " " <> schemaEdn (dtlvSchema d) <> " " <> walOpts <> ")"
    <> maybe "" (\x -> " data " <> x) dat <> "]"
  , "  (try [:ok (binding [*print-namespace-maps* false] (pr-str (do " <> body <> ")))]"
  , "       (catch Exception e [:err (str (ex-message e))])))" ]
  -- the repl opens the khana as the resident JVM does: with the write-ahead log ('walOpts')

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
-- The resident JVM and the resident @dtlv repl@ both get this.
walOpts :: Text
walOpts = "{:wal? true :wal-durability-profile :relaxed :wal-segment-max-bytes 4194304 :wal-retention-bytes 8388608 :wal-retention-ms 60000}"

-- | The function that applies a transaction and comes back when it is done.  @transact-ack!@ does the same as
-- @transact!@ and does not build the report we throw away.
transactFn :: Dtlv -> Text
transactFn _ = "transact-ack!"

-- | Run a body with the connection @c@ in scope; return the text of its value.
run :: Dtlv -> Text -> IO Text
run d = runWith d Nothing

-- | Like 'run', with some EDN data the body sees as @data@.  (Under the resident @dtlv repl@ it is written
-- into the code; with the resident JVM it travels as data, so a big one is not compiled.)
runWith :: Dtlv -> Maybe Text -> Text -> IO Text
runWith = runWithC False

-- | 'runWith' for a body whose text is the same at every call (what changes travels as @data@).  The resident JVM
-- compiles such a body once and keeps the function; Clojure's @eval@ makes new classes at every call, and that was most
-- of what a small call cost (1.5 ms against 0.05 ms for the same work in a function compiled before).  The native
-- @dtlv repl@ has nothing to keep, so it ignores the flag.
runWithC :: Bool -> Dtlv -> Maybe Text -> Text -> IO Text
runWithC cached d dat body = case dtlvRunner d of
  Resident sc  -> runResident cached sc d dat body
  NativeRepl r -> runRepl r d dat body

-- | What a 'script' printed (its last line) as the value, or as the error it caught.
scriptReply :: String -> Text -> Text -> IO Text
scriptReply what outT errT =
  case readEdn (tameEdn lastLn) of
    Right (EVec [EKw "ok", EStr s])  -> pure s
    Right (EVec [EKw "err", EStr m]) -> throwIO (DtlvError (T.unpack m))
    _ -> throwIO (DtlvError ("an answer we did not expect (" ++ what ++ "): " ++ T.unpack (T.take 400 (outT <> errT))))
  where lastLn = case filter (not . T.null . T.strip) (T.lines outT) of [] -> ""; ls -> last ls

runRepl :: DtlvRepl -> Dtlv -> Maybe Text -> Text -> IO Text
runRepl r d dat body = do
  out <- replRequest r (script d dat body) `catch` \(DtlvReplError m) -> throwIO (DtlvError m)
  scriptReply "the resident dtlv repl" out ""

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
    search = case [ k | k <- fulltextAttrs (dtlvSchema d), k `S.notMember` madeOutside ] of
      [] -> ""
      ks -> " :search [" <> T.unwords [ ednStr (kwText k) | k <- ks ] <> "]"
    -- the words attributes get Datalevin's default analyzer: their words are made already
    madeOutside = maybe S.empty (S.fromList . M.elems . wdAttrs) (dtlvWords d)
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
  VUuid a b -> Right ("#uuid " <> ednStr (uuidText a b))

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
      TUuid    -> Just ":db/valueType :db.type/uuid"
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
