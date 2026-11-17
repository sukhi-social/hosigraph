-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Khana
-- Description : かな(刹那)。The "now": events folded into a single instant, kept in a Datalevin database.
--
-- /On the name./  @khana@ is 刹那 (setsuna), a single instant: Pali khaṇa, Sanskrit kṣaṇa;
-- in Japanese the sound is カナ, \"かな\".  The khana is the instant the past keeps
-- producing.  "Hosigraph.Events" are the truth; the khana is what they add up to /right
-- now/, and it can be thrown away and folded again at any time ('Hosigraph.rebuildKhana').
-- (There is an old Buddhist idea, 刹那滅 setsuna-metsu: things arise and vanish at every
-- instant and are made again from the one before.  We did not plan that, but it fits.)
-- Read the package's @Hosi@ handle and its @Khana@ together and you get 「ほしかな」:
-- 「星かな？」 ("a star, I wonder?"), which is also the question the point lookup asks,
-- see "Hosigraph.Probe".
--
-- /Two ways to reach Datalevin./  The instant is a Datalevin database, driven through
--
-- * @'KhanaDtlv' path@: a Datalevin database, driven through the @dtlv@ command
--   (the executable at @path@; in the @khana-dtlv/@ directory, see "Hosigraph.Khana.Dtlv"), or
-- * @'KhanaDatalevinJvm' java jar@: the same Datalevin database (same @khana-dtlv/@ directory),
--   driven through one resident JVM running the Datalevin standalone jar
--   (see "Hosigraph.Khana.Sidecar").  One JVM serves every khana a 'KhanaRuntime' opens.
--
-- Both use the same files, so moving from one to the other needs no migration.
--
-- Folding is:
--
-- * Create  -> @Add@      (for a cardinality-one attribute the newer Create replaces the older: "last one wins")
-- * Delete  -> @Retract@  (the fact disappears from "now"; its history stays in the chunks)
--
-- The khana remembers how far it has folded in a small bookkeeping fact on a reserved
-- entity (id 1, attribute @hosi.meta/folded-seq@), written in the same transaction as
-- the events themselves, so "which events are in me" can never disagree with the data.
module Hosigraph.Khana
  ( KhanaBackend(..), khanaDirName, Khana
  , KhanaRuntime, newKhanaRuntime, closeKhanaRuntime
  , metaEntity, foldedSeqAttr, eraseGenAttr, schemaMarkAttr, schemaMark, khanaSchema
  , openKhana, closeKhana, foldedSeq, applyEvents, applyBatched, applyFacts, foldRefused, khanaDatoms, khanaDatomPage, compareKhanas, khanaDatomCount
  , khanaFoldedAndCount
    -- * Erasing
  , peekKhana, setEraseGen, setSchemaMark, cleanGenOf, setCleanGen, khanaCompactTo, eraseRanges
  , khanaEntity, khanaEntities, khanaQuery, khanaQueryMany, khanaQueryCells, khanaQueryCellsMany, khanaHighest, khanaHighestIn
  ) where

import           Control.Exception (SomeException, try, throwIO, finally)
import           Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word

import           Hosigraph.Backfill (Fact)
import           Hosigraph.Event
import           Hosigraph.Khana.Dtlv
import           Hosigraph.Khana.Sidecar (Sidecar, newSidecar, stopSidecar)
import           Hosigraph.Sha256 (sha256Hex)
import qualified Takka.Syntax as Q
import           Takka.Types

-- | Where the instant lives.
data KhanaBackend
  = KhanaDtlv FilePath         -- ^ a Datalevin database, through the @dtlv@ executable at this path
  | KhanaDatalevinJvm FilePath FilePath
                               -- ^ the same, through one resident JVM: the @java@ command, and the
                               --   @datalevin-<version>-standalone.jar@
  deriving (Eq, Show)

-- | The directory (inside the database directory) that holds this backend's khana.
khanaDirName :: KhanaBackend -> FilePath
khanaDirName (KhanaDtlv _)           = "khana-dtlv"
khanaDirName (KhanaDatalevinJvm _ _) = "khana-dtlv"       -- the same Datalevin files, whoever runs them

-- | What the khanas of one database share: with 'KhanaDatalevinJvm', the resident JVM (not started
-- until the first operation).  Close it after the last khana.
newtype KhanaRuntime = KhanaRuntime (Maybe Sidecar)

newKhanaRuntime :: KhanaBackend -> IO KhanaRuntime
newKhanaRuntime b = case b of
  KhanaDatalevinJvm java jar -> KhanaRuntime . Just <$> newSidecar java jar
  _                          -> pure (KhanaRuntime Nothing)

closeKhanaRuntime :: KhanaRuntime -> IO ()
closeKhanaRuntime (KhanaRuntime m) = mapM_ stopSidecar m

-- | An open khana.
newtype Khana = DtlvKhana Dtlv

-- | Entity 1 is reserved for the khana's own bookkeeping.  Real ids are time-based and
-- start at 2^21, so they cannot collide with it.
metaEntity :: EntityId
metaEntity = 1

foldedSeqAttr :: Keyword
foldedSeqAttr = "hosi.meta/folded-seq"

-- | How many erases the khana has followed (the manifest's @mfEraseGen@ it has caught up with).  It is written
-- in the same transaction that takes the erased entities out, so a khana that is behind the manifest knows it
-- (an erase committed, then we died) and is made again from the events, which leave the erased ids out.
eraseGenAttr :: Keyword
eraseGenAttr = "hosi.meta/erase-gen"

-- | The erase number at which this khana /file/ was last known to hold nothing that an erase took out: a file just
-- made from the events (which left the erased ids out from the start) is clean at the erase it was made for.
-- An erase retracts the facts, but Datalevin (LMDB) keeps the old bytes in its file until the pages are reused; so
-- while this is behind 'eraseGenAttr', the file may still hold erased values, and 'Hosigraph.clean' makes a new file.
cleanGenAttr :: Keyword
cleanGenAttr = "hosi.meta/clean-gen"

-- | The schema the events in this khana were folded under, as 'schemaMark' says it.  What a fold makes of the events
-- depends on the schema (one value or many, which types are taken, which values are unique), and the schema is an
-- argument of open that nobody keeps; so the khana keeps a mark of it, and open can tell a khana made under another one.
schemaMarkAttr :: Keyword
schemaMarkAttr = "hosi.meta/schema"

-- | A mark of a schema: a hash of every attribute's name, type, cardinality and uniqueness (in name order), so two
-- schemas have the same mark exactly when they fold the same events the same way.
schemaMark :: Schema -> Text
schemaMark s = T.pack ("hosi-schema/1:" ++ sha256Hex (TE.encodeUtf8 (T.pack text)))
  where
    text = concat [ T.unpack (kwText k) ++ " " ++ show (asType a) ++ " " ++ show (asCard a) ++ " " ++ show (asUnique a)
                    ++ concat [ " tuple " ++ unwords (map (T.unpack . kwText) (asTuple a)) | not (null (asTuple a)) ]   -- only when set: a mark made before tuples stays the same
                    ++ concat [ " fulltext " ++ analyzerVersion | asFulltext a ]
                    ++ "\n"
                  | (k, a) <- M.toAscList s ]

-- | Which words the fulltext index is made of (see 'Hosigraph.Khana.Sidecar.sidecarSource').  Datalevin keeps the analyzer in
-- the database, so a khana made with another one has to be made again: the mark of a schema with a fulltext attribute has this in it.
analyzerVersion :: String
analyzerVersion = "words/1"

-- | The caller's schema plus the bookkeeping attributes.
khanaSchema :: Schema -> Schema
khanaSchema = M.insert schemaMarkAttr (attr TString) . M.insert cleanGenAttr (attr TLong) . M.insert eraseGenAttr (attr TLong) . M.insert foldedSeqAttr (attr TLong)

-- | Open (creating if needed) a khana in a directory.
openKhana :: KhanaRuntime -> KhanaBackend -> FilePath -> Schema -> IO Khana
openKhana (KhanaRuntime sc) backend dir schema = case backend of
  KhanaDtlv exe
    | any asFulltext (M.elems schema) -> throwIO (SchemaRefused
        ("the attribute(s) " ++ unwords [ T.unpack (kwText k) | (k, a) <- M.toList schema, asFulltext a ]
         ++ " are fulltext, and the native dtlv cannot run the analyzer that makes their words: use KhanaDatalevinJvm"))
    | otherwise -> DtlvKhana <$> openDtlv (Native exe) dir (khanaSchema schema)
  KhanaDatalevinJvm _ _ -> case sc of
    Just s  -> DtlvKhana <$> openDtlv (Resident s) dir (khanaSchema schema)
    Nothing -> throwIO (BadArgument "KhanaDatalevinJvm needs a KhanaRuntime made for it (newKhanaRuntime)")

closeKhana :: Khana -> IO ()
closeKhana k = case k of
  DtlvKhana d     -> closeDtlv d             -- native: nothing stays open; JVM: close this one database

-- | The seq of the last event folded in (0 for an empty khana).
foldedSeq :: Khana -> IO Word64
foldedSeq k = case k of
  DtlvKhana d -> seqIn <$> dtlvEntity d metaEntity

seqIn :: M.Map Keyword [Value] -> Word64
seqIn m = case M.lookup foldedSeqAttr m of
  Just (VInt n : _) -> fromIntegral n
  _                 -> 0

genIn :: M.Map Keyword [Value] -> Int
genIn m = case M.lookup eraseGenAttr m of
  Just (VInt n : _) -> fromIntegral n
  _                 -> 0

-- | The mark of the schema the khana was folded under ('schemaMark'); 'Nothing' for a khana that was made before the mark
-- was kept, or that has not been stamped yet.
markIn :: M.Map Keyword [Value] -> Maybe Text
markIn m = case M.lookup schemaMarkAttr m of
  Just (VStr t : _) -> Just t
  _                 -> Nothing

-- | Where the khana stands, from one read of its bookkeeping entity: the seq of the last event folded in, the
-- number of the last erase it has followed (0: none), and the mark of the schema it was folded under.  Open asks this,
-- and a process costs time, so it is one question.
--
-- The khana is opened with only its own bookkeeping declared, and let go of at once.  Datalevin writes the schema it is
-- opened with into the file, so a schema that may still be refused must not be given to a khana that is to be left as it was.
peekKhana :: KhanaRuntime -> KhanaBackend -> FilePath -> IO (Word64, Int, Maybe Text)
peekKhana (KhanaRuntime sc) backend dir = do
  runner <- case backend of
    KhanaDtlv exe         -> pure (Native exe)
    KhanaDatalevinJvm _ _ -> maybe (throwIO (BadArgument "KhanaDatalevinJvm needs a KhanaRuntime made for it (newKhanaRuntime)"))
                                   (pure . Resident) sc
  d <- attachDtlv runner dir (khanaSchema M.empty)
  m <- dtlvEntity d metaEntity `finally` closeDtlv d
  pure (seqIn m, genIn m, markIn m)

-- | Say that the events in this khana were folded under this schema.  A khana made from the events says it when it is
-- whole; an empty one (nothing folded yet) is stamped at open.
setSchemaMark :: Khana -> Schema -> IO ()
setSchemaMark k s = case k of
  DtlvKhana d -> dtlvTransact d [ DAdd metaEntity schemaMarkAttr (VStr (schemaMark s)) ]

-- | Say that the khana has followed the erases up to this one, and holds nothing they took out: for a khana just
-- made from the events, which left the erased ids out from the start (so both numbers are this one).
setEraseGen :: Khana -> Int -> IO ()
setEraseGen k n = case k of
  DtlvKhana d -> dtlvTransact d [ DAdd metaEntity eraseGenAttr (VInt (fromIntegral n))
                                , DAdd metaEntity cleanGenAttr (VInt (fromIntegral n)) ]

-- | The erase number this khana file was last clean at (0: never an erase, or a file from before the number was kept).
cleanGenOf :: Khana -> IO Int
cleanGenOf k = case k of
  DtlvKhana d -> (\m -> case M.lookup cleanGenAttr m of Just (VInt n : _) -> fromIntegral n; _ -> 0) <$> dtlvEntity d metaEntity

-- | Say that this khana file holds nothing that erases up to number @n@ took out: for a file that was made by copying
-- the khana compacted ('khanaCompactTo'), after which the erased bytes are not in it.
setCleanGen :: Khana -> Int -> IO ()
setCleanGen k n = case k of
  DtlvKhana d -> dtlvTransact d [ DAdd metaEntity cleanGenAttr (VInt (fromIntegral n)) ]

-- | Copy this khana, compacted, into a directory that does not exist yet or is empty ('dtlvCompactTo').  The copy is
-- not opened here.
khanaCompactTo :: Khana -> FilePath -> IO ()
khanaCompactTo k dst = case k of
  DtlvKhana d -> dtlvCompactTo d dst

-- | Take out every fact of the entities whose ids are in these ranges (inclusive), and say in the same
-- transaction (the last one, if there are very many ranges) that the khana has followed erase number @gen@.
-- Only the entities' own facts go: what another entity says with such an id as its value stays.  Returns the number
-- of facts taken out.  Ranges must not reach the bookkeeping entity (ids above 'metaEntity').
eraseRanges :: Khana -> Int -> [(EntityId, EntityId)] -> IO Integer
eraseRanges k gen ranges = case k of
  DtlvKhana d -> dtlvRetractRanges d ranges (DAdd metaEntity eraseGenAttr (VInt (fromIntegral gen)))

-- | 'foldedSeq' and 'khanaDatomCount' together: with Datalevin it is one round trip, not two.
khanaFoldedAndCount :: Khana -> IO (Word64, Word64)
khanaFoldedAndCount k = case k of
  DtlvKhana d  -> do
    (m, n) <- dtlvEntityAndCount d metaEntity
    pure (seqIn m, minusMeta m (fromInteger n))

-- | The bookkeeping facts are not the caller's (the folded seq, and the erase number once there was an erase):
-- they are on the entity @m@ describes.
minusMeta :: M.Map Keyword [Value] -> Word64 -> Word64
minusMeta m n = n - min n (fromIntegral (sum (map length (M.elems m))))

eventToDtlv :: Event -> DtlvOp
eventToDtlv e = case evKind e of
  KCreate -> DAdd     (evId e) (evAttr e) (evVal e)
  KDelete -> DRetract (evId e) (evAttr e) (evVal e)

-- | Fold a batch in one atomic transaction.  Either every event applies (and the folded
-- seq moves to the last one), or the khana is untouched and the exception is rethrown.
applyEvents :: Khana -> [Event] -> IO ()
applyEvents _ [] = pure ()
applyEvents k evs = case k of
  DtlvKhana d     -> dtlvTransact d (map eventToDtlv evs ++ [DAdd metaEntity foldedSeqAttr marker])
  where marker = VInt (fromIntegral (maximum (map evSeq evs)))

-- | How the 'Corrupt' that 'applyBatched' throws begins (a caller can tell this refusal from other kinds of 'Corrupt').
foldRefused :: String
foldRefused = "could not fold sealed events into the khana: "

-- | Fold many events (already in seq order) in transactions of at most @n@ events.
-- Used for catching up and rebuilding, where the events were all accepted once before.
applyBatched :: Int -> Khana -> [Event] -> IO ()
applyBatched n k evs = do
  r <- try (forM_ (chunksOf n evs) (applyEvents k))
  case r of
    Right () -> pure ()
    Left (e :: SomeException) -> throwIO (Corrupt (foldRefused ++ show e))
  where chunksOf _ [] = []
        chunksOf m xs = let (a, b) = splitAt m xs in a : chunksOf m b

-- | Put facts in (adds only), in transactions of at most @n@ facts.  For a backfill ("Hosigraph.Backfill"): the folded seq
-- and the schema mark are not written, so a khana built this way is never taken for a whole one at open.
applyFacts :: Int -> Khana -> [Fact] -> IO ()
applyFacts n k fs = do
  r <- try (forM_ (chunksOf fs) $ \c -> case k of
              DtlvKhana d -> dtlvTransact d [ DAdd e a v | (e, a, v) <- c ])
  case r of
    Right () -> pure ()
    Left (e :: SomeException) -> throwIO (Corrupt ("could not put backfilled facts into the khana: " ++ show e))
  where chunksOf [] = []
        chunksOf xs = let (a, b) = splitAt n xs in a : chunksOf b

-- | Every fact in the khana except the bookkeeping one.
khanaDatoms :: Khana -> IO [Datom]
khanaDatoms k = filter ((/= metaEntity) . dE) <$> case k of
  DtlvKhana d     -> dtlvDatoms d

-- | A page of the facts, as 'dtlvDatomPage' gives it: whole entities, at most @n@ facts (more if one entity has more), from
-- the entity @lo@ (the first there is, for 'Nothing'), and where the next page starts ('Nothing': the end).
khanaDatomPage :: Khana -> Maybe EntityId -> Int -> IO ([Datom], Maybe EntityId)
khanaDatomPage k lo n = case k of
  DtlvKhana d -> do
    (ds, next) <- dtlvDatomPage d lo n
    pure (filter ((/= metaEntity) . dE) ds, next)

-- | How the facts of two khanas differ: how many only the first has, and how many only the second has.
-- Neither is ever held whole: the two are read in pages of about @n@ facts, each page up to the first entity where
-- either of them stops, and compared there.  What it holds at a time is a page of each, however large the khanas are.
compareKhanas :: Int -> Khana -> Khana -> IO (Int, Int)
compareKhanas n a b = go Nothing 0 0
  where
    go lo extra missing = do
      (pa, na) <- khanaDatomPage a lo n
      (pb, nb) <- khanaDatomPage b lo n
      let hi = case (na, nb) of                      -- the first entity where one of the pages stops short of the end
            (Just x, Just y) -> Just (min x y)
            (Just x, Nothing) -> Just x
            (Nothing, y)      -> y
          within ds = maybe ds (\h -> takeWhile ((< h) . dE) ds) hi
          sa = S.fromList (within pa)
          sb = S.fromList (within pb)
          -- counted now, not later: a count left as a thunk holds the page's two sets until the end (4GB on real data)
          !extra'   = extra   + S.size (sa `S.difference` sb)
          !missing' = missing + S.size (sb `S.difference` sa)
      case hi of
        Nothing -> pure (extra', missing')
        Just h  -> h `seq` go (Just h) extra' missing'

khanaDatomCount :: Khana -> IO Word64
khanaDatomCount k = case k of
  DtlvKhana d -> do
    (m, n) <- dtlvEntityAndCount d metaEntity
    pure (minusMeta m (fromInteger n))

-- | Everything the khana knows about one entity: attribute -> values.
khanaEntity :: Khana -> EntityId -> IO (M.Map Keyword [Value])
khanaEntity k i = case k of
  DtlvKhana d     -> dtlvEntity d i

-- | Many entities from one snapshot (with Datalevin, in one round trip).  Answers are in the order asked.
khanaEntities :: Khana -> [EntityId] -> IO [M.Map Keyword [Value]]
khanaEntities k is = case k of
  DtlvKhana d     -> dtlvEntities d is

-- | The facts of one attribute from the high end of its order, as 'dtlvHighest' gives them.
khanaHighest :: Khana -> Keyword -> Maybe (Value, EntityId) -> Int -> IO [Datom]
khanaHighest k a cur n = case k of
  DtlvKhana d -> dtlvHighest d a cur n

-- | The facts of a two-part tuple attribute for some keys, from the high end of its order part, as 'dtlvHighestIn' gives them.
khanaHighestIn :: Khana -> Keyword -> [Value] -> Maybe (Value, EntityId) -> Int -> IO [Datom]
khanaHighestIn k a keys cur n = case k of
  DtlvKhana d -> dtlvHighestIn d a keys cur n

-- | A Datalog query (EDN) against the khana.
khanaQuery :: Khana -> Text -> [Q.Input] -> IO [[Value]]
khanaQuery k txt inputs = case k of
  DtlvKhana _     -> do
    cells <- khanaQueryCells k txt inputs
    forM cells $ mapM $ \c -> case c of
      Q.CVal v  -> pure v
      Q.CPull _ -> throwIO (BadArgument "a pull in :find needs queryCells, not query")

-- | Like 'khanaQuery', with @(pull ?e [...])@ allowed in @:find@.
khanaQueryCells :: Khana -> Text -> [Q.Input] -> IO [[Q.Cell]]
khanaQueryCells k txt inputs = case k of
  DtlvKhana d     -> dtlvQueryCells d txt inputs

-- | Many queries against one snapshot (with Datalevin, in one round trip).  The answers are in the
-- order asked.  If one query fails, the call fails.
khanaQueryMany :: Khana -> [(Text, [Q.Input])] -> IO [[[Value]]]
khanaQueryMany k qs = case k of
  DtlvKhana _     -> do
    all' <- khanaQueryCellsMany k qs
    forM all' $ \cells -> forM cells $ mapM $ \c -> case c of
      Q.CVal v  -> pure v
      Q.CPull _ -> throwIO (BadArgument "a pull in :find needs queryCellsMany, not queryMany")

-- | Like 'khanaQueryMany', with @(pull ?e [...])@ allowed in @:find@.
khanaQueryCellsMany :: Khana -> [(Text, [Q.Input])] -> IO [[[Q.Cell]]]
khanaQueryCellsMany k qs = case k of
  DtlvKhana d     -> dtlvQueryMany d qs
