{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Khana
-- Description : かな(刹那)。The "now": events folded into a single instant, kept in a Datalevin database or an in-process engine.
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
-- /Two backends./  The instant can live in
--
-- * 'KhanaDatalevinNative': an engine that runs inside this process (the @khana/@ directory; see
--   "Takka.Engine"), or
-- * @'KhanaDtlv' path@: a Datalevin database, driven through the @dtlv@ command
--   (the executable at @path@; in the @khana-dtlv/@ directory, see "Hosigraph.Khana.Dtlv"), or
-- * @'KhanaDatalevinJvm' java jar@: the same Datalevin database (same @khana-dtlv/@ directory),
--   driven through one resident JVM running the Datalevin standalone jar
--   (see "Hosigraph.Khana.Sidecar").  One JVM serves every khana a 'KhanaRuntime' opens.
--
-- They have separate directories on purpose.  Both are made from the same events, so moving
-- from one to the other needs no migration: the new one is simply folded from the events.
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
  , metaEntity, foldedSeqAttr, khanaSchema
  , openKhana, closeKhana, foldedSeq, applyEvents, applyBatched, khanaDatoms, khanaDatomCount
  , khanaFoldedAndCount
  , khanaEntity, khanaEntities, khanaQuery, khanaQueryMany, khanaQueryCells, khanaQueryCellsMany, khanaWithDb
  ) where

import           Control.Exception (SomeException, try, throwIO)
import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Word
import           System.Directory (createDirectoryIfMissing)

import           Hosigraph.Event
import           Hosigraph.Khana.Dtlv
import           Hosigraph.Khana.Sidecar (Sidecar, newSidecar, stopSidecar)
import qualified Takka.Query as Q
import           Takka.Engine
import           Takka.Types

-- | Where the instant lives.
data KhanaBackend
  = KhanaDatalevinNative       -- ^ an engine inside this process, no outside tool needed (today a Haskell one; Datalevin's own native core, when there is one)
  | KhanaDtlv FilePath         -- ^ a Datalevin database, through the @dtlv@ executable at this path
  | KhanaDatalevinJvm FilePath FilePath
                               -- ^ the same, through one resident JVM: the @java@ command, and the
                               --   @datalevin-<version>-standalone.jar@
  deriving (Eq, Show)

-- | The directory (inside the database directory) that holds this backend's khana.
khanaDirName :: KhanaBackend -> FilePath
khanaDirName KhanaDatalevinNative     = "khana"
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
data Khana
  = NativeKhana Native
  | DtlvKhana  Dtlv

-- | Entity 1 is reserved for the khana's own bookkeeping.  Real ids are time-based and
-- start at 2^21, so they cannot collide with it.
metaEntity :: EntityId
metaEntity = 1

foldedSeqAttr :: Keyword
foldedSeqAttr = "hosi.meta/folded-seq"

-- | The caller's schema plus the bookkeeping attribute.
khanaSchema :: Schema -> Schema
khanaSchema = M.insert foldedSeqAttr (attr TLong)

-- | Open (creating if needed) a khana in a directory.  @noSync@: skip fsync on commit (the in-process engine only).
-- That is allowed /because/ the khana can be rebuilt; after a crash, 'Hosigraph.verify'
-- (or simply 'Hosigraph.rebuildCore') tells you whether it is still good.
openKhana :: KhanaRuntime -> KhanaBackend -> FilePath -> Schema -> Bool -> IO Khana
openKhana (KhanaRuntime sc) backend dir schema noSync = case backend of
  KhanaDatalevinNative -> do
    createDirectoryIfMissing True dir
    NativeKhana <$> openNative dir (khanaSchema schema) noSync
  KhanaDtlv exe ->
    DtlvKhana <$> openDtlv (Native exe) dir (khanaSchema schema)
  KhanaDatalevinJvm _ _ -> case sc of
    Just s  -> DtlvKhana <$> openDtlv (Resident s) dir (khanaSchema schema)
    Nothing -> throwIO (BadArgument "KhanaDatalevinJvm needs a KhanaRuntime made for it (newKhanaRuntime)")

closeKhana :: Khana -> IO ()
closeKhana k = case k of
  NativeKhana n   -> closeNative n
  DtlvKhana d     -> closeDtlv d             -- native: nothing stays open; JVM: close this one database

-- | The seq of the last event folded in (0 for an empty khana).
foldedSeq :: Khana -> IO Word64
foldedSeq k = case k of
  NativeKhana n -> do
    vs <- nativeValues n metaEntity foldedSeqAttr
    pure $ case vs of
      (VInt x : _) -> fromIntegral x
      _            -> 0
  DtlvKhana d -> seqIn <$> dtlvEntity d metaEntity

seqIn :: M.Map Keyword [Value] -> Word64
seqIn m = case M.lookup foldedSeqAttr m of
  Just (VInt n : _) -> fromIntegral n
  _                 -> 0

-- | 'foldedSeq' and 'khanaDatomCount' together: with Datalevin it is one round trip, not two.
khanaFoldedAndCount :: Khana -> IO (Word64, Word64)
khanaFoldedAndCount k = case k of
  NativeKhana _ -> (,) <$> foldedSeq k <*> khanaDatomCount k
  DtlvKhana d  -> do
    (m, n) <- dtlvEntityAndCount d metaEntity
    pure (seqIn m, minusMeta (fromInteger n))

-- | The bookkeeping fact is not the caller's.
minusMeta :: Word64 -> Word64
minusMeta n = if n > 0 then n - 1 else 0

eventToNative :: Event -> NativeOp
eventToNative e = case evKind e of
  KCreate -> NAdd     (evId e) (evAttr e) (evVal e)
  KDelete -> NRetract (evId e) (evAttr e) (evVal e)

eventToDtlv :: Event -> DtlvOp
eventToDtlv e = case evKind e of
  KCreate -> DAdd     (evId e) (evAttr e) (evVal e)
  KDelete -> DRetract (evId e) (evAttr e) (evVal e)

-- | Fold a batch in one atomic transaction.  Either every event applies (and the folded
-- seq moves to the last one), or the khana is untouched and the exception is rethrown.
applyEvents :: Khana -> [Event] -> IO ()
applyEvents _ [] = pure ()
applyEvents k evs = case k of
  NativeKhana n   -> nativeTransact n (map eventToNative evs ++ [NAdd metaEntity foldedSeqAttr marker])
  DtlvKhana d     -> dtlvTransact d (map eventToDtlv evs ++ [DAdd metaEntity foldedSeqAttr marker])
  where marker = VInt (fromIntegral (maximum (map evSeq evs)))

-- | Fold many events (already in seq order) in transactions of at most @n@ events.
-- Used for catching up and rebuilding, where the events were all accepted once before.
applyBatched :: Int -> Khana -> [Event] -> IO ()
applyBatched n k evs = do
  r <- try (forM_ (chunksOf n evs) (applyEvents k))
  case r of
    Right () -> pure ()
    Left (e :: SomeException) -> throwIO (Corrupt ("could not fold sealed events into the khana: " ++ show e))
  where chunksOf _ [] = []
        chunksOf m xs = let (a, b) = splitAt m xs in a : chunksOf m b

-- | Every fact in the khana except the bookkeeping one.
khanaDatoms :: Khana -> IO [Datom]
khanaDatoms k = filter ((/= metaEntity) . dE) <$> case k of
  NativeKhana n -> nativeDatoms n
  DtlvKhana d     -> dtlvDatoms d

khanaDatomCount :: Khana -> IO Word64
khanaDatomCount k = do
  n <- case k of
    NativeKhana n -> nativeDatomCount n
    DtlvKhana d     -> fromInteger <$> dtlvDatomCount d
  pure (minusMeta n)

-- | Everything the khana knows about one entity: attribute -> values.
khanaEntity :: Khana -> EntityId -> IO (M.Map Keyword [Value])
khanaEntity k i = case k of
  NativeKhana n -> nativeEntity n i
  DtlvKhana d     -> dtlvEntity d i

-- | Many entities from one snapshot (with Datalevin, in one round trip).  Answers are in the order asked.
khanaEntities :: Khana -> [EntityId] -> IO [M.Map Keyword [Value]]
khanaEntities k is = case k of
  NativeKhana n -> nativeEntities n is
  DtlvKhana d     -> dtlvEntities d is

-- | A Datalog query (EDN) against the khana.
khanaQuery :: Khana -> Text -> [Q.Input] -> IO [[Value]]
khanaQuery k txt inputs = case k of
  NativeKhana n -> nativeQuery n txt inputs
  DtlvKhana _     -> do
    cells <- khanaQueryCells k txt inputs
    forM cells $ mapM $ \c -> case c of
      Q.CVal v  -> pure v
      Q.CPull _ -> throwIO (BadArgument "a pull in :find needs queryCells, not query")

-- | Like 'khanaQuery', with @(pull ?e [...])@ allowed in @:find@.
khanaQueryCells :: Khana -> Text -> [Q.Input] -> IO [[Q.Cell]]
khanaQueryCells k txt inputs = case k of
  NativeKhana n -> nativeQueryCells n txt inputs
  DtlvKhana d     -> dtlvQueryCells d txt inputs

-- | Many queries against one snapshot (with Datalevin, in one round trip).  The answers are in the
-- order asked.  If one query fails, the call fails.
khanaQueryMany :: Khana -> [(Text, [Q.Input])] -> IO [[[Value]]]
khanaQueryMany k qs = case k of
  NativeKhana n -> nativeQueryMany n qs
  DtlvKhana _     -> do
    all' <- khanaQueryCellsMany k qs
    forM all' $ \cells -> forM cells $ mapM $ \c -> case c of
      Q.CVal v  -> pure v
      Q.CPull _ -> throwIO (BadArgument "a pull in :find needs queryCellsMany, not queryMany")

-- | Like 'khanaQueryMany', with @(pull ?e [...])@ allowed in @:find@.
khanaQueryCellsMany :: Khana -> [(Text, [Q.Input])] -> IO [[[Q.Cell]]]
khanaQueryCellsMany k qs = case k of
  NativeKhana n -> nativeQueryCellsMany n qs
  DtlvKhana d     -> dtlvQueryMany d qs

-- | Run something against one consistent snapshot, with the whole in-process engine available.
-- Only the in-process khana has a 'Db'; with Datalevin this is a 'BadArgument'.
khanaWithDb :: Khana -> (Db -> IO a) -> IO a
khanaWithDb k act = case k of
  NativeKhana n -> nativeWithDb n act
  DtlvKhana _     -> throwIO (BadArgument "withKhana needs the in-process khana (KhanaDatalevinNative): with a Datalevin khana there is no Db to hand over (use query / queryCells)")
