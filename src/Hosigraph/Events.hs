-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Events
-- Description : The part that manages events: the tail, sealing, chunks, the manifest, and an entity's history.
--
-- An /event/ (the Create or Delete of one fact) happens once, is written down, and never
-- changes.  This module is everything about that: letting events in ('record'), freezing
-- them into chunks ('sealNow'), keeping the list of which chunks are the world
-- ("Hosigraph.Manifest"), and finding the events about one entity again ('history').
--
-- Events are the truth.  "Now" ("Hosigraph.Khana") is made /from/ them and can be thrown
-- away and made again; this part cannot.  It knows nothing about Datalog or any khana.
--
-- > record ──► tail.log ──sealNow──► chunks/<name>/{chunk.hosi, mphf.idx, filter.bin}
-- >                                     ▲
-- >                                     └─ manifest (which chunks are the world; zone maps; hashes; signature)
--
-- A chunk may also have an /erase record/ in @erase/@ (a "Hosigraph.Erase" file, listed by the manifest):
-- the ids of that chunk that are gone.  This part keeps it (it sweeps the ones the manifest does not
-- list), checks it ('verifyEvents'), and reads with it: 'history' and 'eventsAfter' leave out an erased id,
-- 'record' refuses to write to one, and a world whose listed record cannot be read does not open.
--
-- /Locking./  One writer at a time.  'record', 'commit', 'rollback', 'sealNow' and
-- 'resetTailTo' must be called while holding 'withEventsLock'; 'seal' does that for you.
-- 'history' takes the lock for a moment, only to see one consistent picture.
module Hosigraph.Events
  ( -- * Opening
    Events, EventsConfig(..), defaultEventsConfig, openEvents, closeEvents, withEventsLock
    -- * Letting events in (the caller decides between commit and rollback)
  , Recorded(..), record, recordMany, commit, rollback
    -- * Tying them into chunks
  , SealReport(..), sealDue, sealNow, seal
    -- * Taking ids out of the world
  , EraseDone(..), eraseNow, eraseGeneration, withoutReserved, ownedBy
    -- * Letting them go from the disk
  , RewriteDone(..), rewriteErased
    -- * Letting a quiet unit go cold
  , FreezeDone(..), freezeUnit, coldUnits, freezeCold, flushAccess, Stamp(..), stampsOf
    -- * Finding them again
  , history, historyTrace, historyTraceAfter, LookupTrace(..), Probe(..)
    -- * For whoever folds the events into "now"
  , lastSeq, eventsAfter, eventsAfterEach, tailBatches, resetTailTo
    -- * Checking
  , verifyEvents, EventsStats(..), eventsStats
  ) where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.IORef
import           Data.List (nub, partition, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import           Data.Word
import           System.Directory
import           System.FilePath ((</>))

import           Hosigraph.BinaryFuse (BinaryFuse)
import           Hosigraph.Chunk
import qualified Hosigraph.Erase as Erase
import           Hosigraph.Event
import           Hosigraph.Id
import           Hosigraph.Manifest
import           Hosigraph.Sha256 (sha256Hex)
import           Takka.Types (Keyword, Value(..))
import           Hosigraph.Sync
import           Hosigraph.Tail

-- ---------------------------------------------------------------------------
-- Configuration and state

data EventsConfig = EventsConfig
  { ecSealEvents  :: Int              -- ^ seal when the tail holds this many events (design: still open)
  , ecSealAgeMs   :: Maybe Integer    -- ^ ...or when its oldest event is this old (checked on each record)
  , ecBlockEvents :: Int              -- ^ target events per block inside a chunk
  , ecColdBlockEvents :: Int          -- ^ ...inside a cold chunk (bigger: a cold chunk is scanned more than it is asked for one id)
  , ecColdAfterMs :: Maybe Integer    -- ^ a unit (kind × month) nobody has read or written for this long is cold ('coldUnits'); 'Nothing' (the default): none is, and nothing about use is kept
  , ecSync        :: Bool             -- ^ fsync the tail on every record (turn off only for experiments)
  , ecClock       :: Clock            -- ^ milliseconds since the POSIX epoch (tests inject their own)
  , ecReaderCacheBytes :: Int         -- ^ how much memory the open chunk readers may hold together (the least recently used are closed first; one reader is always kept)
  }

defaultEventsConfig :: EventsConfig
defaultEventsConfig = EventsConfig
  { ecSealEvents = 10000, ecSealAgeMs = Nothing, ecBlockEvents = 256, ecColdBlockEvents = 4096, ecColdAfterMs = Nothing, ecSync = True, ecClock = systemClock
  , ecReaderCacheBytes = 64 * 1024 * 1024 }

-- | The open chunk readers.  Each use stamps a reader with a new tick; when the total is over the
-- limit, the reader with the oldest tick goes first.
data ReaderCache = ReaderCache
  { rkTick  :: !Int
  , rkBytes :: !Int
  , rkMap   :: !(M.Map String (Int, ChunkReader))
  }

emptyReaderCache :: ReaderCache
emptyReaderCache = ReaderCache 0 0 M.empty

-- | Put a reader in (or stamp it again) and drop the oldest ones while over the limit.
-- The reader just put in is never dropped, so one huge chunk still opens.
cacheReader :: Int -> String -> ChunkReader -> ReaderCache -> ReaderCache
cacheReader limit name r rk0 = shrink rk1
  where
    t   = rkTick rk0 + 1
    old = maybe 0 (readerBytes . snd) (M.lookup name (rkMap rk0))
    rk1 = ReaderCache t (rkBytes rk0 - old + readerBytes r) (M.insert name (t, r) (rkMap rk0))
    shrink rk
      | rkBytes rk <= limit || M.size (rkMap rk) <= 1 = rk
      | otherwise =
          let (victim, (_, vr)) = minimumByTick (M.toList (rkMap rk))
          in shrink rk { rkBytes = rkBytes rk - readerBytes vr, rkMap = M.delete victim (rkMap rk) }
    minimumByTick = foldr1 (\a b -> if fst (snd a) <= fst (snd b) then a else b)

data TailState = TailState
  { tsBatchesRev :: [[Event]]                 -- ^ newest batch first
  , tsById       :: M.Map HosiId [Event]      -- ^ per entity, oldest event first
  , tsCount      :: !Int
  , tsOldestMs   :: Maybe Integer             -- ^ clock reading when the oldest unsealed batch arrived
  }

emptyTailState :: TailState
emptyTailState = TailState [] M.empty 0 Nothing

tailEvents :: TailState -> [Event]
tailEvents = concat . reverse . tsBatchesRev

addBatch :: Maybe Integer -> [Event] -> TailState -> TailState
addBatch now evs ts = ts
  { tsBatchesRev = evs : tsBatchesRev ts
  , tsById = foldl (\m e -> M.insertWith (flip (++)) (evId e) [e] m) (tsById ts) evs
  , tsCount = tsCount ts + length evs
  , tsOldestMs = case tsOldestMs ts of Nothing -> now; old -> old }

fromBatches :: Maybe Integer -> [[Event]] -> TailState
fromBatches now = foldl (\ts b -> addBatch now b ts) emptyTailState

data Events = Events
  { xDir      :: FilePath                      -- ^ the khana's own directory
  , xCfg      :: EventsConfig
  , xLock     :: MVar ()
  , xManifest :: IORef Manifest
  , xTail     :: TailLog
  , xTailSt   :: IORef TailState
  , xNextSeq  :: IORef Word64
  , xIds      :: IdGen
  , xReaders  :: IORef ReaderCache
  , xFilters  :: IORef (M.Map String (Maybe BinaryFuse))   -- ^ every chunk's filter we have read (about 2.3 to 2.8 bytes an id; not part of the reader limit)
  , xErased   :: IORef Erased                  -- ^ the erase records of the world in 'xManifest', loaded (whoever swaps one swaps the other, under the lock)
  , xRewrite  :: MVar ()                       -- ^ one 'rewriteErased' (or 'freezeUnit') at a time (it works outside 'xLock', and two would write the same scratch directory)
  , xStamps   :: IORef (M.Map (Kind, Month) Stamp)   -- ^ when each unit was last read and written; empty, and never touched, unless 'ecColdAfterMs' is set
  }

chunksDir :: Events -> FilePath
chunksDir k = xDir k </> "chunks"

-- | Where the erase records are: next to @chunks/@, not inside a chunk's directory (a chunk may live far away; the record stays here).
-- It is made when the first record is written, so a world that never erased has no such directory.
eraseDir :: Events -> FilePath
eraseDir k = xDir k </> "erase"

-- | The erase records, read.  They are small and asked often, so they live in memory.
data Erased = Erased
  { erByChunk :: M.Map String Erase.EraseSet   -- ^ by chunk name, each cut to that chunk's own id range (a chunk without erased ids is not here)
  , erAll     :: Erase.EraseSet                -- ^ every erased id of the world (what 'record' asks before it lets a write in)
  }

-- | Read every record the manifest lists.  A listed record that is missing, that is not the file the manifest
-- signed, or that is not an erase record is 'Corrupt': opening without it would show what was erased.
loadErased :: FilePath -> Manifest -> IO Erased
loadErased dir mf = do
  sets <- fmap catMaybes . forM (mfChunks mf) $ \c -> case ceErase c of
    Nothing -> pure Nothing
    Just fe -> do
      let path = dir </> "erase" </> feName fe
          bad why = throwIO (Corrupt (path ++ ": " ++ why))
      there <- doesFileExist path
      unless there (bad "the erase record the manifest lists is missing")
      bs <- B.readFile path
      when (B.length bs /= feBytes fe || sha256Hex bs /= feSha256 fe) (bad "the erase record does not match the manifest (size or SHA-256)")
      case Erase.decode bs of
        Left why -> bad why
        Right s  -> let mine = Erase.restrictTo (ceMinId c, ceMaxId c) s
                    in pure (if Erase.null mine then Nothing else Just (ceName c, mine))
  pure (Erased (M.fromList sets) (Erase.fromRanges (concatMap (Erase.toRanges . snd) sets)))

-- | Ids up to this one are reserved (the "now" layer (Hosigraph.Khana) keeps its own bookkeeping on entity 1).
lastReservedId :: HosiId
lastReservedId = 1

-- ---------------------------------------------------------------------------
-- Opening and recovery

-- | Open a khana directory (creating it if new).  Recovery, in order:
--
-- 1. read the manifest (a bad signature is an error, not a guess);
-- 2. delete chunk directories and erase records the manifest does not list (a seal or an Erase that died before the swap);
-- 3. read the erase records the manifest does list (one that is missing or damaged is 'Corrupt': the world is not shown without it);
-- 4. read the tail, cut off a torn last record, ignore records the manifest already sealed.
--
-- (Making "now" agree with all this is the job of whoever owns "now": see 'lastSeq',
-- 'eventsAfter' and 'tailBatches'.)
openEvents :: EventsConfig -> FilePath -> IO Events
openEvents cfg dir = do
  createDirectoryIfMissing True (dir </> "chunks")
  mf <- readManifest dir
  sweepOrphans dir mf
  erased <- loadErased dir mf                                  -- before anything is opened: a refusal leaves nothing behind
  (batches0, goodSize) <- readTail dir
  fileSize <- (\ex -> if ex then getFileSize (dir </> tailFile) else pure 0) =<< doesFileExist (dir </> tailFile)
  let sealed  = mfSealedSeq mf
      batches = [ b | b <- batches0, not (null b), evSeq (last b) > sealed ]
      lastS   = maximum (sealed : [ evSeq e | b <- batches, e <- b ])
  tl <- openTail dir (ecSync cfg)
  when (goodSize < fileSize || length batches /= length batches0) $ resetTail tl batches
  -- ids must never go backwards, whatever the clock says
  let maxIdSeen = maximum (0 : [ ceMaxId c | c <- mfChunks mf ] ++ [ evId e | b <- batches, e <- b ])
  gen <- newIdGen maxIdSeen
  now <- Just <$> ecClock cfg
  stamps <- loadStamps cfg dir mf
  Events dir cfg <$> newMVar () <*> newIORef mf <*> pure tl <*> newIORef (fromBatches now batches)
                <*> newIORef (lastS + 1) <*> pure gen <*> newIORef emptyReaderCache <*> newIORef M.empty
                <*> newIORef erased <*> newMVar () <*> newIORef stamps

closeEvents :: Events -> IO ()
closeEvents k = flushAccess k >> closeTail (xTail k)

withEventsLock :: Events -> IO a -> IO a
withEventsLock k act = withMVar (xLock k) (const act)

-- | Remove chunk directories, erase records (and stray temp files) that the manifest does not list.
sweepOrphans :: FilePath -> Manifest -> IO ()
sweepOrphans dir mf = do
  let known = S.fromList (map ceName (mfChunks mf))
  present <- listDirectory (dir </> "chunks")
  forM_ present $ \n -> unless (n `S.member` known) $ removePathForcibly (dir </> "chunks" </> n)
  -- a record that was written for an Erase that never reached the manifest, or one that a newer manifest has replaced
  let knownRecords = S.fromList [ feName fe | c <- mfChunks mf, Just fe <- [ceErase c] ]
  hasErase <- doesDirectoryExist (dir </> "erase")
  when hasErase $ do
    records <- listDirectory (dir </> "erase")
    forM_ records $ \n -> unless (n `S.member` knownRecords) $ removePathForcibly (dir </> "erase" </> n)
  forM_ [manifestFile ++ ".tmp", tailFile ++ ".tmp", accessFile ++ ".tmp"] $ \f -> removePathForcibly (dir </> f)

readerFor :: Events -> ChunkEntry -> IO ChunkReader
readerFor k c = do
  cache <- readIORef (xReaders k)
  r <- case M.lookup (ceName c) (rkMap cache) of
    Just (_, r) -> pure r
    Nothing     -> openChunkReader (chunksDir k </> ceName c)
  -- stamp it as just used (readers run in parallel, so the update is atomic)
  atomicModifyIORef' (xReaders k) (\rk -> (cacheReader (ecReaderCacheBytes (xCfg k)) (ceName c) r rk, ()))
  pure r

-- | The chunk's filter (none for a chunk that has no @filter.bin@).  It is read once and kept: it is small,
-- and it is what answers \"is this chunk worth opening?\" before the reader is.
filterFor :: Events -> ChunkEntry -> IO (Maybe BinaryFuse)
filterFor k c = do
  known <- M.lookup (ceName c) <$> readIORef (xFilters k)
  case known of
    Just f  -> pure f
    Nothing -> do
      f <- readChunkFilter (chunksDir k </> ceName c) (ceIds c)
      atomicModifyIORef' (xFilters k) (\m -> (M.insert (ceName c) f m, ()))
      pure f

-- ---------------------------------------------------------------------------
-- Letting events in

-- | An append that has reached the disk but is not yet part of the world: the caller
-- either 'commit's it or 'rollback's it.
data Recorded = Recorded
  { rcTemps      :: M.Map Int HosiId    -- ^ the fresh id given to each @Tmp n@
  , rcEvents     :: [Event]
  , rcSizeBefore :: Integer             -- ^ tail size before the append (what 'rollback' cuts back to)
  }

-- | Give fresh ids to the @Tmp@ placeholders, number the events, append them to the tail
-- as one record and fsync.  Throws 'BadArgument' (before writing anything) for an empty
-- batch, a reserved id, or a write to an entity that was erased.
record :: Events -> [Op] -> IO Recorded
record k ops = do
  [r] <- recordMany k [ops]
  either throwIO pure r

-- | Like 'record' for several batches at once: one append, one fsync.  The batches stay separate
-- (each its own record, its own @Tmp@ names, consecutive seq numbers).  A batch that cannot be
-- recorded (empty, a reserved id, or a write to an erased entity) is a 'Left' in its place and does not
-- stop the others.  The ones that were recorded are all-or-nothing together: 'rollback' of the first
-- of them cuts every one of them back out of the tail.
recordMany :: Events -> [[Op]] -> IO [Either HosiError Recorded]
recordMany k opss = do
  seq0 <- readIORef (xNextSeq k)
  erased <- erAll <$> readIORef (xErased k)
  planned <- plan erased seq0 opss
  sizes <- appendBatches (xTail k) [ evs | Right (_, evs) <- planned ]
  pure (attach planned sizes)
  where
    plan _ _ [] = pure []
    plan erased s (ops : rest) = case validate erased ops of
      Left e   -> (Left e :) <$> plan erased s rest
      Right () -> do
        (tmap, evs) <- resolveBatch k s ops
        (Right (tmap, evs) :) <$> plan erased (s + fromIntegral (length evs)) rest
    attach [] _ = []
    attach (Left e : rest) ss = Left e : attach rest ss
    attach (Right (tmap, evs) : rest) (sz : ss) = Right (Recorded tmap evs sz) : attach rest ss
    attach (Right _ : _) [] = error "recordMany: fewer sizes than batches"

-- | An erased entity takes no more events: a new one would be written into a chunk that has no record
-- of it, and the entity would come back.  Only the entity an operation is /about/ is asked; a value that
-- points at an erased entity is just a number, and numbers may stay.
validate :: Erase.EraseSet -> [Op] -> Either HosiError ()
validate erased ops
  | null ops  = Left (BadArgument "write: no operations")
  | otherwise = case [ i | op <- ops, i <- idsOf op, i <= lastReservedId ] of
      (i : _) -> Left (BadArgument ("write: " ++ show i ++ " is not a valid entity id"))
      []      -> case [ i | op <- ops, Id i <- [subjectOf op], Erase.member i erased ] of
        (i : _) -> Left (BadArgument ("write: " ++ show i ++ " was erased"))
        []      -> Right ()

subjectOf :: Op -> Ref
subjectOf (Create r _ _) = r
subjectOf (Delete r _ _) = r

-- | Fresh ids for the @Tmp@ names of one batch, and its events numbered from @s@.
resolveBatch :: Events -> Word64 -> [Op] -> IO (M.Map Int HosiId, [Event])
resolveBatch k s ops = do
  let temps = nub [ n | op <- ops, Tmp n <- refsOf op ]
  tmap <- M.fromList <$> forM temps (\n -> (,) n <$> nextId (xIds k) (ecClock (xCfg k)))
  let resolve (Id i)  = i
      resolve (Tmp n) = tmap M.! n
      value (Lit v)   = v
      value (RefTo r) = VInt (resolve r)
      evs = [ case op of
                Create r a v -> Event q KCreate (resolve r) a (value v)
                Delete r a v -> Event q KDelete (resolve r) a (value v)
            | (q, op) <- zip [s ..] ops ]
  pure (tmap, evs)

refsOf :: Op -> [Ref]
refsOf (Create r _ v) = r : valRefs v
refsOf (Delete r _ v) = r : valRefs v

valRefs :: Val -> [Ref]
valRefs (RefTo r) = [r]
valRefs _         = []

idsOf :: Op -> [HosiId]
idsOf op = [ i | Id i <- refsOf op ]

-- | The recorded batch is now part of the world.
commit :: Events -> Recorded -> IO ()
commit k rec = do
  now <- ecClock (xCfg k)
  modifyIORef' (xTailSt k) (addBatch (Just now) (rcEvents rec))
  writeIORef (xNextSeq k) (evSeq (last (rcEvents rec)) + 1)

-- | Take the recorded batch back out of the tail (it never happened).
rollback :: Events -> Recorded -> IO ()
rollback k rec = truncateTail (xTail k) (rcSizeBefore rec)

-- ---------------------------------------------------------------------------
-- Sealing

-- | Is it time to seal (too many events, or the oldest one is too old)?
sealDue :: Events -> IO Bool
sealDue k = do
  now <- ecClock (xCfg k)
  ts <- readIORef (xTailSt k)
  let cfg = xCfg k
      byCount = tsCount ts >= ecSealEvents cfg
      byAge   = case (ecSealAgeMs cfg, tsOldestMs ts) of
                  (Just limit, Just t0) -> tsCount ts > 0 && now - t0 >= limit
                  _                     -> False
  pure (byCount || byAge)

data SealReport = SealReport
  { srEvents     :: Int
  , srChunks     :: [ChunkEntry]
  , srGeneration :: Int
  } deriving Show

-- | 'sealNow' under the lock.
seal :: Events -> IO (Maybe SealReport)
seal k = withEventsLock k (sealNow k)

-- | Freeze the tail into hot chunks: one chunk per (kind, creation month of the entity).
-- Returns 'Nothing' if the tail is empty.  Call with the lock held.
--
-- The steps, and why a crash anywhere is safe:
--
-- 1. write the new chunk directories (nobody points at them yet);
-- 2. write the new manifest and swap it in (before this line: old world; after: new world);
-- 3. empty the tail (if we die before this, the tail still holds records the manifest
--    already sealed; 'openEvents' ignores those by seq).
sealNow :: Events -> IO (Maybe SealReport)
sealNow k = do
  ts <- readIORef (xTailSt k)
  let evs = tailEvents ts
  if null evs then pure Nothing else do
    mf <- readIORef (xManifest k)
    let groups = M.toList (M.map reverse (M.fromListWith (++) [ ((evKind e, idMonth (evId e)), [e]) | e <- reverse evs ]))
    entries <- forM (zip [mfNextChunk mf ..] groups) $ \(n, ((kind, month), es)) -> do
      let name = chunkName n kind month
          cdir = chunksDir k </> name
      info  <- writeChunk cdir (ecBlockEvents (xCfg k)) kind month es
      files <- hashChunkFiles cdir
      pure (entryFromInfo name info [ FileEntry fn b sh | (fn, b, sh) <- files ])
    syncDir (chunksDir k)
    let mf' = mf { mfGeneration = mfGeneration mf + 1
                 , mfSealedSeq  = maximum (map evSeq evs)
                 , mfNextChunk  = mfNextChunk mf + length entries
                 , mfChunks     = mfChunks mf ++ entries }
    writeManifest (xDir k) mf'
    writeIORef (xManifest k) mf'
    resetTail (xTail k) []
    writeIORef (xTailSt k) emptyTailState
    stampWrite k [ (ceKind c, ceMonth c) | c <- entries ]
    writeAccess k
    pure (Just (SealReport (length evs) entries (mfGeneration mf')))

-- | The name of the @n@th chunk: @c000007-create-2026-11@.  A name is used once and never again.
chunkName :: Int -> Kind -> Month -> String
chunkName n kind month = "c" ++ pad 6 n ++ "-" ++ kindName kind ++ "-" ++ showMonth month
  where pad w x = let s = show x in replicate (w - length s) '0' ++ s

-- | Take a chunk the manifest no longer lists out of the process and off the disk: its cached reader and
-- filter, its directory, and its erase record.  (If we die before this, 'openEvents' sweeps what is left.)
retire :: Events -> ChunkEntry -> IO ()
retire k c = do
  dropCached k [ceName c]
  removePathForcibly (chunksDir k </> ceName c)
  forM_ (ceErase c) $ \fe -> removePathForcibly (eraseDir k </> feName fe)

-- ---------------------------------------------------------------------------
-- Erasing

-- | What an 'eraseNow' did.
data EraseDone = EraseDone
  { edSealed     :: Maybe SealReport   -- ^ the seal that came first (the tail is the one place a record cannot reach), if the tail was not empty
  , edDropped    :: [String]           -- ^ chunks taken out of the world: the erase covered every id from their first to their last, so nothing is left to keep
  , edRecorded   :: [String]           -- ^ chunks that now have a record (a new one, or a bigger one in place of the old)
  , edGeneration :: Int                -- ^ the world's erase generation after; the same as before when there was nothing new to erase
  } deriving Show

-- | The world's erase generation (@mfEraseGen@): how many erases have changed it.  The khana keeps the number of the
-- last one it followed; if the two differ, the khana is made again from the events.
eraseGeneration :: Events -> IO Int
eraseGeneration k = mfEraseGen <$> readIORef (xManifest k)

-- | The ids an erase may take: not the reserved ones (the "now" layer's own bookkeeping lives there).
withoutReserved :: Erase.EraseSet -> Erase.EraseSet
withoutReserved = Erase.restrictTo (lastReservedId + 1, maxBound)

-- | The ids of the entities that, in a Create event, have one of these attributes with one of these owners as its value
-- (a reference is stored as the id it points at), among the entities created in the id range (inclusive).  An account's
-- things are not one range of ids, and a post it already deleted has lost its facts in \"now\" but not its Create in
-- the chunks, so the answer comes from the events.  Any Create counts, not only the newest: an entity that was ever said
-- to be the owner's is the owner's.
--
-- Call with the lock held.  It seals the tail first (the Creates not yet in a chunk are also the owner's), then reads
-- every Create chunk whose ids reach into the range, each from its file and not through the reader cache (a scan
-- should not push the readers that are in use out of it).  The cost is that read.  Ids come back in order, once each.
-- Only a lookup: nothing is erased.
ownedBy :: Events -> [Keyword] -> [HosiId] -> (HosiId, HosiId) -> IO [HosiId]
ownedBy k attrs owners (lo, hi)
  | null attrs || null owners = pure []
  | otherwise = do
      _ <- sealNow k
      mf <- readIORef (xManifest k)
      let attrSet  = S.fromList attrs
          ownerSet = S.fromList owners
          theirs e = evKind e == KCreate && lo <= evId e && evId e <= hi && evAttr e `S.member` attrSet
                     && case evVal e of VInt n -> n `S.member` ownerSet; _ -> False
      found <- forM [ c | c <- mfChunks mf, ceKind c == KCreate, ceMaxId c >= lo, ceMinId c <= hi ] $ \c -> do
        cr <- openChunkReader (chunksDir k </> ceName c)
        map evId . filter theirs <$> readChunkEvents cr
      pure (S.toAscList (S.fromList (concat found)))

-- | Make these ids gone from the world.  Call with the lock held.  The steps, and why a crash anywhere is safe:
--
-- 1. 'sealNow', so that nothing of the erased ids is left in the tail (a record cannot reach the tail);
-- 2. for each chunk, cut the set to the chunk's own id range (so a record never names an id that has not been
--    handed out yet) and join it with the chunk's record, if it has one.  If that now covers /every/ id from the
--    chunk's first to its last, the chunk is dropped (no record, no rewrite: this is what makes \"erase a whole
--    month\" cheap); if it is more than before, the chunk gets a new record file (named for the new generation);
--    otherwise the chunk is left alone;
-- 3. write the record files (nobody points at them yet), then the new manifest and swap it in.  /This is the commit point/:
--    before it, the old world, with the seal in 1 standing on its own; after it, the ids are gone;
-- 4. only now take what the manifest no longer lists off the disk and out of memory.  (If we die before that,
--    'openEvents' sweeps it.)
--
-- Nothing new to erase (the ids are not in any chunk, or already gone) writes nothing and keeps the generation.
-- This part knows nothing of the khana: whoever owns \"now\" takes the entities out of it after this, and tells
-- it the generation (see 'Hosigraph.erase').
eraseNow :: Events -> Erase.EraseSet -> IO EraseDone
eraseNow k set0 = do
  sealed <- sealNow k
  mf <- readIORef (xManifest k)
  erased <- readIORef (xErased k)
  let set = withoutReserved set0
      fate c =
        let mine = Erase.restrictTo (ceMinId c, ceMaxId c) set
            old  = M.findWithDefault Erase.empty (ceName c) (erByChunk erased)
            new  = Erase.union old mine
        in if new == old then Keep
           else if new == Erase.fromRanges [(ceMinId c, ceMaxId c)] then Drop
           else Record new
      fates = [ (c, fate c) | c <- mfChunks mf ]
  if all (isKeep . snd) fates then pure (EraseDone sealed [] [] (mfEraseGen mf)) else do
    let gen = mfEraseGen mf + 1
    let anyRecord = not (null [ () | (_, Record _) <- fates ])
    when anyRecord (createDirectoryIfMissing True (eraseDir k))      -- a world that only ever dropped chunks has no erase/
    kept <- forM fates $ \(c, f) -> case f of
      Keep      -> pure (Just c)
      Drop      -> pure Nothing
      Record s  -> do
        let nm   = ceName c ++ "-" ++ show gen ++ ".bin"
            bs   = Erase.encode s
            path = eraseDir k </> nm
        B.writeFile path bs
        syncFile path
        pure (Just c { ceErase = Just (FileEntry nm (B.length bs) (sha256Hex bs)) })
    when anyRecord $ syncDir (eraseDir k) >> syncDir (xDir k)        -- the records, and erase/ itself the first time
    let mf' = mf { mfGeneration = mfGeneration mf + 1, mfEraseGen = gen, mfChunks = catMaybes kept }
    writeManifest (xDir k) mf'                         -- the commit point
    erased' <- loadErased (xDir k) mf'                 -- read back what open would read: memory and disk say one thing
    writeIORef (xManifest k) mf'
    writeIORef (xErased k) erased'
    let dropped  = [ c | (c, Drop) <- fates ]
        replaced = [ feName fe | (c, Record _) <- fates, Just fe <- [ceErase c] ]      -- the older, smaller record of a chunk that has a bigger one now
    mapM_ (retire k) dropped
    forM_ replaced $ \n -> removePathForcibly (eraseDir k </> n)
    pure (EraseDone sealed (map ceName dropped) [ ceName c | (c, Record _) <- fates ] gen)
  where
    isKeep Keep = True
    isKeep _    = False

data Fate = Keep | Drop | Record Erase.EraseSet

-- | Forget the readers and filters of chunks that are no longer in the world.
dropCached :: Events -> [String] -> IO ()
dropCached k names = do
  atomicModifyIORef' (xFilters k) (\m -> (foldr M.delete m names, ()))
  atomicModifyIORef' (xReaders k) $ \rk ->
    let gone = [ r | n <- names, Just (_, r) <- [M.lookup n (rkMap rk)] ]
    in (rk { rkBytes = rkBytes rk - sum (map readerBytes gone), rkMap = foldr M.delete (rkMap rk) names }, ())

-- ---------------------------------------------------------------------------
-- Rewriting

-- | What a 'rewriteErased' did.
data RewriteDone = RewriteDone
  { rwRewritten :: [(String, Maybe String)]  -- ^ (the chunk that had a record, the chunk that took its place); 'Nothing' when nothing of it was left, so no chunk takes its place
  , rwSkipped   :: [String]                  -- ^ chunks it gave up on, because the world changed under it (an erase got there first); a later call finds them again
  , rwLeft      :: Int                       -- ^ chunks that still have a record after this
  } deriving Show

-- | Write the chunks that have an erase record again, without the erased events, and let the old ones go: after
-- this the erased ids are in no file of the events (the old chunk's directory and its record are removed).  At most
-- @limit@ chunks, so that a caller can do a little in each quiet moment.  Call it /without/ the lock: it takes the
-- lock itself, for a moment at a time.
--
-- For each chunk:
--
-- 1. (no lock) read the old chunk's events and write the ones that are left into a scratch directory in @chunks/@;
--    the new chunk has its own zone map, its own MPHF and its own filter;
-- 2. (lock) if the old chunk is still exactly what we read (same entry, same record), give the scratch directory its
--    name, write the manifest with the new chunk in the old one's place, and swap it in.  /This is the commit point/:
--    before it the old chunk is the world, after it the new one is;
-- 3. 'retire' the old chunk.  (A crash before this leaves a directory and a record the manifest does not list;
--    'openEvents' sweeps them.)
--
-- One swap for each chunk, so each step is cheap.  A chunk none of whose events is left is simply taken out.
-- When the world changed under step 1, the scratch directory is thrown away and the chunk is left for the next call.
--
-- The record goes with the chunk, and so does the write guard that came with it ('record' no longer refuses an
-- erased id whose record is gone): the id is a bare number that points at nothing.
rewriteErased :: Events -> Int -> IO RewriteDone
rewriteErased k limit = withMVar (xRewrite k) $ \_ -> do
  mf <- withEventsLock k (readIORef (xManifest k))
  results <- forM (take limit [ c | c <- mfChunks mf, isJust (ceErase c) ]) (rewriteOne k)
  left <- length . filter (isJust . ceErase) . mfChunks <$> withEventsLock k (readIORef (xManifest k))
  pure (RewriteDone [ r | Right r <- results ] [ n | Left n <- results ] left)

rewriteOne :: Events -> ChunkEntry -> IO (Either String (String, Maybe String))
rewriteOne k old = do
  erased <- withEventsLock k (readIORef (xErased k))
  let gone    = M.findWithDefault Erase.empty (ceName old) (erByChunk erased)
      scratch = chunksDir k </> (".rewrite-" ++ ceName old)
      giveUp  = removePathForcibly scratch >> pure (Left (ceName old))
  removePathForcibly scratch
  -- the old chunk may be retired by an erase while we read it: then we only have to see that it is gone
  made <- try $ do
    cr  <- openChunkReader (chunksDir k </> ceName old)
    evs <- readChunkEvents cr
    let keep = [ e | e <- evs, not (Erase.member (evId e) gone) ]
    if null keep then pure Nothing else do
      -- a cold chunk is written again as a cold one: an erase must not make it hot (and give it an MPHF) again
      info  <- (if ceTier old == "cold" then writeColdChunk scratch (ecColdBlockEvents (xCfg k)) else writeChunk scratch (ecBlockEvents (xCfg k)))
                 (ceKind old) (ceMonth old) keep
      files <- hashChunkFiles scratch
      pure (Just (info, [ FileEntry fn b sh | (fn, b, sh) <- files ]))
  case made of
    Left (e :: SomeException) -> do
      stillThere <- (old `elem`) . mfChunks <$> withEventsLock k (readIORef (xManifest k))
      if stillThere then removePathForcibly scratch >> throwIO e else giveUp
    Right result -> withEventsLock k $ do
      now <- readIORef (xManifest k)
      if old `notElem` mfChunks now then giveUp else do          -- same entry, same record: nobody erased more of it, nobody dropped it
        let name = chunkName (mfNextChunk now) (ceKind old) (ceMonth old)
            newEntry = fmap (\(info, fs) -> (entryFromInfo name info fs) { ceTier = ceTier old }) result
        forM_ result $ \_ -> renameDirectory scratch (chunksDir k </> name) >> syncDir (chunksDir k)
        let mf' = now { mfGeneration = mfGeneration now + 1
                      , mfNextChunk  = mfNextChunk now + maybe 0 (const 1) newEntry
                      , mfChunks     = concat [ if ceName c == ceName old then maybe [] pure newEntry else [c] | c <- mfChunks now ] }
        writeManifest (xDir k) mf'                                -- the commit point
        erased' <- loadErased (xDir k) mf'
        writeIORef (xManifest k) mf'
        writeIORef (xErased k) erased'
        retire k old
        pure (Right (ceName old, ceName <$> newEntry))

-- ---------------------------------------------------------------------------
-- Freezing

-- | What a 'freezeUnit' did.
data FreezeDone = FreezeDone
  { fzUnit    :: (Kind, Month)
  , fzMerged  :: [String]         -- ^ the hot chunks that were put together (and are gone from the world)
  , fzInto    :: Maybe String     -- ^ the cold chunk that took their place; 'Nothing' when there was no hot chunk, or none of their events was left (all erased)
  , fzSkipped :: Bool             -- ^ the world changed under it (an erase got there first): nothing was done, and the next call finds the unit again
  } deriving Show

-- | Put all the /hot/ chunks of one unit (kind × creation month) together into one cold chunk: bigger blocks
-- ('ecColdBlockEvents'), no @mphf.idx@, @tier = "cold"@.  A cold chunk of the unit that is already there is left as it is
-- (cold chunks are not put together with each other); a hot chunk sealed later stays hot, and the next call freezes it
-- into a second cold chunk.  Call it /without/ the lock: it takes the lock for a moment at a time, as 'rewriteErased' does.
--
-- It reads each chunk /through its erase record/, so the cold chunk has none of the erased events, and the records of the
-- chunks it replaces go with them: a freeze also finishes the Erase on that unit.  Steps, as in 'rewriteErased':
--
-- 1. (no lock) read the hot chunks and write the cold one into a scratch directory in @chunks/@;
-- 2. (lock) if every one of the hot chunks is still in the manifest exactly as it was (same entry, same record), give the
--    scratch directory its name and swap in a manifest with the cold chunk in the first one's place.  /The commit point./
-- 3. 'retire' the hot chunks.  (A crash before this leaves directories the manifest does not list; 'openEvents' sweeps them.)
--
-- Whatever the world held, a 'history' asked before and after has the same answer; only where it is found changes.
freezeUnit :: Events -> (Kind, Month) -> IO FreezeDone
freezeUnit k unit@(kind, month) = withMVar (xRewrite k) $ \_ -> do
  (mf, erased) <- withEventsLock k ((,) <$> readIORef (xManifest k) <*> readIORef (xErased k))
  let olds    = [ c | c <- mfChunks mf, ceKind c == kind, ceMonth c == month, ceTier c == "hot" ]
      scratch = chunksDir k </> (".freeze-" ++ kindName kind ++ "-" ++ showMonth month)
      giveUp  = removePathForcibly scratch >> pure (FreezeDone unit [] Nothing True)
  if null olds then pure (FreezeDone unit [] Nothing False) else do
    removePathForcibly scratch
    made <- try $ do
      kept <- forM olds $ \c -> do
        cr  <- openChunkReader (chunksDir k </> ceName c)
        evs <- readChunkEvents cr
        let gone = M.findWithDefault Erase.empty (ceName c) (erByChunk erased)
        pure [ e | e <- evs, not (Erase.member (evId e) gone) ]
      let keep = concat kept
      if null keep then pure Nothing else do
        info  <- writeColdChunk scratch (ecColdBlockEvents (xCfg k)) kind month keep
        files <- hashChunkFiles scratch
        pure (Just (info, [ FileEntry fn b sh | (fn, b, sh) <- files ]))
    case made of
      Left (e :: SomeException) -> do
        -- an erase or a rewrite may have retired one of them while we read: then we only have to see that it is gone
        stillThere <- (\now -> all (`elem` mfChunks now) olds) <$> withEventsLock k (readIORef (xManifest k))
        if stillThere then removePathForcibly scratch >> throwIO e else giveUp
      Right result -> withEventsLock k $ do
        now <- readIORef (xManifest k)
        if any (`notElem` mfChunks now) olds then giveUp else do
          let name     = chunkName (mfNextChunk now) kind month
              newEntry = fmap (\(info, fs) -> (entryFromInfo name info fs) { ceTier = "cold" }) result
              oldNames = map ceName olds
              place c | [ceName c] == take 1 oldNames = maybe [] pure newEntry  -- the cold chunk stands where the first hot one stood
                      | ceName c `elem` oldNames  = []
                      | otherwise                 = [c]
          forM_ result $ \_ -> renameDirectory scratch (chunksDir k </> name) >> syncDir (chunksDir k)
          let mf' = now { mfGeneration = mfGeneration now + 1
                        , mfNextChunk  = mfNextChunk now + maybe 0 (const 1) newEntry
                        , mfChunks     = concatMap place (mfChunks now) }
          writeManifest (xDir k) mf'                                -- the commit point
          erased' <- loadErased (xDir k) mf'
          writeIORef (xManifest k) mf'
          writeIORef (xErased k) erased'
          mapM_ (retire k) olds
          pure (FreezeDone unit oldNames (ceName <$> newEntry) False)

-- | Which units are cold, by the rule of the first version: a unit that has a hot chunk, and was last read or written
-- ('Stamp') 'ecColdAfterMs' ago or longer.  A unit with no stamp is taken as just used.  Nothing is cold when
-- 'ecColdAfterMs' is 'Nothing'.  In order.
coldUnits :: Events -> IO [(Kind, Month)]
coldUnits k = case ecColdAfterMs (xCfg k) of
  Nothing    -> pure []
  Just after -> do
    now    <- ecClock (xCfg k)
    mf     <- withEventsLock k (readIORef (xManifest k))
    stamps <- readIORef (xStamps k)
    pure [ u | u <- S.toAscList (S.fromList [ (ceKind c, ceMonth c) | c <- mfChunks mf, ceTier c == "hot" ])
             , Just s <- [M.lookup u stamps], now - max (stRead s) (stWrite s) >= after ]

-- | 'freezeUnit' for the first @limit@ of the 'coldUnits' (in their order), and then the stamps are written down.
freezeCold :: Events -> Int -> IO [FreezeDone]
freezeCold k limit = do
  units <- take limit <$> coldUnits k
  done  <- mapM (freezeUnit k) units
  flushAccess k
  pure done

-- ---------------------------------------------------------------------------
-- When a unit was last used

-- | When a unit (kind × month) was last read ('history') and last written (a seal made a chunk of it), in milliseconds
-- on the 'ecClock'.  A hint for 'coldUnits', not a record: a lost or unreadable @access.json@ makes every unit \"just
-- used\", which only delays a freeze.
data Stamp = Stamp { stRead :: !Integer, stWrite :: !Integer } deriving (Eq, Show)

accessFile :: FilePath
accessFile = "access.json"

-- | The stamps as they are now.
stampsOf :: Events -> IO (M.Map (Kind, Month) Stamp)
stampsOf = readIORef . xStamps

-- | Read @access.json@ (when freezing is on); a unit of the world that has no stamp (a new world, a lost file) is just used.
loadStamps :: EventsConfig -> FilePath -> Manifest -> IO (M.Map (Kind, Month) Stamp)
loadStamps cfg dir mf = case ecColdAfterMs cfg of
  Nothing -> pure M.empty
  Just _  -> do
    now   <- ecClock cfg
    there <- doesFileExist (dir </> accessFile)
    saved <- if there then (decodeStamps <$> B.readFile (dir </> accessFile)) `catch` \(_ :: IOException) -> pure Nothing else pure Nothing
    let fresh = M.fromList [ ((ceKind c, ceMonth c), Stamp now now) | c <- mfChunks mf ]
    pure (M.union (M.intersection (maybe M.empty id saved) fresh) fresh)

encodeStamps :: M.Map (Kind, Month) Stamp -> B.ByteString
encodeStamps m = BL.toStrict . A.encode $ A.object
  [ "units" A..= [ A.object [ "kind" A..= kindName kd, "month" A..= showMonth mo, "read" A..= stRead s, "write" A..= stWrite s ]
                 | ((kd, mo), s) <- M.toList m ] ]

decodeStamps :: B.ByteString -> Maybe (M.Map (Kind, Month) Stamp)
decodeStamps bs = A.decodeStrict bs >>= AT.parseMaybe (A.withObject "access" $ \o -> do
  us <- o A..: "units"
  fmap M.fromList . forM us $ A.withObject "unit" $ \u -> do
    kindS  <- u A..: "kind"
    monthS <- u A..: "month"
    kd <- maybe (fail "unknown kind") pure (kindFromName kindS)
    mo <- maybe (fail "bad month") pure (parseMonth monthS)
    r  <- u A..: "read"
    w  <- u A..: "write"
    pure ((kd, mo), Stamp r w))

-- | Stamp these units as read just now (no-op unless freezing is on).
stampRead :: Events -> [(Kind, Month)] -> IO ()
stampRead = stampWith (\now s -> s { stRead = max (stRead s) now }) (\now -> Stamp now 0)

-- | Stamp these units as written just now.
stampWrite :: Events -> [(Kind, Month)] -> IO ()
stampWrite = stampWith (\now s -> s { stWrite = max (stWrite s) now }) (\now -> Stamp 0 now)

stampWith :: (Integer -> Stamp -> Stamp) -> (Integer -> Stamp) -> Events -> [(Kind, Month)] -> IO ()
stampWith touch new k units = forM_ (ecColdAfterMs (xCfg k)) $ \_ -> do
  now <- ecClock (xCfg k)
  atomicModifyIORef' (xStamps k) $ \m ->
    (foldl (\acc u -> M.alter (Just . maybe (new now) (touch now)) u acc) m units, ())

-- | Write the stamps down.  Call with the lock held.  A hint: if it cannot be written, nothing else is the worse for it.
-- (A stamp of a unit that has no chunk is let go when the file is read again: 'loadStamps' takes the units of the world.)
writeAccess :: Events -> IO ()
writeAccess k = forM_ (ecColdAfterMs (xCfg k)) $ \_ -> do
  m <- readIORef (xStamps k)
  let tmp = xDir k </> (accessFile ++ ".tmp")
  (B.writeFile tmp (encodeStamps m) >> atomicReplace tmp (xDir k </> accessFile))
    `catch` \(_ :: IOException) -> pure ()

-- | 'writeAccess' under the lock: for whoever stamps from outside it ('closeEvents', maintenance).
flushAccess :: Events -> IO ()
flushAccess k = withEventsLock k (writeAccess k)

-- ---------------------------------------------------------------------------
-- Finding events again

-- | What a 'history' lookup did, so the design's claims can be watched happening.
data LookupTrace = LookupTrace
  { ltMonth          :: Month                 -- ^ taken from the id alone
  , ltChunks         :: Int                   -- ^ chunks in the world
  , ltSkippedByMonth :: Int                   -- ^ zone map: another month
  , ltSkippedByRange :: Int                   -- ^ zone map: right month, id outside [min, max]
  , ltSkippedByErase :: Int                   -- ^ the chunk's erase record has this id: asked in memory, so the filter was not even read
  , ltSkippedByFilter :: Int                  -- ^ the chunk's filter said \"certainly not\" (the MPHF was not asked)
  , ltProbed         :: [(String, Probe Int)] -- ^ 「星かな？」 asked of each chunk that was not skipped (Int = events found)
  , ltTailEvents     :: Int                   -- ^ events found in the unsealed tail
  } deriving Show

-- | Every event about one entity (Creates and Deletes, oldest first), from the chunks
-- and the unsealed tail.  Each chunk is asked in this order, cheapest first: the zone map in the
-- manifest (month, id range), the chunk's erase record (in memory), the chunk's filter, then the MPHF
-- with its fingerprint and key check.  An id a chunk's record has erased is not in that chunk, whatever
-- the chunk's bytes still hold.
history :: Events -> HosiId -> IO [Event]
history k i = fst <$> historyTrace k i

historyTrace :: Events -> HosiId -> IO ([Event], LookupTrace)
historyTrace = historyTraceAfter (pure ())

-- | 'historyTrace' with something that happens between the look at the manifest and the reading of the chunks (the
-- first time round only).  That is exactly where a rewrite or an erase can retire a chunk the view still lists, so a
-- test can put one there; nothing else has a use for it.
historyTraceAfter :: IO () -> Events -> HosiId -> IO ([Event], LookupTrace)
historyTraceAfter between k i = (attempt (4 :: Int) between <* stampRead k [(KCreate, idMonth i), (KDelete, idMonth i)])
  where
    -- A chunk can leave the world between our look at the manifest and our opening it (a rewrite or an erase
    -- retires it).  Then the read fails; if the manifest has moved on since, we look again with the new one.
    attempt tries act = do
      view@(mf, _, _) <- withEventsLock k ((,,) <$> readIORef (xManifest k) <*> readIORef (xTailSt k) <*> readIORef (xErased k))
      act
      r <- try (historyIn k i view)
      case r of
        Right x -> pure x
        Left (e :: SomeException)
          | tries > 0, isChunkTrouble e -> do
              gen <- mfGeneration <$> withEventsLock k (readIORef (xManifest k))
              if gen /= mfGeneration mf then attempt (tries - 1) (pure ()) else throwIO e
          | otherwise -> throwIO e
    isChunkTrouble e = case (fromException e, fromException e) of
      (Just (_ :: IOException), _) -> True
      (_, Just (_ :: HosiError))   -> True
      _                            -> False

-- | 'historyTrace' for one fixed view of the world.
historyIn :: Events -> HosiId -> (Manifest, TailState, Erased) -> IO ([Event], LookupTrace)
historyIn k i (mf, ts, erased) = do
  let month = idMonth i
      chunks = mfChunks mf
      sameMonth = [ c | c <- chunks, ceMonth c == month ]
      inRange   = [ c | c <- sameMonth, ceMinId c <= i, i <= ceMaxId c ]
      erasedIn c = maybe False (Erase.member i) (M.lookup (ceName c) (erByChunk erased))
      (gone, live) = partition erasedIn inRange
  -- the filters next: a chunk that says \"certainly not\" is never opened
  filters <- forM live $ \c -> (,) c <$> filterFor k c
  let maybeHere = [ c | (c, f) <- filters, filterMayHave f i ]
  probes <- forM maybeHere $ \c -> do
    cr <- readerFor k c
    r <- lookupInChunk cr i
    pure (ceName c, r)
  let fromChunks = concat [ es | (_, Star es) <- probes ]
      fromTail   = M.findWithDefault [] i (tsById ts)
      trace = LookupTrace
        { ltMonth = month, ltChunks = length chunks
        , ltSkippedByMonth = length chunks - length sameMonth
        , ltSkippedByRange = length sameMonth - length inRange
        , ltSkippedByErase = length gone
        , ltSkippedByFilter = length live - length maybeHere
        , ltProbed = [ (n, length <$> r) | (n, r) <- probes ]
        , ltTailEvents = length fromTail }
  pure (sortOn evSeq (fromChunks ++ fromTail), trace)

-- ---------------------------------------------------------------------------
-- For whoever folds the events into "now"

-- | The seq of the last event that is part of the world (0 if there is none).
lastSeq :: Events -> IO Word64
lastSeq k = subtract 1 <$> readIORef (xNextSeq k)

-- | Sealed events (in chunks) with seq greater than the argument, in seq order, /without the ones whose
-- id a chunk's erase record has erased/ (so a khana folded from these is a khana without them).  Call it
-- with the lock held, or before anything else can erase (as at open): it reads the manifest and the
-- records one after the other.
eventsAfter :: Events -> Word64 -> IO [Event]
eventsAfter k after = do
  acc <- newIORef []
  eventsAfterEach k after (\g -> modifyIORef' acc (g :))
  concat . reverse <$> readIORef acc

-- | The same events as 'eventsAfter', in the same order, but handed over one group at a time, so only one
-- group is in memory at once.  A group is a run of chunks whose seq ranges overlap (a chunk that starts at
-- or before the largest seq so far joins the run); groups do not overlap each other, so they come in seq
-- order and each one is sorted by seq.  Same rules for the lock as 'eventsAfter'.  A group is as big as
-- its chunks together, so chunks with wide, overlapping ranges bring the memory back up.
eventsAfterEach :: Events -> Word64 -> ([Event] -> IO ()) -> IO ()
eventsAfterEach k after each = do
  mf <- readIORef (xManifest k)
  erased <- readIORef (xErased k)
  let live = sortOn ceMinSeq [ c | c <- mfChunks mf, ceMaxSeq c > after ]
      groups = map reverse (reverse (foldl step [] live))
      step [] c = [[c]]
      step (g : gs) c
        | ceMinSeq c <= maximum (map ceMaxSeq g) = (c : g) : gs
        | otherwise                              = [c] : g : gs
  forM_ groups $ \g -> do
    evs <- forM g $ \c -> do
      cr <- readerFor k c
      let goneHere = case M.lookup (ceName c) (erByChunk erased) of
            Nothing -> const False
            Just s  -> (`Erase.member` s)
      filter (\e -> evSeq e > after && not (goneHere (evId e))) <$> readChunkEvents cr
    each (sortOn evSeq (concat evs))

-- | The unsealed batches, oldest first.  One batch = one 'record' = all or nothing.
tailBatches :: Events -> IO [[Event]]
tailBatches k = reverse . tsBatchesRev <$> readIORef (xTailSt k)

-- | Keep exactly these batches in the tail (a batch the "now" layer (Hosigraph.Khana) refused is simply
-- left out), and let their seq numbers be used again.  Call with the lock held.
resetTailTo :: Events -> [[Event]] -> IO ()
resetTailTo k keep = do
  ts <- readIORef (xTailSt k)
  mf <- readIORef (xManifest k)
  resetTail (xTail k) keep
  writeIORef (xTailSt k) (fromBatches (tsOldestMs ts) keep)
  writeIORef (xNextSeq k) (maximum (mfSealedSeq mf : [ evSeq e | b <- keep, e <- b ]) + 1)

-- ---------------------------------------------------------------------------
-- Checking

data EventsStats = EventsStats
  { xsGeneration :: Int
  , xsChunks     :: Int
  , xsSealedSeq  :: Word64
  , xsNextSeq    :: Word64
  , xsTailEvents :: Int
  , xsTailBytes  :: Integer
  , xsReadersOpen  :: Int       -- ^ chunk readers held open right now
  , xsReaderBytes  :: Int       -- ^ about how much memory they hold (see 'ecReaderCacheBytes')
  } deriving Show

eventsStats :: Events -> IO EventsStats
eventsStats k = do
  mf <- readIORef (xManifest k)
  ts <- readIORef (xTailSt k)
  ns <- readIORef (xNextSeq k)
  tb <- tailSize (xTail k)
  rk <- readIORef (xReaders k)
  pure (EventsStats (mfGeneration mf) (length (mfChunks mf)) (mfSealedSeq mf) ns (tsCount ts) tb
                    (M.size (rkMap rk)) (rkBytes rk))

-- | Check what can be checked without "now": the manifest's signature, every chunk file's
-- hash and footer, and every MPHF (each id must find exactly its own events).
-- Returns (was the manifest readable?, problems).  Call with the lock held.
verifyEvents :: Events -> IO (Bool, [String])
verifyEvents k = do
  diskMf <- try (readManifest (xDir k))
  memMf  <- readIORef (xManifest k)
  case diskMf of
    Left (e :: HosiError) -> pure (False, ["manifest: " ++ show e])
    Right mf -> do
      let p1 = [ "manifest on disk differs from the one in memory" | mf /= memMf ]
      p2 <- concat <$> forM (mfChunks mf) (checkChunk k)
      p3 <- concat <$> forM (mfChunks mf) (checkRecord k)
      pure (True, p1 ++ p2 ++ p3)

-- | A chunk's erase record, if it has one: the file is there, it is the file the manifest signed
-- (size and SHA-256), and it reads as an erase record (its own magic and CRC).
checkRecord :: Events -> ChunkEntry -> IO [String]
checkRecord k c = case ceErase c of
  Nothing -> pure []
  Just fe -> do
    let path = eraseDir k </> feName fe
        tag s = ceName c ++ ": erase record " ++ feName fe ++ " " ++ s
    exists <- doesFileExist path
    if not exists then pure [tag "is missing"] else do
      r <- try (B.readFile path)
      pure $ case r of
        Left (e :: SomeException) -> [tag ("is unreadable: " ++ show e)]
        Right bs ->
          [ tag "does not match its recorded hash/size" | B.length bs /= feBytes fe || sha256Hex bs /= feSha256 fe ]
          ++ either (\why -> [tag ("is not an erase record: " ++ why)]) (const []) (Erase.decode bs)

checkChunk :: Events -> ChunkEntry -> IO [String]
checkChunk k c = do
  let dir = chunksDir k </> ceName c
      tag s = ceName c ++ ": " ++ s
  exists <- doesDirectoryExist dir
  if not exists then pure [tag "directory is missing"] else do
    r <- try $ do
      files <- hashChunkFiles dir
      let fileProblems = [ tag ("file " ++ feName fe ++ " is missing") | fe <- ceFiles c, feName fe `notElem` [ n | (n, _, _) <- files ] ]
                      ++ [ tag ("file " ++ feName fe ++ " does not match its recorded hash/size")
                         | fe <- ceFiles c, (n, sz, sh) <- files, n == feName fe, sz /= feBytes fe || sh /= feSha256 fe ]
          hashProblem  = [ tag "chunk hash does not match its files" | chunkEntryHash (ceFiles c) /= ceHash c ]
      if not (null fileProblems) then pure (fileProblems ++ hashProblem) else do
        cr <- openChunkReader dir
        filt <- readChunkFilter dir (ceIds c)
        evs <- readChunkEvents cr
        let info = crInfo cr
            footerProblems = [ tag "footer disagrees with the manifest (counts, range, month or kind)"
                             | (ciKind info, ciMonth info, ciEvents info, ciIds info, ciMinId info, ciMaxId info, ciMinSeq info, ciMaxSeq info)
                               /= (ceKind c, ceMonth c, ceEvents c, ceIds c, ceMinId c, ceMaxId c, ceMinSeq c, ceMaxSeq c) ]
            byId = M.fromListWith (flip (++)) [ (evId e, [e]) | e <- evs ]
        mphfProblems <- fmap concat . forM (M.toList byId) $ \(i, es) -> do
          o <- lookupInChunk cr i
          pure $ case o of
            Star got | sortOn evSeq got == sortOn evSeq es -> []
                     | otherwise -> [tag ("MPHF lookup of " ++ show i ++ " returns different events than a scan")]
            other -> [tag ("MPHF lookup of " ++ show i ++ " failed: " ++ show other)]
        -- the filter may be wrong about strangers, never about an id that is there
        let filterProblems = [ tag ("the filter says no to " ++ show i ++ ", which is in the chunk") | i <- M.keys byId, not (filterMayHave filt i) ]
        pure (hashProblem ++ footerProblems ++ mphfProblems ++ filterProblems ++ [ tag "event count differs from the manifest" | length evs /= ceEvents c ])
    pure $ either (\(e :: SomeException) -> [tag ("unreadable: " ++ show e)]) id r
