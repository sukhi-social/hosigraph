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
-- > record ──► tail.log ──sealNow──► chunks/<name>/{chunk.hosi, mphf.idx}
-- >                                     ▲
-- >                                     └─ manifest (which chunks are the world; zone maps; hashes; signature)
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
    -- * Finding them again
  , history, historyTrace, LookupTrace(..), Probe(..)
    -- * For whoever folds the events into "now"
  , lastSeq, eventsAfter, tailBatches, resetTailTo
    -- * Checking
  , verifyEvents, EventsStats(..), eventsStats
  ) where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Word
import           System.Directory
import           System.FilePath ((</>))

import           Hosigraph.Chunk
import           Hosigraph.Event
import           Hosigraph.Id
import           Hosigraph.Manifest
import           Takka.Types (Value(..))
import           Hosigraph.Sync
import           Hosigraph.Tail

-- ---------------------------------------------------------------------------
-- Configuration and state

data EventsConfig = EventsConfig
  { ecSealEvents  :: Int              -- ^ seal when the tail holds this many events (design: still open)
  , ecSealAgeMs   :: Maybe Integer    -- ^ ...or when its oldest event is this old (checked on each record)
  , ecBlockEvents :: Int              -- ^ target events per block inside a chunk
  , ecSync        :: Bool             -- ^ fsync the tail on every record (turn off only for experiments)
  , ecClock       :: Clock            -- ^ milliseconds since the POSIX epoch (tests inject their own)
  }

defaultEventsConfig :: EventsConfig
defaultEventsConfig = EventsConfig
  { ecSealEvents = 10000, ecSealAgeMs = Nothing, ecBlockEvents = 256, ecSync = True, ecClock = systemClock }

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
  , xReaders  :: IORef (M.Map String ChunkReader)
  }

chunksDir :: Events -> FilePath
chunksDir k = xDir k </> "chunks"

-- | Ids up to this one are reserved (the "now" layer (Hosigraph.Khana) keeps its own bookkeeping on entity 1).
lastReservedId :: HosiId
lastReservedId = 1

-- ---------------------------------------------------------------------------
-- Opening and recovery

-- | Open a khana directory (creating it if new).  Recovery, in order:
--
-- 1. read the manifest (a bad signature is an error, not a guess);
-- 2. delete chunk directories the manifest does not list (a seal that died before the swap);
-- 3. read the tail, cut off a torn last record, ignore records the manifest already sealed.
--
-- (Making "now" agree with all this is the job of whoever owns "now": see 'lastSeq',
-- 'eventsAfter' and 'tailBatches'.)
openEvents :: EventsConfig -> FilePath -> IO Events
openEvents cfg dir = do
  createDirectoryIfMissing True (dir </> "chunks")
  mf <- readManifest dir
  sweepOrphans dir mf
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
  Events dir cfg <$> newMVar () <*> newIORef mf <*> pure tl <*> newIORef (fromBatches now batches)
                <*> newIORef (lastS + 1) <*> pure gen <*> newIORef M.empty

closeEvents :: Events -> IO ()
closeEvents = closeTail . xTail

withEventsLock :: Events -> IO a -> IO a
withEventsLock k act = withMVar (xLock k) (const act)

-- | Remove chunk directories (and stray temp files) that the manifest does not list.
sweepOrphans :: FilePath -> Manifest -> IO ()
sweepOrphans dir mf = do
  let known = S.fromList (map ceName (mfChunks mf))
  present <- listDirectory (dir </> "chunks")
  forM_ present $ \n -> unless (n `S.member` known) $ removePathForcibly (dir </> "chunks" </> n)
  forM_ [manifestFile ++ ".tmp", tailFile ++ ".tmp"] $ \f -> removePathForcibly (dir </> f)

readerFor :: Events -> ChunkEntry -> IO ChunkReader
readerFor k c = do
  cache <- readIORef (xReaders k)
  case M.lookup (ceName c) cache of
    Just r  -> pure r
    Nothing -> do
      r <- openChunkReader (chunksDir k </> ceName c)
      modifyIORef' (xReaders k) (M.insert (ceName c) r)
      pure r

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
-- batch or a reserved id.
record :: Events -> [Op] -> IO Recorded
record k ops = do
  [r] <- recordMany k [ops]
  either throwIO pure r

-- | Like 'record' for several batches at once: one append, one fsync.  The batches stay separate
-- (each its own record, its own @Tmp@ names, consecutive seq numbers).  A batch that cannot be
-- recorded (empty, or a reserved id) is a 'Left' in its place and does not stop the others.
-- The ones that were recorded are all-or-nothing together: 'rollback' of the first of them cuts
-- every one of them back out of the tail.
recordMany :: Events -> [[Op]] -> IO [Either HosiError Recorded]
recordMany k opss = do
  seq0 <- readIORef (xNextSeq k)
  planned <- plan seq0 opss
  sizes <- appendBatches (xTail k) [ evs | Right (_, evs) <- planned ]
  pure (attach planned sizes)
  where
    plan _ [] = pure []
    plan s (ops : rest) = case validate ops of
      Left e   -> (Left e :) <$> plan s rest
      Right () -> do
        (tmap, evs) <- resolveBatch k s ops
        (Right (tmap, evs) :) <$> plan (s + fromIntegral (length evs)) rest
    attach [] _ = []
    attach (Left e : rest) ss = Left e : attach rest ss
    attach (Right (tmap, evs) : rest) (sz : ss) = Right (Recorded tmap evs sz) : attach rest ss
    attach (Right _ : _) [] = error "recordMany: fewer sizes than batches"

validate :: [Op] -> Either HosiError ()
validate ops
  | null ops  = Left (BadArgument "write: no operations")
  | otherwise = case [ i | op <- ops, i <- idsOf op, i <= lastReservedId ] of
      (i : _) -> Left (BadArgument ("write: " ++ show i ++ " is not a valid entity id"))
      []      -> Right ()

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
      let name = "c" ++ pad 6 n ++ "-" ++ kindName kind ++ "-" ++ showMonth month
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
    pure (Just (SealReport (length evs) entries (mfGeneration mf')))
  where pad n x = let s = show x in replicate (n - length s) '0' ++ s

-- ---------------------------------------------------------------------------
-- Finding events again

-- | What a 'history' lookup did, so the design's claims can be watched happening.
data LookupTrace = LookupTrace
  { ltMonth          :: Month                 -- ^ taken from the id alone
  , ltChunks         :: Int                   -- ^ chunks in the world
  , ltSkippedByMonth :: Int                   -- ^ zone map: another month
  , ltSkippedByRange :: Int                   -- ^ zone map: right month, id outside [min, max]
  , ltProbed         :: [(String, Probe Int)] -- ^ 「星かな？」 asked of each chunk that was not skipped (Int = events found)
  , ltTailEvents     :: Int                   -- ^ events found in the unsealed tail
  } deriving Show

-- | Every event about one entity (Creates and Deletes, oldest first), from the chunks
-- (MPHF point lookup) and the unsealed tail.
history :: Events -> HosiId -> IO [Event]
history k i = fst <$> historyTrace k i

historyTrace :: Events -> HosiId -> IO ([Event], LookupTrace)
historyTrace k i = do
  -- one consistent view: a seal may move events from the tail into a new chunk at any time
  (mf, ts) <- withEventsLock k ((,) <$> readIORef (xManifest k) <*> readIORef (xTailSt k))
  let month = idMonth i
      chunks = mfChunks mf
      sameMonth = [ c | c <- chunks, ceMonth c == month ]
      inRange   = [ c | c <- sameMonth, ceMinId c <= i, i <= ceMaxId c ]
  probes <- forM inRange $ \c -> do
    cr <- readerFor k c
    r <- lookupInChunk cr i
    pure (ceName c, r)
  let fromChunks = concat [ es | (_, Star es) <- probes ]
      fromTail   = M.findWithDefault [] i (tsById ts)
      trace = LookupTrace
        { ltMonth = month, ltChunks = length chunks
        , ltSkippedByMonth = length chunks - length sameMonth
        , ltSkippedByRange = length sameMonth - length inRange
        , ltProbed = [ (n, length <$> r) | (n, r) <- probes ]
        , ltTailEvents = length fromTail }
  pure (sortOn evSeq (fromChunks ++ fromTail), trace)

-- ---------------------------------------------------------------------------
-- For whoever folds the events into "now"

-- | The seq of the last event that is part of the world (0 if there is none).
lastSeq :: Events -> IO Word64
lastSeq k = subtract 1 <$> readIORef (xNextSeq k)

-- | Sealed events (in chunks) with seq greater than the argument, in seq order.
eventsAfter :: Events -> Word64 -> IO [Event]
eventsAfter k after = do
  mf <- readIORef (xManifest k)
  sortOn evSeq . concat <$> forM [ c | c <- mfChunks mf, ceMaxSeq c > after ] (\c -> do
    cr <- readerFor k c
    filter ((> after) . evSeq) <$> readChunkEvents cr)

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
  } deriving Show

eventsStats :: Events -> IO EventsStats
eventsStats k = do
  mf <- readIORef (xManifest k)
  ts <- readIORef (xTailSt k)
  ns <- readIORef (xNextSeq k)
  tb <- tailSize (xTail k)
  pure (EventsStats (mfGeneration mf) (length (mfChunks mf)) (mfSealedSeq mf) ns (tsCount ts) tb)

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
      pure (True, p1 ++ p2)

checkChunk :: Events -> ChunkEntry -> IO [String]
checkChunk k c = do
  let dir = chunksDir k </> ceName c
      tag s = ceName c ++ ": " ++ s
  exists <- doesDirectoryExist dir
  if not exists then pure [tag "directory is missing"] else do
    r <- try $ do
      files <- hashChunkFiles dir
      let fileProblems = [ tag ("file " ++ feName fe ++ " does not match its recorded hash/size")
                         | fe <- ceFiles c, (n, sz, sh) <- files, n == feName fe, sz /= feBytes fe || sh /= feSha256 fe ]
          hashProblem  = [ tag "chunk hash does not match its files" | chunkEntryHash (ceFiles c) /= ceHash c ]
      if not (null fileProblems) then pure (fileProblems ++ hashProblem) else do
        cr <- openChunkReader dir
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
        pure (hashProblem ++ footerProblems ++ mphfProblems ++ [ tag "event count differs from the manifest" | length evs /= ceEvents c ])
    pure $ either (\(e :: SomeException) -> [tag ("unreadable: " ++ show e)]) id r
