-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph
-- Description : hosigraph, Phase 1 prototype: events (tail, chunks, manifest) + khana (now: Datalevin, or an in-process engine).
--
-- > import Hosigraph
-- >
-- > main = withHosi "/tmp/hosi" schema $ \h -> do
-- >   r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "Frege"))
-- >                , Create (Tmp 1) "person/age"  (Lit (VInt 70)) ]
-- >   let frege = wrTemps r M.! 1
-- >   getEntity h frege >>= print          -- "now", from the khana
-- >   _ <- seal h                          -- freeze the tail into hot chunks
-- >   history h frege >>= mapM_ print      -- every event about it, via the MPHF
--
-- A 'Hosi' (星) holds two parts, and read together they say 「ほしかな」, 「星かな？」:
--
-- * "Hosigraph.Events": the events, which are the truth (tail, chunks, manifest);
-- * "Hosigraph.Khana": 刹那 (setsuna), the instant "now" they add up to, kept in a Datalevin
--   database.  It is derived: it can be thrown away and folded again at any time.
--
-- > write ──► events/tail.log ──seal──► events/chunks/<name>/{chunk.hosi, mphf.idx, filter.bin}
-- >                 │                              │          (events/manifest lists the chunks)
-- >                 └──────────── fold ────────────┴───────► khana/  ("now", Datalog)
module Hosigraph
  ( -- * Opening
    Hosi, HosiConfig(..), defaultConfig, openHosi, openHosiWith, closeHosi, withHosi
    -- * Where "now" lives
  , KhanaBackend(..), khanaDirName
    -- * Writing
  , Op(..), Ref(..), Val(..), WriteReport(..), write, writeMany
  , Cond(..), writeIf
  , SealReport(..), seal
    -- * Erasing (an id, a span of ids, a span of time)
  , Erasure(..), EraseReport(..), erase
    -- * Letting erased things go from the disk
  , CleanOptions(..), defaultCleanOptions, CleanReport(..), clean
  , FreezeOptions(..), defaultFreezeOptions, FreezeReport(..), freeze
    -- * Reading "now" (the khana)
  , getEntity, query, queryCells
    -- * Reading many at once (one snapshot; one round trip with Datalevin)
  , getEntities, queryMany, queryCellsMany
    -- * Reading the past (events: chunks + tail, point lookup by id)
  , history, historyTrace, LookupTrace(..), Probe(..), probeSays
    -- * Keeping it honest
  , rebuildKhana, verify, Stats(..), stats
    -- * Ids
  , HosiId, describeId, idMonth, showMonth, Month, Clock, systemClock
    -- * Re-exported (the words of the khana)
  , Value(..), Keyword(..), Schema, Input(..), Cell(..), attr, ref, many, identity, uniqueValue, ValueType(..)
  , Event(..), Kind(..), HosiError(..)
  ) where

import           Control.Exception
import           Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.IORef
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Word
import           System.Directory
import           System.FilePath ((</>))

import qualified Hosigraph.Erase as Erase
import           Hosigraph.Event
import           Hosigraph.Events (Events, EventsConfig(..), EventsStats(..), LookupTrace(..), Recorded(..), SealReport(..))
import qualified Hosigraph.Events as E
import           Hosigraph.Id
import           Hosigraph.Khana
import           Hosigraph.Probe
import           Hosigraph.Sync
import           Takka.Syntax (Input(..), Cell(..))
import           Takka.Types

-- ---------------------------------------------------------------------------
-- Configuration and state

data HosiConfig = HosiConfig
  { hcSealEvents  :: Int              -- ^ seal when the tail holds this many events (design: still open)
  , hcSealAgeMs   :: Maybe Integer    -- ^ ...or when its oldest event is this old (checked on each write)
  , hcBlockEvents :: Int              -- ^ target events per block inside a chunk
  , hcColdBlockEvents :: Int          -- ^ ...inside a cold chunk (bigger: it is scanned more than it is asked for one id)
  , hcColdAfterMs :: Maybe Integer    -- ^ 'freeze' puts a (kind × month) nobody has read or written for this long into a cold chunk; 'Nothing' (the default): none is cold, and the use of units is not kept
  , hcSync        :: Bool             -- ^ fsync the tail on every write (turn off only for experiments)
  , hcKhanaBackend :: KhanaBackend    -- ^ where "now" lives: Datalevin through a resident JVM (the default; give it the real jar), or through @dtlv@
  , hcClock       :: Clock            -- ^ milliseconds since the POSIX epoch (tests inject their own)
  , hcReaderCacheBytes :: Int         -- ^ memory the open chunk readers may hold together (least recently used go first)
  }

defaultConfig :: HosiConfig
defaultConfig = HosiConfig
  { hcSealEvents = 10000, hcSealAgeMs = Nothing, hcBlockEvents = 256, hcColdBlockEvents = 4096, hcColdAfterMs = Nothing
  , hcSync = True, hcKhanaBackend = KhanaDatalevinJvm "java" "datalevin-standalone.jar", hcClock = systemClock
  , hcReaderCacheBytes = 64 * 1024 * 1024 }

eventsConfig :: HosiConfig -> EventsConfig
eventsConfig c = EventsConfig
  { ecSealEvents = hcSealEvents c, ecSealAgeMs = hcSealAgeMs c, ecBlockEvents = hcBlockEvents c, ecColdBlockEvents = hcColdBlockEvents c, ecColdAfterMs = hcColdAfterMs c
  , ecSync = hcSync c, ecClock = hcClock c, ecReaderCacheBytes = hcReaderCacheBytes c }

data Hosi = Hosi
  { hDir    :: FilePath
  , hCfg    :: HosiConfig
  , hSchema :: Schema
  , hEvents :: Events                 -- ^ the truth
  , hKhana  :: IORef Khana            -- ^ the instant (swapped when it is rebuilt)
  , hRuntime :: KhanaRuntime          -- ^ what every khana of this Hosi shares (the resident JVM, if any)
  }

khanaDir :: Hosi -> FilePath
khanaDir h = hDir h </> khanaDirName (hcKhanaBackend (hCfg h))

-- | Where a new khana is built before it replaces the live one.
freshDir :: Hosi -> FilePath
freshDir h = khanaDir h ++ ".new"

-- | Where 'verify' builds the khana it compares against.
verifyDir :: Hosi -> FilePath
verifyDir h = hDir h </> ("verify-" ++ khanaDirName (hcKhanaBackend (hCfg h)))

-- | Fail early, and without touching any khana, if the chosen backend cannot start.
checkBackend :: KhanaBackend -> IO ()
checkBackend (KhanaDtlv exe) = do
  found <- runnable exe
  unless found $ throwIO (BadArgument ("KhanaDtlv: no executable dtlv at " ++ show exe))
checkBackend (KhanaDatalevinJvm java jar) = do
  okJava <- runnable java
  unless okJava $ throwIO (BadArgument ("KhanaDatalevinJvm: no java at " ++ show java))
  okJar <- doesFileExist jar
  unless okJar $ throwIO (BadArgument ("KhanaDatalevinJvm: no Datalevin jar at " ++ show jar))

-- | A path to an executable file, or a command name found on the PATH.
runnable :: FilePath -> IO Bool
runnable exe
  | '/' `elem` exe = do e <- doesFileExist exe
                        if e then executable <$> getPermissions exe else pure False
  | otherwise      = (/= Nothing) <$> findExecutable exe

-- ---------------------------------------------------------------------------
-- Opening and recovery

openHosi :: FilePath -> Schema -> IO Hosi
openHosi = openHosiWith defaultConfig

-- | Open a database directory (creating it if new).
--
-- 1. the events recover themselves ("Hosigraph.Events": manifest, orphan chunks, torn tail);
-- 2. then the khana is made to agree with them: if it is missing, broken, or ahead of the
--    events, rebuild it; otherwise fold in whatever it has not seen yet.
openHosiWith :: HosiConfig -> FilePath -> Schema -> IO Hosi
openHosiWith cfg dir schema = do
  checkBackend (hcKhanaBackend cfg)
  createDirectoryIfMissing True dir
  rt <- newKhanaRuntime (hcKhanaBackend cfg)
  ev <- E.openEvents (eventsConfig cfg) (dir </> "events") `onException` closeKhanaRuntime rt
  (do forM_ [KhanaDtlv ""] $ \b -> removePathForcibly (dir </> (khanaDirName b ++ ".new"))
      ref0 <- newIORef (error "khana not opened yet")
      let h = Hosi dir cfg schema ev ref0 rt
      lastS <- E.lastSeq ev
      conn <- recoverKhana h lastS
      writeIORef ref0 conn
      pure h) `onException` (E.closeEvents ev >> closeKhanaRuntime rt)

closeHosi :: Hosi -> IO ()
closeHosi h = do
  E.closeEvents (hEvents h)
  readIORef (hKhana h) >>= closeKhana
  closeKhanaRuntime (hRuntime h)

withHosi :: FilePath -> Schema -> (Hosi -> IO a) -> IO a
withHosi dir schema = bracket (openHosi dir schema) closeHosi

-- | Get a khana that agrees with the events.
recoverKhana :: Hosi -> Word64 -> IO Khana
recoverKhana h lastS = do
  r <- try (openKhana (hRuntime h) (hcKhanaBackend (hCfg h)) (khanaDir h) (hSchema h))
  case r of
    Left (_ :: SomeException) -> rebuildFresh
    Right conn -> do
      (fs, gen) <- foldedState conn
      mgen <- E.eraseGeneration ev
      if fs > lastS || gen /= mgen
        then closeKhana conn >> rebuildFresh              -- the khana saw a write the events never kept, or has not followed an erase (or is ahead of one)
        else catchUp conn fs >> pure conn
  where
    ev = hEvents h
    rebuildFresh = do
      removePathForcibly (khanaDir h)
      (conn, _) <- buildKhana h (khanaDir h)
      pure conn
    catchUp conn fs = do
      sealedEvs <- E.eventsAfter ev fs
      applyBatched 2000 conn sealedEvs
      batches <- E.tailBatches ev
      refused <- foldTail conn (foldedAfter fs sealedEvs) batches
      -- a logged batch the khana refuses was never acknowledged (see 'write'): take it out of the log
      unless (null refused) $ E.resetTailTo ev [ b | b <- batches, b `notElem` refused ]

-- | Where the khana's bookkeeping stands after these events went in on top of @fs@ (what 'applyEvents'
-- wrote last), so nobody has to ask the khana again.
foldedAfter :: Word64 -> [Event] -> Word64
foldedAfter = foldl (\m e -> max m (evSeq e))

-- | Fold unsealed batches (those the khana has not seen: seq after @fs@) and return the ones it refused.
-- All of them go in one transaction first (one round trip, one fsync); only if the khana refuses that,
-- they are tried one by one to find the batch (or batches) it will not take.  A refused transaction
-- leaves the khana untouched, so the second pass starts from where the first one did.
foldTail :: Khana -> Word64 -> [[Event]] -> IO [[Event]]
foldTail conn fs batches = case [ b | b <- batches, evSeq (last b) > fs ] of
  []  -> pure []
  [b] -> one b
  bs  -> do
    ok <- try (applyEvents conn (concat bs))
    case ok of
      Right ()                  -> pure []
      Left (_ :: SomeException) -> concat <$> mapM one bs
  where
    one b = do
      ok <- try (applyEvents conn b)
      pure $ case ok of
        Right ()                  -> []
        Left (_ :: SomeException) -> [b]

-- | Build a fresh khana in @target@ from every event we know (chunks, then the tail's batches).
-- Returns the open khana and the tail batches it refused.
buildKhana :: Hosi -> FilePath -> IO (Khana, [[Event]])
buildKhana h target = do
  let ev = hEvents h
  sealedEvs <- E.eventsAfter ev 0
  conn <- openKhana (hRuntime h) (hcKhanaBackend (hCfg h)) target (hSchema h)
  applyBatched 2000 conn sealedEvs
  mgen <- E.eraseGeneration ev                         -- the events left the erased ids out, so this khana is up to date with them
  when (mgen > 0) (setEraseGen conn mgen)
  batches <- E.tailBatches ev
  refused <- foldTail conn (foldedAfter 0 sealedEvs) batches
  pure (conn, refused)

-- ---------------------------------------------------------------------------
-- Writing

data WriteReport = WriteReport
  { wrTemps    :: M.Map Int HosiId     -- ^ the fresh id given to each @Tmp n@
  , wrFirstSeq :: Word64
  , wrLastSeq  :: Word64
  , wrSealed   :: Bool                 -- ^ this write pushed the tail over its limit and a seal ran
  } deriving Show

-- | Write a batch, atomically.  The order of things:
--
-- 1. 'E.record': give fresh ids to the @Tmp@ placeholders, number the events, append the
--    batch to the tail and fsync (now it is durable);
-- 2. fold it into the khana in one transaction (this is also where bad input is found:
--    the wrong type for an attribute, a unique value taken twice, ...);
-- 3. if step 2 refused, 'E.rollback' cuts the batch back out of the tail and we throw
--    'WriteRejected': the log never keeps a batch the khana cannot take.  Otherwise 'E.commit'.
--
-- A crash between 1 and 2 is repaired at the next 'openHosi' (the batch is folded then,
-- or dropped if the khana refuses it).  Nothing was acknowledged, so either is fair.
write :: Hosi -> [Op] -> IO WriteReport
write h ops = E.withEventsLock (hEvents h) (writeLocked h ops)

-- | 'write', but only if every condition still holds in "now".  The conditions are checked with the
-- events lock held, and the batch goes in under the same lock, so no other writer can come between
-- the look and the write: what 'getEntity' showed is what the check sees.
--
-- * @Right report@: the conditions held, and the batch was written (as 'write' does).
-- * @Left conds@: these conditions did not hold (in the order given), and nothing was written (no seq
--   was used).  Read again and try again.  It is the caller's retry, not ours: this is for
--   "add one to the likes", "only if nobody has taken it", "set it up once".
-- * Bad input is an exception, as in 'write' (and a condition on a reserved id, up to 1, is a 'BadArgument').
--   If the conditions fail and the batch is also bad, you hear about the conditions first.
--
-- 'writeMany' has no conditions: a later batch may depend on what an earlier one did.
writeIf :: Hosi -> [Cond] -> [Op] -> IO (Either [Cond] WriteReport)
writeIf h conds ops = E.withEventsLock (hEvents h) $ do
  forM_ conds $ \c -> when (condId c <= 1) $
    throwIO (BadArgument ("writeIf: " ++ show (condId c) ++ " is not an entity id"))
  failed <- failedConds h conds
  if null failed then Right <$> writeLocked h ops else pure (Left failed)

condId :: Cond -> HosiId
condId (Holds i _ _) = i
condId (Lacks i _ _) = i
condId (Unset i _)   = i

-- | The conditions that do not hold now, in order.  One read of the khana, for all the entities named.
failedConds :: Hosi -> [Cond] -> IO [Cond]
failedConds _ [] = pure []
failedConds h conds = do
  conn <- readIORef (hKhana h)
  let ids = S.toAscList (S.fromList (map condId conds))
  facts <- M.fromList . zip ids <$> khanaEntities conn ids
  let valuesOf i a = M.findWithDefault [] a (M.findWithDefault M.empty i facts)
      holds c = case c of
        Holds i a v -> v `elem` valuesOf i a
        Lacks i a v -> v `notElem` valuesOf i a
        Unset i a   -> null (valuesOf i a)
  pure (filter (not . holds) conds)

-- | 'write' without taking the lock (the caller holds it).
writeLocked :: Hosi -> [Op] -> IO WriteReport
writeLocked h ops = do
  rec <- E.record ev ops
  conn <- readIORef (hKhana h)
  applied <- try (applyEvents conn (rcEvents rec))
  case applied of
    Left (ex :: SomeException) -> do
      E.rollback ev rec
      throwIO (WriteRejected (displayException ex))
    Right () -> E.commit ev rec
  due <- E.sealDue ev
  when due (void (E.sealNow ev))
  pure (reportOf rec due)
  where ev = hEvents h

reportOf :: Recorded -> Bool -> WriteReport
reportOf rec sealed = WriteReport (rcTemps rec) (evSeq (head (rcEvents rec))) (evSeq (last (rcEvents rec))) sealed

-- | Write several batches at once.  Each batch is its own batch, as if you had called 'write' for
-- it (its own @Tmp@ names, all of it or none of it); what is shared is the cost: one append and one
-- fsync of the log, one transaction in the khana (with Datalevin, one process or one round trip).
-- The answers come in the order given.
--
-- * A batch that is not valid (empty, or a reserved id) is a @Left 'BadArgument'@ in its place and
--   the others still go in.
-- * If the khana refuses the whole lot, everything is cut back out of the log and the batches are
--   written one by one, as 'write' would: the ones the khana accepts are in, and each one it refuses
--   is a @Left 'WriteRejected'@.  (A single valid batch is not tried twice.)
-- * A seal that falls due happens once, after the last batch (that report has 'wrSealed').
-- * A crash in the middle leaves a prefix of the batches or none, never half of one; nothing was
--   acknowledged, so the recovery at open is as for 'write'.
writeMany :: Hosi -> [[Op]] -> IO [Either HosiError WriteReport]
writeMany h opss = E.withEventsLock ev $ do
  recs <- E.recordMany ev opss
  case [ r | Right r <- recs ] of
    [] -> pure [ Left e | Left e <- recs ]
    good -> do
      conn <- readIORef (hKhana h)
      applied <- try (applyEvents conn (concatMap rcEvents good))
      case applied of
        Right () -> do
          mapM_ (E.commit ev) good
          due <- E.sealDue ev
          when due (void (E.sealNow ev))
          let lastGood = last good
          pure [ fmap (\r -> reportOf r (due && sameRec r lastGood)) x | x <- recs ]
        Left (ex :: SomeException) -> do
          E.rollback ev (head good)                       -- cuts out every recorded batch
          case good of
            [_] -> pure [ either Left (const (Left (WriteRejected (displayException ex)))) x | x <- recs ]
            _   -> forM (zip opss recs) $ \(ops, x) -> case x of
                     Left e  -> pure (Left e)
                     Right _ -> try (writeLocked h ops)
  where
    ev = hEvents h
    sameRec a b = rcSizeBefore a == rcSizeBefore b

-- | Freeze the tail into hot chunks: one chunk per (kind, creation month of the entity).
-- Returns 'Nothing' if the tail is empty.
seal :: Hosi -> IO (Maybe SealReport)
seal = E.seal . hEvents

-- ---------------------------------------------------------------------------
-- Erasing

-- | What to erase.  Several can be given to one 'erase'; they are joined.
data Erasure
  = EraseIds [HosiId]               -- ^ these entities (a post the user deletes later).  An id of the reserved ones (up to 1) is an error.
  | EraseRange HosiId HosiId        -- ^ every id from the first to the last, inclusive.  The reserved ids in it are left out.
  | EraseBetween UTCTime UTCTime    -- ^ every entity /created/ in @[from, to)@ (UTC).  An id is a creation time, so this is one range.
                                    --   It is the entity's creation time, not the time of an event: an edit made today to an old post
                                    --   is in the old month, so erasing that month takes the whole post.
  | EraseOwnedBy [Keyword] [HosiId] (Maybe (UTCTime, UTCTime))
                                    -- ^ every entity that has, in a Create event, one of these attributes with one of these
                                    --   ids as its value (@EraseOwnedBy [\"post/author\", \"media/owner\"] [account] Nothing@): the
                                    --   things an account owns, however far apart in time, and the ones it already deleted (their
                                    --   Creates are still in the chunks).  The window, if given, is on the entity's creation
                                    --   time, as in 'EraseBetween'.  hosigraph does not know what the attributes mean: the caller
                                    --   names them.  The account itself is not an owned thing; add @EraseIds [account]@.  It reads
                                    --   every Create chunk the window reaches, under the lock.  What other entities say about
                                    --   an erased one (a reply to a post, a mention) stays as a number.
  deriving (Eq, Show)

-- | What an 'erase' did.
data EraseReport = EraseReport
  { erGeneration :: Int          -- ^ the world's erase generation now (unchanged when there was nothing new to erase)
  , erDropped    :: [String]     -- ^ chunks taken out of the world whole: the erase covered every id they could hold
  , erRecorded   :: [String]     -- ^ chunks that got an erase record
  , erRetracted  :: Integer      -- ^ facts taken out of "now"
  , erOwned      :: [HosiId]     -- ^ the ids the 'EraseOwnedBy' parts matched, in order (also the ones erased before): for an audit line, or as the
                                 --   owners of a second call (\"the media of those posts\").  Empty when there was none.
  } deriving Show

-- | Make these entities gone: from the events (no more history, no more fold into "now"), and from "now".
-- It is one call, one new manifest, however many 'Erasure's there are.  The order, under the events lock:
--
-- 1. seal the tail ("Hosigraph.Events.eraseNow": a record cannot reach the tail), then write the records,
--    drop the chunks the erase covers whole, and swap the manifest: /that is the commit point/;
-- 2. take the entities' own facts out of the khana, and write the erase generation in the same transaction.
--
-- A crash before 1's swap: nothing happened (the seal stands on its own).  A crash after it and before 2 is
-- over: the khana's erase generation is behind the manifest's, so 'openHosi' makes the khana again from the
-- events, which leave the erased ids out.  If 2 fails without a crash, the erase is committed all the same and
-- the exception says what the khana did; reopening (or 'rebuildKhana') repairs it.
--
-- What does not go: a fact of /another/ entity that holds an erased id as its value (a reply, a mention).
-- It is a number that points at nothing.  And a write to an erased entity is refused ('BadArgument')
-- for as long as its chunk still has the record; a chunk dropped whole leaves no record, so there the id is free again.
erase :: Hosi -> [Erasure] -> IO EraseReport
erase h erasures = do
  (named, questions) <- either throwIO pure (eraseSetOf erasures)
  E.withEventsLock ev $ do
    -- the owned things are asked of the events now, under the lock, so that the answer is the one the erase acts on
    owned <- S.toAscList . S.fromList . concat <$> mapM (\(attrs, owners, ids) -> E.ownedBy ev attrs owners ids) questions
    let set = E.withoutReserved (Erase.union named (Erase.fromIds owned))
    if Erase.null set then (\g -> EraseReport g [] [] 0 owned) <$> E.eraseGeneration ev else do
      done <- E.eraseNow ev set
      let gen = E.edGeneration done
      if null (E.edDropped done) && null (E.edRecorded done)
        then pure (EraseReport gen [] [] 0 owned)               -- nothing in the world was left to erase
        else do
          conn <- readIORef (hKhana h)
          n <- eraseRanges conn gen (Erase.toRanges set)
          pure (EraseReport gen (E.edDropped done) (E.edRecorded done) n owned)
  where ev = hEvents h

-- | What the 'Erasure's name before anything is read: the ids they give outright, and the questions still to ask the
-- events (the attributes, the owners, and the range of ids created in the window).
eraseSetOf :: [Erasure] -> Either HosiError (Erase.EraseSet, [([Keyword], [HosiId], (HosiId, HosiId))])
eraseSetOf = foldM step (Erase.empty, [])
  where
    step (acc, qs) e = case e of
      EraseOwnedBy attrs owners window -> (\q -> (acc, qs ++ q)) <$> ownedQuestion attrs owners window
      _                                -> (\s -> (Erase.union acc s, qs)) <$> one e
    one (EraseIds is)
      | (i : _) <- filter (<= 1) is = Left (BadArgument ("erase: " ++ show i ++ " is not an entity id"))
      | otherwise                   = Right (Erase.fromIds is)
    one (EraseRange lo hi)
      | lo > hi   = Left (BadArgument "erase: the range ends before it starts")
      | otherwise = Right (Erase.fromRanges [(lo, hi)])
    one (EraseBetween from to)
      | to <= from = Left (BadArgument "erase: the time span ends before it starts")
      | otherwise  = Right (maybe Erase.empty (\r -> Erase.fromRanges [r]) (idRangeBetween from to))
    one (EraseOwnedBy _ _ _) = Right Erase.empty
    ownedQuestion attrs owners window = case window of
      Nothing -> Right [(attrs, owners, (minBound, maxBound))]
      Just (from, to)
        | to <= from -> Left (BadArgument "erase: the time span ends before it starts")
        | otherwise  -> Right [ (attrs, owners, r) | Just r <- [idRangeBetween from to] ]      -- a window before the first id selects nothing

-- ---------------------------------------------------------------------------
-- Letting erased things go from the disk

data CleanOptions = CleanOptions
  { coMaxChunks :: Int          -- ^ rewrite at most this many chunks in this call (a host can call 'clean' a little at a time, in each quiet moment)
  }

-- | As many chunks as there are.
defaultCleanOptions :: CleanOptions
defaultCleanOptions = CleanOptions { coMaxChunks = maxBound }

data CleanReport = CleanReport
  { crRewritten    :: [(String, Maybe String)]   -- ^ each chunk that had an erase record, and the chunk written in its place ('Nothing': nothing of it was left)
  , crSkipped      :: [String]                    -- ^ chunks it gave up on because an erase changed them meanwhile; the next call finds them
  , crRecordsLeft  :: Int                         -- ^ chunks that still have an erase record (more than 0: call again)
  , crKhanaCleaned :: Bool                        -- ^ the khana was made into a new file, because the old one may still hold what erases took out
  , crKhanaRebuilt :: Bool                        -- ^ ...and the quick way (a compacted copy) did not work, so it was folded again from the events (slow; look at why if it keeps happening)
  } deriving Show

-- | Make what 'erase' took out go from the disk.  'erase' hides an id at once (and its chunk's bytes stay in the chunk,
-- the retracted facts stay in the khana's file); this writes the chunks again without the erased events, and then
-- makes a new khana file, so that the erased values are in no file under the database directory.
--
-- 1. 'E.rewriteErased' for at most 'coMaxChunks' chunks (no lock is held across it: writes go on meanwhile);
-- 2. when no chunk has a record any more, and the khana file may still hold what an erase took out (its @clean-gen@ is
--    behind its @erase-gen@: this is also so when the erase only dropped whole chunks, which needs no rewrite), the
--    khana is made into a new file.  The quick way is a compacted copy of the khana (no replay: what is alive is
--    written, what was taken out is not); if the copy cannot be made, or does not agree with the khana (same folded
--    seq, same number of facts), the khana is folded again from the events, which leave the erased ids out
--    ('rebuildKhana'), and 'crKhanaRebuilt' says so.  This holds the events lock.
--
-- Nothing to do costs a look at the manifest and at one small fact in the khana.  There is no scheduler: call it when
-- the host is quiet.  Call it without holding any lock of this Hosi.
clean :: Hosi -> CleanOptions -> IO CleanReport
clean h opts = do
  done <- E.rewriteErased (hEvents h) (max 0 (coMaxChunks opts))
  (cleaned, rebuilt) <- if E.rwLeft done > 0 then pure (False, False) else E.withEventsLock (hEvents h) $ do
    conn <- readIORef (hKhana h)
    cg <- cleanGenOf conn
    gen <- E.eraseGeneration (hEvents h)
    if cg >= gen then pure (False, False) else do
      copied <- try (compactKhanaLocked h gen)
      case copied of
        Right ()                    -> pure (True, False)
        Left (e :: SomeException)
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e     -- being stopped is not a reason to start a long job
          | otherwise                                         -> rebuildKhanaLocked h >> pure (True, True)
  pure (CleanReport (E.rwRewritten done) (E.rwSkipped done) (E.rwLeft done) cleaned rebuilt)

-- ---------------------------------------------------------------------------
-- Letting a quiet month go cold

data FreezeOptions = FreezeOptions
  { foMaxUnits :: Int          -- ^ freeze at most this many (kind × month) units in this call
  }

-- | As many units as there are.
defaultFreezeOptions :: FreezeOptions
defaultFreezeOptions = FreezeOptions { foMaxUnits = maxBound }

data FreezeReport = FreezeReport
  { frFrozen  :: [((Kind, Month), Maybe String)]   -- ^ each unit whose hot chunks were put into one cold chunk, and that chunk ('Nothing': nothing of them was left to keep)
  , frSkipped :: [(Kind, Month)]                    -- ^ units it gave up on because an erase changed them meanwhile; the next call finds them
  } deriving Show

-- | Put the units nobody has read or written for 'hcColdAfterMs' into cold chunks: the hot chunks of a unit become one,
-- with bigger blocks and no @mphf.idx@ (a smaller footprint, and a read that asks a block instead of a slot).  What
-- 'history' answers does not change, only where it is found.  A freeze reads through the erase records, so it also finishes
-- the Erase on those units.  Without 'hcColdAfterMs' nothing is ever cold, and this does nothing.
--
-- There is no scheduler and no thaw: call it when the host is quiet, as with 'clean'; a unit that is written again gets a hot
-- chunk, which the next freeze puts into a second cold chunk.  Call it without holding any lock of this Hosi.
freeze :: Hosi -> FreezeOptions -> IO FreezeReport
freeze h opts = do
  done <- E.freezeCold (hEvents h) (max 0 (foMaxUnits opts))
  pure (FreezeReport [ (E.fzUnit d, E.fzInto d) | d <- done, not (E.fzSkipped d), not (null (E.fzMerged d)) ]
                     [ E.fzUnit d | d <- done, E.fzSkipped d ])

-- ---------------------------------------------------------------------------
-- Reading "now"

-- | Everything the khana knows about one entity: attribute -> values.
getEntity :: Hosi -> HosiId -> IO (M.Map Keyword [Value])
getEntity h i = do
  k <- readIORef (hKhana h)
  khanaEntity k i

-- | A Datalog query (EDN, Datalevin's) against "now".
query :: Hosi -> T.Text -> [Input] -> IO [[Value]]
query h txt inputs = do
  k <- readIORef (hKhana h)
  khanaQuery k txt inputs

-- | Like 'query', with @(pull ?e [...])@ allowed in @:find@.
queryCells :: Hosi -> T.Text -> [Input] -> IO [[Cell]]
queryCells h txt inputs = do
  k <- readIORef (hKhana h)
  khanaQueryCells k txt inputs

-- | Many entities, all from the same instant.  The answers are in the order asked.  With a Datalevin
-- khana this is one round trip instead of one per entity.
getEntities :: Hosi -> [HosiId] -> IO [M.Map Keyword [Value]]
getEntities h is = do
  k <- readIORef (hKhana h)
  khanaEntities k is

-- | Many queries, all against the same instant, answers in the order asked.  With a Datalevin khana
-- this is one round trip instead of one per query.  If one of them fails, the call fails (and nothing
-- is returned).
queryMany :: Hosi -> [(T.Text, [Input])] -> IO [[[Value]]]
queryMany h qs = do
  k <- readIORef (hKhana h)
  khanaQueryMany k qs

-- | Like 'queryMany', with @(pull ?e [...])@ allowed in @:find@.
queryCellsMany :: Hosi -> [(T.Text, [Input])] -> IO [[[Cell]]]
queryCellsMany h qs = do
  k <- readIORef (hKhana h)
  khanaQueryCellsMany k qs

-- ---------------------------------------------------------------------------
-- Reading the past

-- | Every event about one entity (Creates and Deletes, oldest first).
history :: Hosi -> HosiId -> IO [Event]
history = E.history . hEvents

-- | Like 'history', plus what the lookup did (which chunks were skipped by the zone map,
-- which were asked 「星かな？」 and what they answered).
historyTrace :: Hosi -> HosiId -> IO ([Event], LookupTrace)
historyTrace = E.historyTrace . hEvents

-- ---------------------------------------------------------------------------
-- Keeping it honest

-- | Throw the khana away and fold it again from chunks and tail.  Same answers, by design.
rebuildKhana :: Hosi -> IO ()
rebuildKhana h = E.withEventsLock (hEvents h) (rebuildKhanaLocked h)

-- | 'rebuildKhana' for a caller that holds the events lock.
rebuildKhanaLocked :: Hosi -> IO ()
rebuildKhanaLocked h = do
  let fresh = freshDir h
  removePathForcibly fresh
  (newConn, refused) <- buildKhana h fresh
  closeKhana newConn
  installFreshKhana h fresh
  unless (null refused) $ do
    -- same rule as at open: a logged batch the khana refuses is not part of the world
    batches <- E.tailBatches ev
    E.resetTailTo ev [ b | b <- batches, b `notElem` refused ]
  where ev = hEvents h

-- | Make the khana a compacted copy of itself, stamped as clean at erase number @gen@.  For a caller that holds the events
-- lock (so nothing is written to the khana meanwhile).  The copy is made in the 'freshDir', opened to be stamped and to be
-- compared with the live khana (same folded seq, same number of facts), and only then takes the live one's place; a copy
-- that is not made, or does not agree, is thrown away and the exception goes up, with the live khana as it was.
compactKhanaLocked :: Hosi -> Int -> IO ()
compactKhanaLocked h gen = do
  let fresh = freshDir h
  live <- readIORef (hKhana h)
  removePathForcibly fresh
  (do khanaCompactTo live fresh
      new <- openKhana (hRuntime h) (hcKhanaBackend (hCfg h)) fresh (hSchema h)
      (do setCleanGen new gen
          a <- khanaFoldedAndCount live
          b <- khanaFoldedAndCount new
          when (a /= b) $ throwIO (Corrupt ("the compacted khana does not agree with the live one: " ++ show b ++ " against " ++ show a)))
        `finally` closeKhana new) `onException` removePathForcibly fresh
  installFreshKhana h fresh

-- | Put the khana built in @fresh@ in the live one's place: let go of the live khana, move the directory, open it.
-- (The same steps whether the new file was folded from the events or copied.)
installFreshKhana :: Hosi -> FilePath -> IO ()
installFreshKhana h fresh = do
  old <- readIORef (hKhana h)
  closeKhana old
  removePathForcibly (khanaDir h)
  renameDirectory fresh (khanaDir h)
  syncDir (hDir h)
  conn <- openKhana (hRuntime h) (hcKhanaBackend (hCfg h)) (khanaDir h) (hSchema h)
  writeIORef (hKhana h) conn

-- | Check everything checkable: the manifest's signature, every chunk file's hash and
-- footer, every MPHF (each id finds exactly its own events), and that the live khana
-- equals a khana freshly folded from the events.  Empty list = all good.
verify :: Hosi -> IO [String]
verify h = E.withEventsLock (hEvents h) $ do
  (manifestOk, p1) <- E.verifyEvents (hEvents h)
  if not manifestOk then pure p1 else (p1 ++) <$> khanaCheck
  where
    khanaCheck = do
      let fresh = verifyDir h
      removePathForcibly fresh
      r <- try (buildKhana h fresh)
      case r of
        Left (e :: SomeException) -> removePathForcibly fresh >> pure ["khana: cannot rebuild: " ++ show e]
        Right (c2, _) -> do
          live <- readIORef (hKhana h)
          a <- S.fromList <$> khanaDatoms live
          b <- S.fromList <$> khanaDatoms c2
          closeKhana c2
          removePathForcibly fresh
          pure [ "khana differs from a fresh fold: " ++ show (S.size (a `S.difference` b)) ++ " extra, "
                   ++ show (S.size (b `S.difference` a)) ++ " missing" | a /= b ]

data Stats = Stats
  { stGeneration  :: Int
  , stChunks      :: Int
  , stSealedSeq   :: Word64
  , stNextSeq     :: Word64
  , stTailEvents  :: Int
  , stTailBytes   :: Integer
  , stFoldedSeq   :: Word64
  , stKhanaDatoms :: Word64
  } deriving Show

stats :: Hosi -> IO Stats
stats h = do
  es <- E.eventsStats (hEvents h)
  conn <- readIORef (hKhana h)
  (fs, n) <- khanaFoldedAndCount conn
  pure (Stats (xsGeneration es) (xsChunks es) (xsSealedSeq es) (xsNextSeq es) (xsTailEvents es) (xsTailBytes es) fs n)
