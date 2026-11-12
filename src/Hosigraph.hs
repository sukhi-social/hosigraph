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
  , SealReport(..), seal
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
import           Data.Word
import           System.Directory
import           System.FilePath ((</>))

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
  , hcSync        :: Bool             -- ^ fsync the tail on every write (turn off only for experiments)
  , hcKhanaBackend :: KhanaBackend    -- ^ where "now" lives: Datalevin through a resident JVM (the default; give it the real jar), or through @dtlv@
  , hcClock       :: Clock            -- ^ milliseconds since the POSIX epoch (tests inject their own)
  , hcReaderCacheBytes :: Int         -- ^ memory the open chunk readers may hold together (least recently used go first)
  }

defaultConfig :: HosiConfig
defaultConfig = HosiConfig
  { hcSealEvents = 10000, hcSealAgeMs = Nothing, hcBlockEvents = 256
  , hcSync = True, hcKhanaBackend = KhanaDatalevinJvm "java" "datalevin-standalone.jar", hcClock = systemClock
  , hcReaderCacheBytes = 64 * 1024 * 1024 }

eventsConfig :: HosiConfig -> EventsConfig
eventsConfig c = EventsConfig
  { ecSealEvents = hcSealEvents c, ecSealAgeMs = hcSealAgeMs c, ecBlockEvents = hcBlockEvents c
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
      fs <- foldedSeq conn
      if fs > lastS
        then closeKhana conn >> rebuildFresh              -- the khana saw a write the events never kept
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
rebuildKhana h = E.withEventsLock ev $ do
  let fresh = freshDir h
  removePathForcibly fresh
  (newConn, refused) <- buildKhana h fresh
  old <- readIORef (hKhana h)
  closeKhana newConn
  closeKhana old
  removePathForcibly (khanaDir h)
  renameDirectory fresh (khanaDir h)
  syncDir (hDir h)
  conn <- openKhana (hRuntime h) (hcKhanaBackend (hCfg h)) (khanaDir h) (hSchema h)
  writeIORef (hKhana h) conn
  unless (null refused) $ do
    -- same rule as at open: a logged batch the khana refuses is not part of the world
    batches <- E.tailBatches ev
    E.resetTailTo ev [ b | b <- batches, b `notElem` refused ]
  where ev = hEvents h

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
