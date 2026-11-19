-- | A short memory for answers that many people ask again and again (a timeline that everyone opens).
--
-- The khana answers one thing at a time.  When a hundred people open the same page, the hundred questions are the same
-- one, and the khana should be asked once.  'cached' does that:
--
--   * An answer is kept under a /scope/ (the room, say) and a /key/ (which question about that room).  The same
--     question again, while the answer is fresh, is answered from memory.
--   * If the answer is missing and many ask at the same moment, /one/ of them goes to the khana and the others wait for
--     that answer (they are not all sent): a rush on a page nobody has asked for yet costs the khana one question.
--   * A write that changes a room calls 'invalidate' for that room: its kept answers are forgotten, and an answer that
--     was being fetched when the write came is /not/ kept (it may have been read before the write).  Other rooms keep
--     theirs.  A time limit ('rcTtlMs') is a second safety for the writes nobody told the cache about.
--   * It is bounded ('rcMaxEntries'; the oldest answers go first), it never makes the caller fail (an exception of the
--     question goes to its askers and is not kept), and it counts what it sees: 'cacheStats', and 'cacheAdvice' says in
--     words what the counts suggest about the settings and about how the questions are shaped.
--
-- It holds anything (it is typed by the caller: a key must always be asked with the same result type).  It knows
-- nothing of 'Hosi': @cached rc \"room:42\" \"newest20\" (highestIn h ...)@.
module Hosigraph.ReadCache
  ( ReadCache
  , ReadCacheConfig(..)
  , defaultReadCacheConfig
  , CacheStats(..)
  , newReadCache
  , cached
  , invalidate
  , invalidateAll
  , cacheStats
  , cacheAdvice
  ) where

import           Control.Concurrent.MVar
import           Control.Exception (SomeException, ErrorCall(..), throwIO, try)
import           Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           GHC.Clock (getMonotonicTime)
import           Text.Printf (printf)

data ReadCacheConfig = ReadCacheConfig
  { rcMaxEntries :: Int         -- ^ how many answers are kept at most (default 10000); the oldest go first
  , rcTtlMs      :: Maybe Int   -- ^ an answer older than this is not used (default 5000); 'Nothing': no limit but 'invalidate'
  } deriving Show

defaultReadCacheConfig :: ReadCacheConfig
defaultReadCacheConfig = ReadCacheConfig { rcMaxEntries = 10000, rcTtlMs = Just 5000 }

data CacheStats = CacheStats
  { csHits          :: !Int   -- ^ answered from memory
  , csShared        :: !Int   -- ^ answered by waiting for the same question another caller had already sent
  , csMisses        :: !Int   -- ^ went to the source
  , csExpired       :: !Int   -- ^ kept answers found too old
  , csInvalidations :: !Int   -- ^ calls of 'invalidate' / 'invalidateAll'
  , csForgotten     :: !Int   -- ^ answers forgotten by them
  , csStaleDropped  :: !Int   -- ^ answers not kept because their scope was invalidated while they were being fetched
  , csEvictions     :: !Int   -- ^ answers dropped for room
  , csSize          :: !Int   -- ^ kept now
  } deriving Show

data Entry = Entry { eVal :: Dynamic, eAt :: !Double }

type Gen = (Int, Int)   -- (of everything, of the scope)

data CState = CState
  { cEntries :: !(M.Map Text (M.Map Text Entry))
  , cSize    :: !Int
  , cAll     :: !Int
  , cGens    :: !(M.Map Text Int)
  , cFlight  :: !(M.Map (Text, Text) (MVar (Either SomeException Dynamic)))
  , cStats   :: !CacheStats
  }

data ReadCache = ReadCache { rcCfg :: ReadCacheConfig, rcState :: MVar CState }

newReadCache :: ReadCacheConfig -> IO ReadCache
newReadCache cfg0 = do
  let cfg = cfg0 { rcMaxEntries = max 1 (rcMaxEntries cfg0) }
  ReadCache cfg <$> newMVar (CState M.empty 0 0 M.empty M.empty (CacheStats 0 0 0 0 0 0 0 0 0))

data Decision = Hit Dynamic | Wait (MVar (Either SomeException Dynamic)) | Lead (MVar (Either SomeException Dynamic)) Gen

-- | The answer for this scope and key: from memory when it is there and fresh, else from the action (once, however
-- many ask at the same time).
cached :: Typeable a => ReadCache -> Text -> Text -> IO a -> IO a
cached rc scope key act = do
  now <- getMonotonicTime
  decision <- modifyMVar (rcState rc) $ \s -> case lookupFresh rc now scope key s of
    (Just d, s')  -> pure (bump (\st -> st { csHits = csHits st + 1 }) s', Hit d)
    (Nothing, s') -> case M.lookup (scope, key) (cFlight s') of
      Just box -> pure (bump (\st -> st { csShared = csShared st + 1 }) s', Wait box)
      Nothing  -> do
        box <- newEmptyMVar
        pure ( bump (\st -> st { csMisses = csMisses st + 1 }) s' { cFlight = M.insert (scope, key) box (cFlight s') }
             , Lead box (genOf scope s') )
  case decision of
    Hit d     -> typed d
    Wait box  -> readMVar box >>= either throwIO typed
    Lead box g0 -> do
      r <- try act
      case r of
        Right v -> do
          done <- getMonotonicTime
          modifyMVar_ (rcState rc) $ \s ->
            let s1 = s { cFlight = M.delete (scope, key) (cFlight s) }
            in pure (if genOf scope s1 == g0 then store rc done scope key (toDyn v) s1
                     else bump (\st -> st { csStaleDropped = csStaleDropped st + 1 }) s1)
          putMVar box (Right (toDyn v))
          pure v
        Left e -> do
          modifyMVar_ (rcState rc) $ \s -> pure s { cFlight = M.delete (scope, key) (cFlight s) }
          putMVar box (Left (e :: SomeException))
          throwIO e
  where
    typed d = case fromDynamic d of
      Just v  -> pure v
      Nothing -> throwIO (ErrorCall ("ReadCache: the key " ++ show (scope, key) ++ " was asked with two different result types"))

-- | Forget the kept answers of one scope (a write changed it), and do not keep answers being fetched for it now.
invalidate :: ReadCache -> Text -> IO ()
invalidate rc scope = modifyMVar_ (rcState rc) $ \s ->
  let gone = maybe 0 M.size (M.lookup scope (cEntries s))
  in pure (bump (\st -> st { csInvalidations = csInvalidations st + 1, csForgotten = csForgotten st + gone }) s
             { cEntries = M.delete scope (cEntries s), cSize = cSize s - gone
             , cGens = M.insertWith (+) scope 1 (cGens s) })

-- | Forget everything.
invalidateAll :: ReadCache -> IO ()
invalidateAll rc = modifyMVar_ (rcState rc) $ \s ->
  pure (bump (\st -> st { csInvalidations = csInvalidations st + 1, csForgotten = csForgotten st + cSize s }) s
          { cEntries = M.empty, cSize = 0, cAll = cAll s + 1 })

cacheStats :: ReadCache -> IO CacheStats
cacheStats rc = withMVar (rcState rc) $ \s -> pure ((cStats s) { csSize = cSize s })

-- ---------------------------------------------------------------- inside

bump :: (CacheStats -> CacheStats) -> CState -> CState
bump f s = s { cStats = f (cStats s) }

genOf :: Text -> CState -> Gen
genOf scope s = (cAll s, M.findWithDefault 0 scope (cGens s))

lookupFresh :: ReadCache -> Double -> Text -> Text -> CState -> (Maybe Dynamic, CState)
lookupFresh rc now scope key s = case M.lookup scope (cEntries s) >>= M.lookup key of
  Nothing -> (Nothing, s)
  Just e
    | fresh e   -> (Just (eVal e), s)
    | otherwise ->
        let inner = M.delete key (M.findWithDefault M.empty scope (cEntries s))
            es    = if M.null inner then M.delete scope (cEntries s) else M.insert scope inner (cEntries s)
        in (Nothing, bump (\st -> st { csExpired = csExpired st + 1 }) s { cEntries = es, cSize = cSize s - 1 })
  where
    fresh e = case rcTtlMs (rcCfg rc) of
      Nothing  -> True
      Just ttl -> (now - eAt e) * 1000 <= fromIntegral ttl

store :: ReadCache -> Double -> Text -> Text -> Dynamic -> CState -> CState
store rc now scope key d s0 =
  let s1 = if cSize s0 >= rcMaxEntries (rcCfg rc) then evict rc s0 else s0
      inner = M.findWithDefault M.empty scope (cEntries s1)
      new   = not (M.member key inner)
  in s1 { cEntries = M.insert scope (M.insert key (Entry d now) inner) (cEntries s1)
        , cSize = cSize s1 + (if new then 1 else 0) }

-- | Drop the oldest tenth (and at least one).
evict :: ReadCache -> CState -> CState
evict rc s =
  let all' = sortOn (\(t, _, _) -> t) [ (eAt e, sc, k) | (sc, m) <- M.toList (cEntries s), (k, e) <- M.toList m ]
      n    = max 1 (rcMaxEntries (rcCfg rc) `div` 10)
      drop' es (_, sc, k) = M.update (\m -> let m' = M.delete k m in if M.null m' then Nothing else Just m') sc es
      es'  = foldl drop' (cEntries s) (take n all')
      gone = min n (length all')
  in bump (\st -> st { csEvictions = csEvictions st + gone }) s { cEntries = es', cSize = cSize s - gone }

-- ---------------------------------------------------------------- advice

-- | What the counts suggest about the settings and about how the questions are shaped.  An empty list: nothing to say.
cacheAdvice :: ReadCacheConfig -> CacheStats -> [String]
cacheAdvice cfg st = concat
  [ [ printf "Only %.0f%% of %d lookups were answered from memory or by sharing: caching gains little here. If each \
             \question is asked once, there is nothing to share; if the same ones come back but are forgotten at once, \
             \the writes invalidate faster than the reads repeat (is the scope wider than it needs to be?) or the time \
             \limit (%s) is too short." (hitRate * 100) lookups (maybe "none" (\t -> show t ++ " ms") (rcTtlMs cfg))
    | lookups >= 100, hitRate < 0.2 ]
  , [ show (csEvictions st) ++ " answers were dropped for room (rcMaxEntries = " ++ show (rcMaxEntries cfg) ++ "): the cache is full of "
      ++ "answers that are not asked again soon.  A bigger number keeps more; a coarser key (fewer different questions per scope) "
      ++ "would also keep more of what repeats."
    | csEvictions st > 0 ]
  , [ show (csStaleDropped st) ++ " answers were not kept because a write to their scope came while they were being fetched: "
      ++ "those scopes are written about as fast as they are read, and a cache gains little on them (the khana is asked each time)."
    | csStaleDropped st >= 10, csStaleDropped st * 5 >= csMisses st ]
  , [ show (csExpired st) ++ " kept answers were too old when asked for again (time limit " ++ maybe "none" (\t -> show t ++ " ms") (rcTtlMs cfg)
      ++ "): if writes always call invalidate, a longer limit loses nothing and saves questions to the khana."
    | csExpired st >= 20, csExpired st * 3 >= csMisses st, csInvalidations st > 0 ]
  ]
  where
    lookups = csHits st + csShared st + csMisses st
    hitRate = fromIntegral (csHits st + csShared st) / fromIntegral (max 1 lookups) :: Double
