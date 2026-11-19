-- | Many small worlds instead of one big one: a 'Hosi' for each region (a community, a server's rooms, a country),
-- opened by name when somebody first needs it.
--
-- What it gives, and what it does not:
--
--   * Each region has its own events, its own tail, its own lock and its own khana.  A scan (erase by owner, a
--     backfill, a rebuild of the khana) reads the size of a region and not of everything; the fsync of one region's
--     write does not stand behind another's; a region that is slow or full or broken does not make the others so.
--   * The regions share the resident JVM(s) ('rgJvms'): a JVM costs about 300 MB and some seconds to start, a database
--     inside it costs far less (see 'openHosiShared').  But one JVM is one pipe: the questions of all its regions to
--     their khanas are asked one at a time.  With 'rgJvms' above 1 the regions are spread over that many JVMs (a region
--     always goes to the same one) and ask in parallel, for the memory of one more JVM each.
--   * It is bounded: at most 'rgMaxOpen' regions are open; asking for another closes the one nobody is using that was
--     used longest ago (its data stays on disk and comes back when it is asked for).  If every open region is in use,
--     the answer is 'TooManyBusy' and nothing is closed under anyone.
--   * It does not go down: a bad name, a full set, a region that fails to open are answers ('Left'), not exceptions,
--     and the other regions go on.  And it counts, and 'regionsAdvice' says in words what the counts suggest.
--
-- A region's 'Hosi' must not be kept past the call that was given it ('withRegion'), or past 'pinRegion''s release:
-- it may be closed to make room as soon as nobody uses it.
module Hosigraph.Regions
  ( Regions
  , RegionsConfig(..)
  , defaultRegionsConfig
  , RegionError(..)
  , RegionStats(..)
  , newRegions
  , withRegion
  , pinRegion
  , validRegionName
  , regionDir
  , knownRegions
  , openRegionNames
  , regionStats
  , regionsAdvice
  , closeRegions
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception (SomeException, finally, try)
import           Control.Monad
import           Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Clock (getMonotonicTime)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath ((</>))
import           Text.Printf (printf)

import           Hosigraph

data RegionsConfig = RegionsConfig
  { rgRoot        :: FilePath      -- ^ a directory; each region is a directory of its name inside it
  , rgHosi        :: HosiConfig    -- ^ the configuration every region is opened with
  , rgSchema      :: Schema
  , rgMaxOpen     :: Int           -- ^ how many regions may be open at once (default 32)
  , rgJvms        :: Int           -- ^ over how many resident JVMs the regions are spread (default 1)
  , rgCloseWaitMs :: Int           -- ^ 'closeRegions' waits this long for regions in use, then closes them all (default 10000)
  }

defaultRegionsConfig :: FilePath -> HosiConfig -> Schema -> RegionsConfig
defaultRegionsConfig root cfg sch = RegionsConfig
  { rgRoot = root, rgHosi = cfg, rgSchema = sch, rgMaxOpen = 32, rgJvms = 1, rgCloseWaitMs = 10000 }

data RegionError
  = BadRegionName String      -- ^ the name cannot be a directory of its own (1 to 64 of A-Z a-z 0-9 . _ -, not beginning with a dot)
  | TooManyBusy Int           -- ^ that many regions are open and every one is in use; nothing was closed
  | OpenFailed String         -- ^ the region could not be opened (the reason is the string); the others are as they were
  | RegionsClosed             -- ^ 'closeRegions' has been called
  deriving Show

data RegionStats = RegionStats
  { rsOpened     :: !Int      -- ^ regions opened (a region closed to make room and opened again counts again)
  , rsHits       :: !Int      -- ^ asks answered by a region already open
  , rsEvicted    :: !Int      -- ^ regions closed to make room
  , rsBusy       :: !Int      -- ^ asks answered 'TooManyBusy'
  , rsOpenFailed :: !Int      -- ^ asks answered 'OpenFailed'
  , rsOpenMs     :: !Double   -- ^ the time spent opening, all together
  , rsOpenNow    :: !Int      -- ^ open now
  } deriving Show

data Slot = Slot { slHosi :: Hosi, slUsers :: !Int, slLast :: !Double }

data RState = RState
  { rOpen   :: !(M.Map Text Slot)
  , rClosed :: !Bool
  , rStats  :: !RegionStats
  }

data Regions = Regions
  { rgCfg      :: RegionsConfig
  , rgRuntimes :: [KhanaRuntime]
  , rgState    :: MVar RState
  }

-- | The one place a region name is checked: it becomes a directory name.
validRegionName :: Text -> Bool
validRegionName t =
  let s = T.unpack t
  in not (null s) && length s <= 64 && all ok s && take 1 s /= "."
  where ok c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '.' || c == '_' || c == '-'

regionDir :: Regions -> Text -> FilePath
regionDir rg name = rgRoot (rgCfg rg) </> T.unpack name

-- | Start the regions.  (No JVM is started until the first region is opened.)
newRegions :: RegionsConfig -> IO Regions
newRegions cfg0 = do
  let cfg = cfg0 { rgMaxOpen = max 1 (rgMaxOpen cfg0), rgJvms = max 1 (rgJvms cfg0) }
  rts <- replicateM (rgJvms cfg) (newKhanaRuntime (hcKhanaBackend (rgHosi cfg)))
  st  <- newMVar (RState M.empty False (RegionStats 0 0 0 0 0 0 0))
  pure (Regions cfg rts st)

-- | The JVM a region always goes to.
runtimeFor :: Regions -> Text -> KhanaRuntime
runtimeFor rg name =
  let h = foldl (\a c -> (a * 31 + ord c) `mod` 1000003) 7 (T.unpack name)
  in rgRuntimes rg !! (h `mod` length (rgRuntimes rg))

-- | Use a region: open it if it is not, run the action on its 'Hosi', and let it go.  An exception of the action
-- goes up as it is (the region stays open and in order).
withRegion :: Regions -> Text -> (Hosi -> IO a) -> IO (Either RegionError a)
withRegion rg name act = do
  r <- pinRegion rg name
  case r of
    Left e            -> pure (Left e)
    Right (h, unpin)  -> (Right <$> act h) `finally` unpin

-- | Take a region and hold it (it is not closed to make room) until the returned action is run.  For what has to
-- keep a 'Hosi' for a while (a write queue on it, say).
pinRegion :: Regions -> Text -> IO (Either RegionError (Hosi, IO ()))
pinRegion rg name
  | not (validRegionName name) = pure (Left (BadRegionName (show (T.unpack name))))
  | otherwise = do
      now <- getMonotonicTime
      r <- modifyMVar (rgState rg) $ \s ->
        if rClosed s then pure (s, Left RegionsClosed)
        else case M.lookup name (rOpen s) of
          Just sl -> pure ( s { rOpen = M.insert name sl { slUsers = slUsers sl + 1, slLast = now } (rOpen s)
                              , rStats = (rStats s) { rsHits = rsHits (rStats s) + 1 } }
                          , Right (slHosi sl) )
          Nothing -> openNew now name s
      pure (fmap (\h -> (h, release)) r)
  where
    release = modifyMVar_ (rgState rg) $ \s ->
      pure s { rOpen = M.adjust (\sl -> sl { slUsers = max 0 (slUsers sl - 1) }) name (rOpen s) }

    openNew now nm s
      | M.size (rOpen s) >= rgMaxOpen (rgCfg rg) =
          case sortOn (slLast . snd) [ kv | kv@(_, sl) <- M.toList (rOpen s), slUsers sl == 0 ] of
            []              -> pure (s { rStats = (rStats s) { rsBusy = rsBusy (rStats s) + 1 } }, Left (TooManyBusy (M.size (rOpen s))))
            ((victim, vs) : _) -> do
              _ <- try (closeHosi (slHosi vs)) :: IO (Either SomeException ())
              let s' = s { rOpen = M.delete victim (rOpen s), rStats = (rStats s) { rsEvicted = rsEvicted (rStats s) + 1 } }
              openNew now nm s'
      | otherwise = do
          t0 <- getMonotonicTime
          res <- try (openHosiShared (runtimeFor rg nm) (rgHosi (rgCfg rg)) (regionDir rg nm) (rgSchema (rgCfg rg)))
          t1 <- getMonotonicTime
          case res of
            Left e -> pure ( s { rStats = (rStats s) { rsOpenFailed = rsOpenFailed (rStats s) + 1 } }
                           , Left (OpenFailed (show (e :: SomeException))) )
            Right h -> pure ( s { rOpen = M.insert nm (Slot h 1 now) (rOpen s)
                                , rStats = (rStats s) { rsOpened = rsOpened (rStats s) + 1, rsOpenMs = rsOpenMs (rStats s) + (t1 - t0) * 1000 } }
                            , Right h )

-- | The regions that exist on disk under the root (open or not).
knownRegions :: Regions -> IO [Text]
knownRegions rg = do
  let root = rgRoot (rgCfg rg)
  there <- doesDirectoryExist root
  if not there then pure [] else do
    names <- listDirectory root
    fmap (map T.pack) (filterM (\n -> doesDirectoryExist (root </> n </> "events")) (filter (validRegionName . T.pack) names))

-- | The regions open now.
openRegionNames :: Regions -> IO [Text]
openRegionNames rg = withMVar (rgState rg) (pure . M.keys . rOpen)

regionStats :: Regions -> IO RegionStats
regionStats rg = withMVar (rgState rg) $ \s -> pure ((rStats s) { rsOpenNow = M.size (rOpen s) })

-- | Close everything: no new ask is taken ('RegionsClosed'), the regions in use are waited for (up to
-- 'rgCloseWaitMs', then they are closed all the same), every region is closed, and the JVMs are stopped.
closeRegions :: Regions -> IO ()
closeRegions rg = do
  modifyMVar_ (rgState rg) (\s -> pure s { rClosed = True })
  let waitLoop left = do
        busy <- withMVar (rgState rg) (pure . any ((> 0) . slUsers) . M.elems . rOpen)
        when (busy && left > 0) (threadDelay 10000 >> waitLoop (left - 10))
  waitLoop (rgCloseWaitMs (rgCfg rg))
  slots <- modifyMVar (rgState rg) (\s -> pure (s { rOpen = M.empty }, M.elems (rOpen s)))
  forM_ slots $ \sl -> (try (closeHosi (slHosi sl)) :: IO (Either SomeException ())) 
  forM_ (rgRuntimes rg) $ \rt -> (try (closeKhanaRuntime rt) :: IO (Either SomeException ()))

-- | What the counts suggest about the settings and about the shape of the work.  An empty list: nothing to say.
regionsAdvice :: RegionsConfig -> RegionStats -> [String]
regionsAdvice cfg st = concat
  [ [ show (rsBusy st) ++ " asks were turned away (TooManyBusy): all " ++ show (rgMaxOpen cfg) ++ " open regions were in use at the "
      ++ "same time.  A bigger rgMaxOpen lets more be worked on at once (each open region holds its files, a share of the reader "
      ++ "cache and a database in its JVM); if that is already as much as the memory allows, the work is spread over more "
      ++ "regions than can be served together: fewer regions, or a queue before them."
    | rsBusy st > 0 ]
  , [ printf ("Regions were closed to make room %d times against %d openings (%.0f ms each on average to open): the regions in use "
              ++ "over time are more than rgMaxOpen (%d), and each return costs an open.  A bigger rgMaxOpen stops that.")
             (rsEvicted st) (rsOpened st) (rsOpenMs st / fromIntegral (max 1 (rsOpened st))) (rgMaxOpen cfg)
    | rsOpened st >= 10, rsEvicted st * 2 >= rsOpened st ]
  , [ show (rsOpenFailed st) ++ " regions failed to open (see OpenFailed): the others went on; look at the disk (space, rights) "
      ++ "and at whether the JVM starts."
    | rsOpenFailed st > 0 ]
  , [ "All " ++ show (rsOpenNow st) ++ " open regions share one JVM, one pipe: their questions to their khanas are asked one at a "
      ++ "time.  If those questions are what limits the work, rgJvms above 1 spreads the regions over more JVMs (about 300 MB each) "
      ++ "and lets them ask in parallel; if the limit is elsewhere (fsync, the callers), it changes nothing."
    | rgJvms cfg == 1, rsOpenNow st >= 8, rsHits st >= 1000 ]
  ]
