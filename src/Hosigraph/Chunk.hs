{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Chunk
-- Description : One hot chunk = one self-contained directory: @chunk.hosi@ + @mphf.idx@.
--
-- A chunk is a frozen lump of events of one kind (Create or Delete) whose entities were
-- created in one month.  It is written once and never changed.
--
-- > chunk.hosi
-- >   header   magic, kind, month, where the body starts, attribute dictionary
-- >   body     blocks of runs;   run = all events about one entity, sorted by (attribute, seq)
-- >              run   = id:8  count:4  payloadBytes:4  { attrIdx:4 seq:8 value }*
-- >   footer   one line per block (offset, runs, events, first id, last id) + totals
-- >   trailer  footer offset, magic
-- >
-- > mphf.idx
-- >   MPHF over the entity ids, then per slot: fingerprint:4  offset-of-run:8
--
-- Point lookup (the "fast path"): MPHF gives a slot, the slot's fingerprint is compared
-- (most absent ids stop here without touching @chunk.hosi@), the run is read at its
-- offset and its id is compared once more (the last, certain check).
--
-- Not yet here, on purpose: value dictionary, compression, filters for cold chunks,
-- the erase sidecar.  The layout leaves room (the version field, and the footer).
module Hosigraph.Chunk
  ( ChunkInfo(..), writeChunk, chunkDataFile, chunkMphfFile
  , ChunkReader, crInfo, crDir, openChunkReader, readChunkEvents
  , Probe(..), lookupInChunk
  , hashChunkFiles
  ) where

import           Control.Exception (throwIO)
import           Control.Monad
import           Data.Array (Array, bounds, listArray, (!))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import           Data.Word
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.IO

import           Hosigraph.Bin
import           Hosigraph.Event
import           Hosigraph.Id (Month)
import           Hosigraph.Mphf
import           Hosigraph.Probe
import           Hosigraph.Sha256 (sha256Hex)
import           Hosigraph.Sync
import           Native.Types (Keyword(..))

chunkDataFile, chunkMphfFile :: FilePath
chunkDataFile = "chunk.hosi"
chunkMphfFile = "mphf.idx"

magicData, magicFooter, magicMphf :: B.ByteString
magicData   = "HOSICHK1"
magicFooter = "HOSIFTR1"
magicMphf   = "HOSIMPH1"

-- | What a chunk says about itself (the same numbers go into the manifest as its zone map).
data ChunkInfo = ChunkInfo
  { ciKind   :: !Kind
  , ciMonth  :: !Month
  , ciEvents :: !Int
  , ciIds    :: !Int
  , ciMinId  :: !HosiId
  , ciMaxId  :: !HosiId
  , ciMinSeq :: !Word64
  , ciMaxSeq :: !Word64
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Writing

data Block = Block { bOffset :: !Int, bRuns :: !Int, bEvents :: !Int, bFirst :: !HosiId, bLast :: !HosiId }

-- | Write the chunk's two files into @dir@ (created if needed) and make them durable.
-- @blockEvents@ is a target: a run is never split across blocks.
writeChunk :: FilePath -> Int -> Kind -> Month -> [Event] -> IO ChunkInfo
writeChunk dir blockEvents kind month@(yr, mo) events0 = do
  when (null events0) $ throwIO (BadArgument "writeChunk: no events")
  createDirectoryIfMissing True dir
  let events = sortOn (\e -> (evId e, evAttr e, evSeq e)) events0
      attrs  = M.fromList (zip (M.keys (M.fromList [ (evAttr e, ()) | e <- events ])) [0 :: Word32 ..])
      runsL  = groupRuns events
      header0 = headerBytes 0
      headerBytes bodyStart = build $
        put magicData <> putU32 1 <> putU8 (fromIntegral (fromEnum kind))
          <> putU32 (fromIntegral yr) <> putU8 (fromIntegral mo) <> putU64 bodyStart
          <> putU32 (fromIntegral (M.size attrs)) <> mconcat [ putText a | Keyword a <- M.keys attrs ]
      bodyStart = B.length header0
      header    = headerBytes (fromIntegral bodyStart)
      runBytes (i, es) =
        let payload = build (mconcat [ putU32 (attrs M.! evAttr e) <> putU64 (evSeq e) <> putValue (evVal e) | e <- es ])
        in build (putU64 (fromIntegral i) <> putU32 (fromIntegral (length es))
                    <> putU32 (fromIntegral (B.length payload)) <> put payload)
      -- lay out blocks one after another; remember where each run starts (for the MPHF slot table)
      encodeBlocks _ [] = []
      encodeBlocks off (b : rest) =
        let rbs    = map runBytes b
            starts = scanl (+) (off + 4) (map B.length rbs)
            bytes  = build (putU32 (fromIntegral (length b)) <> mconcat (map put rbs))
            blk    = Block off (length b) (sum (map (length . snd) b)) (fst (head b)) (fst (last b))
        in (bytes, blk, zip (map fst b) starts) : encodeBlocks (off + B.length bytes) rest
      encoded    = encodeBlocks bodyStart (splitBlocks blockEvents runsL)
      blocks     = [ (bytes, blk) | (bytes, blk, _) <- encoded ]
      runOffsets = concat [ offs | (_, _, offs) <- encoded ]
      body        = B.concat (map fst blocks)
      footerStart = bodyStart + B.length body
      ids         = map fst runsL
      minSeq      = minimum (map evSeq events)
      maxSeq      = maximum (map evSeq events)
      info = ChunkInfo kind month (length events) (length ids) (head ids) (last ids) minSeq maxSeq
      footer = build $
        putU32 (fromIntegral (length blocks))
          <> mconcat [ putU64 (fromIntegral (bOffset b)) <> putU32 (fromIntegral (bRuns b))
                         <> putU32 (fromIntegral (bEvents b)) <> putU64 (fromIntegral (bFirst b))
                         <> putU64 (fromIntegral (bLast b)) | (_, b) <- blocks ]
          <> putU64 (fromIntegral (ciEvents info)) <> putU64 (fromIntegral (ciIds info))
          <> putU64 (fromIntegral (ciMinId info)) <> putU64 (fromIntegral (ciMaxId info))
          <> putU64 minSeq <> putU64 maxSeq
      trailer = build (putU64 (fromIntegral footerStart) <> put magicFooter)
  BL.writeFile (dir </> chunkDataFile) (BL.fromChunks [header, body, footer, trailer])
  -- the MPHF index: function, then (fingerprint, offset) per slot
  let keys = map (fromIntegral . fst) runsL :: [Word64]
      mp   = buildMphf keys
      bySlot = IM.fromList [ (mphfSlot mp (fromIntegral i), (fingerprint (fromIntegral i), o)) | (i, o) <- runOffsets ]
      slots  = [ bySlot IM.! s | s <- [0 .. mphfSize mp - 1] ]
  BL.writeFile (dir </> chunkMphfFile) . BL.fromStrict . build $
    put magicMphf <> putU32 1 <> putMphf mp
      <> mconcat [ putU32 fp <> putU64 (fromIntegral o) | (fp, o) <- slots ]
  syncFile (dir </> chunkDataFile)
  syncFile (dir </> chunkMphfFile)
  syncDir dir
  pure info

-- | Cut the runs into blocks of about @target@ events (a run is never split).
splitBlocks :: Int -> [(HosiId, [Event])] -> [[(HosiId, [Event])]]
splitBlocks target = go [] 0
  where
    go acc _ [] = [ reverse acc | not (null acc) ]
    go acc n (r : rs) =
      let n' = n + length (snd r)
      in if n' >= target then reverse (r : acc) : go [] 0 rs else go (r : acc) n' rs

-- | Events about the same entity, in id order.
groupRuns :: [Event] -> [(HosiId, [Event])]
groupRuns [] = []
groupRuns (e : es) = let (same, rest) = span ((== evId e) . evId) es in (evId e, e : same) : groupRuns rest

-- ---------------------------------------------------------------------------
-- Reading

data ChunkReader = ChunkReader
  { crDir       :: FilePath
  , crInfo      :: ChunkInfo
  , crAttrs     :: Array Int Keyword
  , crBodyStart :: Int
  , crFooterAt  :: Int
  , crMphf      :: Mphf
  , crSlots     :: B.ByteString          -- ^ n * 12 bytes
  }

corrupt :: FilePath -> String -> IO a
corrupt dir msg = throwIO (Corrupt (dir ++ ": " ++ msg))

readAt :: FilePath -> Integer -> Int -> IO B.ByteString
readAt path off len = withBinaryFile path ReadMode $ \h -> do
  hSeek h AbsoluteSeek off
  B.hGet h len

-- | Open a chunk: reads the MPHF index, the header and the footer (not the body).
openChunkReader :: FilePath -> IO ChunkReader
openChunkReader dir = do
  let dataPath = dir </> chunkDataFile
  size <- withBinaryFile dataPath ReadMode hFileSize
  when (size < 26 + 16) $ corrupt dir "chunk.hosi is too short"
  fixed <- readAt dataPath 0 26
  (kind, month, bodyStart) <- either (corrupt dir) pure (runGet parseFixed fixed 0)
  headerBs <- readAt dataPath 0 bodyStart
  attrs <- either (corrupt dir) pure (runGet (getU32 >>= \n -> replicateM (fromIntegral n) (Keyword <$> getText)) headerBs 26)
  trailer <- readAt dataPath (size - 16) 16
  footerAt <- either (corrupt dir) pure (runGet parseTrailer trailer 0)
  when (footerAt < bodyStart || fromIntegral footerAt > size - 16) $ corrupt dir "footer offset out of range"
  footer <- readAt dataPath (fromIntegral footerAt) (fromIntegral size - 16 - footerAt)
  info <- either (corrupt dir) pure (runGet (parseFooter kind month) footer 0)
  idxBs <- B.readFile (dir </> chunkMphfFile)
  (mp, slotStart) <- either (corrupt dir) pure (runGet parseMphfHeader idxBs 0)
  let slots = B.drop slotStart idxBs
  when (B.length slots /= mphfSize mp * 12) $ corrupt dir "mphf.idx slot table has the wrong size"
  when (mphfSize mp /= ciIds info) $ corrupt dir "mphf.idx and chunk.hosi disagree on the number of ids"
  pure (ChunkReader dir info (listArray (0, length attrs - 1) attrs) bodyStart footerAt mp slots)
  where
    parseFixed = do
      m <- getRaw 8
      when (m /= magicData) (failGet "bad chunk magic")
      v <- getU32
      when (v /= 1) (failGet ("unsupported chunk version " ++ show v))
      k <- getU8
      when (k > 1) (failGet "bad kind")
      y <- getU32
      mo <- getU8
      bs <- getU64
      pure (toEnum (fromIntegral k) :: Kind, (fromIntegral y, fromIntegral mo) :: Month, fromIntegral bs :: Int)
    parseTrailer = do
      o <- getU64
      m <- getRaw 8
      when (m /= magicFooter) (failGet "bad footer magic")
      pure (fromIntegral o :: Int)
    parseFooter kind month = do
      nb <- getU32
      replicateM_ (fromIntegral nb) (getRaw (8 + 4 + 4 + 8 + 8))
      ev <- getU64; ids <- getU64; mn <- getU64; mx <- getU64; sa <- getU64; sb <- getU64
      pure (ChunkInfo kind month (fromIntegral ev) (fromIntegral ids) (fromIntegral mn) (fromIntegral mx) sa sb)
    parseMphfHeader = do
      m <- getRaw 8
      when (m /= magicMphf) (failGet "bad mphf magic")
      v <- getU32
      when (v /= 1) (failGet "unsupported mphf version")
      mp <- getMphf
      p <- getPos
      pure (mp, p)

-- | Every event in the chunk, in (id, attribute, seq) order.  Reads the whole body.
readChunkEvents :: ChunkReader -> IO [Event]
readChunkEvents cr = do
  body <- readAt (crDir cr </> chunkDataFile) (fromIntegral (crBodyStart cr)) (crFooterAt cr - crBodyStart cr)
  let kind = ciKind (crInfo cr)
      go = do
        done <- atEnd
        if done then pure [] else do
          nruns <- getU32
          rs <- replicateM (fromIntegral nruns) (getRun cr kind)
          (concat rs ++) <$> go
  either (corrupt (crDir cr)) pure (runGet go body 0)

getRun :: ChunkReader -> Kind -> Get [Event]
getRun cr kind = do
  i <- getU64
  n <- getU32
  _len <- getU32
  replicateM (fromIntegral n) $ do
    ai <- getU32
    sq <- getU64
    v  <- getValue
    checkAttr cr ai
    pure (Event sq kind (fromIntegral i) (crAttrs cr ! fromIntegral ai) v)

checkAttr :: ChunkReader -> Word32 -> Get ()
checkAttr cr ai = when (fromIntegral ai > snd (bounds (crAttrs cr))) (failGet "attribute index out of range")

-- | The fast path: MPHF slot, then 「星かな？」 ('confirmSlot': fingerprint, then the key at the slot).
lookupInChunk :: ChunkReader -> HosiId -> IO (Probe [Event])
lookupInChunk cr i
  | mphfSize (crMphf cr) == 0 = pure EmptySky
  | otherwise = do
      let key  = fromIntegral i :: Word64
          slot = mphfSlot (crMphf cr) key
          row  = B.take 12 (B.drop (slot * 12) (crSlots cr))
          (fpStored, off) = (foldl' (\a b -> a * 256 + fromIntegral b) 0 (B.unpack (B.take 4 row)) :: Word32, getBe64At row 4)
          dataPath = crDir cr </> chunkDataFile
      confirmSlot (fingerprint key) fpStored $ do
        hdr <- readAt dataPath (fromIntegral off) 16
        (rid, n, len) <- either (corrupt (crDir cr)) pure (runGet ((,,) <$> getU64 <*> getU32 <*> getU32) hdr 0)
        if rid /= key then pure Nothing else do
          payload <- readAt dataPath (fromIntegral off + 16) (fromIntegral len)
          evs <- either (corrupt (crDir cr)) pure $
            runGet (replicateM (fromIntegral n) $ do
                      ai <- getU32; sq <- getU64; v <- getValue
                      checkAttr cr ai
                      pure (Event sq (ciKind (crInfo cr)) i (crAttrs cr ! fromIntegral ai) v)) payload 0
          pure (Just evs)

-- | (file name, size, sha256) of every file in a chunk directory, for the manifest.
hashChunkFiles :: FilePath -> IO [(String, Int, String)]
hashChunkFiles dir = forM [chunkDataFile, chunkMphfFile] $ \f -> do
  bs <- B.readFile (dir </> f)
  pure (f, B.length bs, sha256Hex bs)
