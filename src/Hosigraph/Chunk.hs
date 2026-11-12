{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Chunk
-- Description : One hot chunk = one self-contained directory: @chunk.hosi@ + @mphf.idx@ + @filter.bin@.
--
-- A chunk is a frozen lump of events of one kind (Create or Delete) whose entities were
-- created in one month.  It is written once and never changed.
--
-- > chunk.hosi
-- >   header   magic, version, kind, month, where the body starts, attribute dictionary,
-- >            (version 2:) value dictionary
-- >   body     blocks of runs;   run = all events about one entity, sorted by (attribute, seq)
-- >              version 1 (fixed-width numbers, no dictionary):
-- >              run   = id:8  count:4  payloadBytes:4  { attrIdx:4 seq:8 value }*
-- >              value = the same bytes as in the tail log ('putValue')
-- >
-- >              version 2 writes every number as a variable-length one ('putVar', 1-10 bytes):
-- >              run   = (id - first id) count payloadBytes  { attrIdx (seq - first seq) value }*
-- >              value = tag, then an integer as zigzag, a text as length and bytes, @5 dictIdx@ for the dictionary
-- >   footer   one line per block (offset, runs, events, first id, last id) + totals
-- >   trailer  footer offset, magic
-- >
-- > mphf.idx
-- >   MPHF over the entity ids, then per slot: fingerprint:4  offset-of-run:8
-- >
-- > filter.bin
-- >   magic, version, a Binary Fuse filter ("Hosigraph.BinaryFuse") over the entity ids
--
-- Point lookup (the "fast path"): MPHF gives a slot, the slot's fingerprint is compared
-- (most absent ids stop here without touching @chunk.hosi@), the run is read at its
-- offset and its id is compared once more (the last, certain check).
--
-- /The filter./  @filter.bin@ is a guess made before all of that: 'filterMayHave' says \"certainly not\"
-- for almost every id that is not in the chunk, and costs three reads from a small table.  For a chunk
-- we hold in hand the MPHF's fingerprint does much the same, so the filter earns its place when the
-- rest of the chunk is far away (the deep path).  A chunk written before filters existed has none;
-- it is read as before.
--
-- /The deep path./  A reader does not need the whole directory.  It asks a 'ChunkSource' for a few ranges
-- (the 26-byte start, the header, the trailer, the footer: the footer has one line per block with the
-- first and last id in it) and, if the source has no @mphf.idx@, finds the one block that may hold an id
-- by binary search over those lines and reads only that block.  So a lookup in a chunk of 4,000 ids is
-- one range of about 2 KB out of 40 KB, with no MPHF at all.  A source that is a bucket in S3 would answer
-- these ranges with Range GET; 'localSource' answers them from a directory.
--
-- /The value dictionary./  A value that the chunk says many times (a type name, a language tag,
-- the same IRI for thousands of entities) is written once in the header; the runs say
-- \"number 17\" instead.  A value goes in only when that makes the chunk smaller
-- (see 'worthIt').  The dictionary is read when the chunk is opened and stays in memory with the
-- reader, so it is counted in 'readerBytes'.  Version 1 chunks (no dictionary) are still read.
--
-- Not yet here, on purpose: compression, an S3 'ChunkSource', the erase sidecar.
-- The layout leaves room (the version field, and the footer).
module Hosigraph.Chunk
  ( ChunkInfo(..), writeChunk, writeChunkVersion, chunkDataFile, chunkMphfFile, chunkFilterFile
  , ChunkSource(..), localSource
  , ChunkReader, crInfo, crDir, readerBytes, openChunkReader, openChunkReaderWith, readChunkEvents
  , readChunkFilter, readChunkFilterWith, filterMayHave
  , Probe(..), lookupInChunk
  , hashChunkFiles
  ) where

import           Control.Exception (throwIO)
import           Control.Monad
import           Data.Array (Array, bounds, listArray, rangeSize, (!))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.List (sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import           Data.Word
import           GHC.Float (castDoubleToWord64, castWord64ToDouble)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath ((</>))
import           System.IO

import           Hosigraph.Bin
import           Hosigraph.BinaryFuse
import           Hosigraph.Event
import           Hosigraph.Id (Month)
import           Hosigraph.Mphf
import           Hosigraph.Probe
import           Hosigraph.Sha256 (sha256Hex)
import           Hosigraph.Sync
import           Takka.Types (Keyword(..), Value(..))

chunkDataFile, chunkMphfFile, chunkFilterFile :: FilePath
chunkDataFile   = "chunk.hosi"
chunkMphfFile   = "mphf.idx"
chunkFilterFile = "filter.bin"

magicData, magicFooter, magicMphf, magicFilter :: B.ByteString
magicData   = "HOSICHK1"
magicFooter = "HOSIFTR1"
magicMphf   = "HOSIMPH1"
magicFilter = "HOSIFLT1"

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
writeChunk = writeChunkVersion 2

-- | 'writeChunk' for a chosen format version: 2 is the current one (variable-length numbers and
-- the value dictionary), and 1 is the oldest (fixed-width numbers, no dictionary).
-- Version 1 is here to make old-format chunks for tests and for measuring, and for
-- nothing else; any other number is a 'BadArgument'.
writeChunkVersion :: Word32 -> FilePath -> Int -> Kind -> Month -> [Event] -> IO ChunkInfo
writeChunkVersion version dir blockEvents kind month@(yr, mo) events0 = do
  when (null events0) $ throwIO (BadArgument "writeChunk: no events")
  when (version `notElem` [1, 2]) $ throwIO (BadArgument ("writeChunk: no such chunk version " ++ show version))
  createDirectoryIfMissing True dir
  let events = sortOn (\e -> (evId e, evAttr e, evSeq e)) events0
      attrs  = M.fromList (zip (M.keys (M.fromList [ (evAttr e, ()) | e <- events ])) [0 :: Word32 ..])
      runsL  = groupRuns events
      -- Values are told apart by their bytes, not by (==): 0.0 and -0.0 are equal but not the same value.
      v2     = version >= 2
      valueB = if v2 then putValue2 else putValue                -- how this version spells a plain value
      refB ix = putU8 tagDict <> putVar (fromIntegral ix)        -- (version 1 has no dictionary, so this is never used there)
      dictL  = if v2 then dictionaryFor valueB [ evVal e | e <- events ] else []
      dictIx = M.fromList (zip (map (build . valueB) dictL) [0 :: Word32 ..])
      putChunkValue v =
        let enc = build (valueB v)
        in maybe (put enc) refB (M.lookup enc dictIx)
      -- version 2 counts from the chunk's first id and first seq: smaller numbers, shorter bytes
      -- (wrapping Word64 arithmetic, so the way back is exact whatever the ids are)
      baseId  = fromIntegral (fst (head runsL)) :: Word64
      baseSeq = minimum (map evSeq events)
      header0 = headerBytes 0
      headerBytes bodyStart = build $
        put magicData <> putU32 version <> putU8 (fromIntegral (fromEnum kind))
          <> putU32 (fromIntegral yr) <> putU8 (fromIntegral mo) <> putU64 bodyStart
          <> putU32 (fromIntegral (M.size attrs)) <> mconcat [ putText a | Keyword a <- M.keys attrs ]
          <> (if v2 then putU32 (fromIntegral (length dictL)) <> mconcat (map valueB dictL) else mempty)
      bodyStart = B.length header0
      header    = headerBytes (fromIntegral bodyStart)
      runBytes (i, es)
        | v2 =
            let payload = build (mconcat [ putVar (fromIntegral (attrs M.! evAttr e)) <> putVar (evSeq e - baseSeq) <> putChunkValue (evVal e) | e <- es ])
            in build (putVar (fromIntegral i - baseId) <> putVar (fromIntegral (length es))
                        <> putVar (fromIntegral (B.length payload)) <> put payload)
        | otherwise =
            let payload = build (mconcat [ putU32 (attrs M.! evAttr e) <> putU64 (evSeq e) <> putChunkValue (evVal e) | e <- es ])
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
  -- the filter: a first guess about an id, before anything else is read
  BL.writeFile (dir </> chunkFilterFile) . BL.fromStrict . build $
    put magicFilter <> putU32 1 <> putBinaryFuse (buildBinaryFuse keys)
  syncFile (dir </> chunkDataFile)
  syncFile (dir </> chunkMphfFile)
  syncFile (dir </> chunkFilterFile)
  syncDir dir
  pure info

-- | The tag byte that means "a value from the dictionary" (0-4 are the plain values, see 'putValue').
tagDict :: Word8
tagDict = 5

-- | The values worth a place in the dictionary, in a fixed order (the same input gives the same
-- chunk).  A reference costs 'refCost' bytes (a little over the usual 2-3 for tag and number); the
-- entry costs the value's own bytes once.  So a value that is @len@ bytes and appears @n@ times is
-- worth it when @n * (len - refCost) > len@.
dictionaryFor :: (Value -> Builder') -> [Value] -> [Value]
dictionaryFor spell vals = [ v | (_, (v, n, len)) <- M.toAscList counted, worthIt n len ]
  where
    counted = M.fromListWith (\(v, a, l) (_, b, _) -> (v, a + b, l))
                [ (enc, (v, 1 :: Int, B.length enc)) | v <- vals, let enc = build (spell v) ]

-- | What a reference to the dictionary is counted as costing (tag, and the number as a varint).
refCost :: Int
refCost = 4

worthIt :: Int -> Int -> Bool
worthIt n len = n * (len - refCost) > len

-- | A plain value in version 2: the same tags as 'putValue', but numbers and lengths are
-- variable-length (a small integer is 2 bytes, not 9; a short string's length is 1 byte, not 4).
putValue2 :: Value -> Builder'
putValue2 v = case v of
  VInt n          -> putU8 0 <> putVar (zigzag n)
  VDouble d       -> putU8 1 <> putU64 (castDoubleToWord64 d)
  VStr t          -> putU8 2 <> putVarText t
  VBool b         -> putU8 3 <> putU8 (if b then 1 else 0)
  VKw (Keyword t) -> putU8 4 <> putVarText t

getValueBody2 :: Word8 -> Get Value
getValueBody2 tag = case tag of
  0 -> VInt . unzigzag <$> getVar
  1 -> VDouble . castWord64ToDouble <$> getU64
  2 -> VStr <$> getVarText
  3 -> VBool . (/= 0) <$> getU8
  4 -> VKw . Keyword <$> getVarText
  _ -> failGet ("unknown value tag " ++ show tag)

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

-- | Where a chunk's bytes come from.  A directory on disk is one ('localSource'); a prefix in an S3
-- bucket is another, where 'csRange' is a Range GET.  A reader asks for as little as it can: a few
-- small ranges of @chunk.hosi@ when it opens (the header, the trailer, the footer), then one block for each lookup.
data ChunkSource = ChunkSource
  { csName     :: String                                          -- ^ for messages: the directory, or the object prefix
  , csRange    :: FilePath -> Integer -> Int -> IO B.ByteString   -- ^ @csRange file offset len@: those bytes of one file of the chunk (fewer if the file ends first)
  , csWhole    :: FilePath -> IO (Maybe B.ByteString)             -- ^ a small file all at once, or 'Nothing' if the chunk does not have it
  , csDataSize :: IO Integer                                      -- ^ how many bytes @chunk.hosi@ has (the manifest knows, so a remote source needs no request)
  }

-- | A chunk in a directory on disk.
localSource :: FilePath -> ChunkSource
localSource dir = ChunkSource
  { csName     = dir
  , csRange    = \f off len -> readAt (dir </> f) off len
  , csWhole    = \f -> do
      there <- doesFileExist (dir </> f)
      if there then Just <$> B.readFile (dir </> f) else pure Nothing
  , csDataSize = withBinaryFile (dir </> chunkDataFile) ReadMode hFileSize }

data ChunkReader = ChunkReader
  { crDir       :: String                              -- ^ the source's name (for messages)
  , crRead      :: Integer -> Int -> IO B.ByteString   -- ^ bytes of @chunk.hosi@
  , crVersion   :: Word32
  , crInfo      :: ChunkInfo
  , crAttrs     :: Array Int Keyword
  , crDict      :: Array Int Value       -- ^ the value dictionary (empty for a version 1 chunk)
  , crBodyStart :: Int
  , crFooterAt  :: Int
  , crBlocks    :: Array Int (Int, HosiId, HosiId)   -- ^ per block, from the footer: where it starts, its first and its last entity id
  , crIndex     :: Maybe (Mphf, B.ByteString)        -- ^ the MPHF and its slot table (n * 12 bytes); none when the source has no @mphf.idx@
  }

-- | About how much memory an open reader holds: the slot table (12 bytes per id), the block table
-- (32 bytes per block) and the header (the attribute names and the value dictionary, which is as big
-- as its values).  The MPHF's own table is small beside these.
readerBytes :: ChunkReader -> Int
readerBytes cr = maybe 0 (B.length . snd) (crIndex cr) + 32 * rangeSize (bounds (crBlocks cr)) + crBodyStart cr + 256   -- the header holds both dictionaries

corrupt :: String -> String -> IO a
corrupt dir msg = throwIO (Corrupt (dir ++ ": " ++ msg))

readAt :: FilePath -> Integer -> Int -> IO B.ByteString
readAt path off len = withBinaryFile path ReadMode $ \h -> do
  hSeek h AbsoluteSeek off
  B.hGet h len

-- | Open a chunk on disk: reads the MPHF index, the header and the footer (not the body).
openChunkReader :: FilePath -> IO ChunkReader
openChunkReader = openChunkReaderWith . localSource

-- | Open a chunk from any source.  It reads the header, the trailer and the footer of @chunk.hosi@
-- (three ranges after the fixed first 26 bytes), and the MPHF index if the source has one.  Without the
-- index the reader still answers: 'lookupInChunk' finds the one block that could hold the id from the
-- footer's block table and reads only that block.
openChunkReaderWith :: ChunkSource -> IO ChunkReader
openChunkReaderWith src = do
  let dir = csName src
      rd  = csRange src chunkDataFile
  size <- csDataSize src
  when (size < 26 + 16) $ corrupt dir "chunk.hosi is too short"
  fixed <- rd 0 26
  (version, kind, month, bodyStart) <- either (corrupt dir) pure (runGet parseFixed fixed 0)
  headerBs <- rd 0 bodyStart
  (attrs, dict) <- either (corrupt dir) pure (runGet (parseNames version) headerBs 26)
  trailer <- rd (size - 16) 16
  footerAt <- either (corrupt dir) pure (runGet parseTrailer trailer 0)
  when (footerAt < bodyStart || fromIntegral footerAt > size - 16) $ corrupt dir "footer offset out of range"
  footer <- rd (fromIntegral footerAt) (fromIntegral size - 16 - footerAt)
  (info, blockList) <- either (corrupt dir) pure (runGet (parseFooter kind month) footer 0)
  -- blocks lie in order inside the body, and their id ranges do not overlap (a run is never split)
  let inOrder = and [ o1 < o2 && l1 < f2 | ((o1, _, l1), (o2, f2, _)) <- zip blockList (drop 1 blockList) ]
      sane    = and [ f <= l && o >= bodyStart && o < footerAt | (o, f, l) <- blockList ]
  unless (inOrder && sane) $ corrupt dir "the footer's block table is out of order"
  index <- csWhole src chunkMphfFile >>= \found -> case found of
    Nothing    -> pure Nothing
    Just idxBs -> do
      (mp, slotStart) <- either (corrupt dir) pure (runGet parseMphfHeader idxBs 0)
      let slots = B.drop slotStart idxBs
      when (B.length slots /= mphfSize mp * 12) $ corrupt dir "mphf.idx slot table has the wrong size"
      when (mphfSize mp /= ciIds info) $ corrupt dir "mphf.idx and chunk.hosi disagree on the number of ids"
      pure (Just (mp, slots))
  pure (ChunkReader dir rd version info (listArray (0, length attrs - 1) attrs) (listArray (0, length dict - 1) dict)
                    bodyStart footerAt (listArray (0, length blockList - 1) blockList) index)
  where
    parseFixed = do
      m <- getRaw 8
      when (m /= magicData) (failGet "bad chunk magic")
      v <- getU32
      when (v < 1 || v > 2) (failGet ("unsupported chunk version " ++ show v))
      k <- getU8
      when (k > 1) (failGet "bad kind")
      y <- getU32
      mo <- getU8
      bs <- getU64
      pure (v, toEnum (fromIntegral k) :: Kind, (fromIntegral y, fromIntegral mo) :: Month, fromIntegral bs :: Int)
    -- the attribute names, then (version 2) the value dictionary; nothing may be left over
    parseNames version = do
      attrs <- getU32 >>= \n -> replicateM (fromIntegral n) (Keyword <$> getText)
      dict  <- if version >= 2 then getU32 >>= \n -> replicateM (fromIntegral n) (getU8 >>= getValueBody2) else pure []
      end   <- atEnd
      unless end (failGet "the header has bytes left over")
      pure (attrs, dict)
    parseTrailer = do
      o <- getU64
      m <- getRaw 8
      when (m /= magicFooter) (failGet "bad footer magic")
      pure (fromIntegral o :: Int)
    -- one line per block: (offset, runs, events, first id, last id); then the totals
    parseFooter kind month = do
      nb <- getU32
      blockList <- replicateM (fromIntegral nb) $ do
        off <- getU64; _runs <- getU32; _evs <- getU32; first <- getU64; lst <- getU64
        pure (fromIntegral off :: Int, fromIntegral first :: HosiId, fromIntegral lst :: HosiId)
      ev <- getU64; ids <- getU64; mn <- getU64; mx <- getU64; sa <- getU64; sb <- getU64
      pure (ChunkInfo kind month (fromIntegral ev) (fromIntegral ids) (fromIntegral mn) (fromIntegral mx) sa sb, blockList)
    parseMphfHeader = do
      m <- getRaw 8
      when (m /= magicMphf) (failGet "bad mphf magic")
      v <- getU32
      when (v /= 1) (failGet "unsupported mphf version")
      mp <- getMphf
      p <- getPos
      pure (mp, p)

-- | The chunk's filter, if it has one: a chunk written before filters existed has no @filter.bin@.
-- A filter that is there but wrong (bad bytes, or built from another number of ids) is 'Corrupt'.
-- The filter is read on its own, before (and without) opening the reader: it is what tells you
-- whether the rest of the chunk is worth a request.
readChunkFilter :: FilePath -> Int -> IO (Maybe BinaryFuse)
readChunkFilter = readChunkFilterWith . localSource

-- | 'readChunkFilter' from any source.  @ids@ is how many entity ids the chunk has (the manifest says).
readChunkFilterWith :: ChunkSource -> Int -> IO (Maybe BinaryFuse)
readChunkFilterWith src ids = do
  let dir = csName src
  csWhole src chunkFilterFile >>= \found -> case found of
    Nothing -> pure Nothing
    Just bs -> do
      f <- either (corrupt dir . ("filter.bin: " ++)) pure (runGet parseFilter bs 0)
      when (bfKeys f /= ids) $ corrupt dir "filter.bin and chunk.hosi disagree on the number of ids"
      pure (Just f)
  where
    parseFilter = do
      m <- getRaw 8
      when (m /= magicFilter) (failGet "bad filter magic")
      v <- getU32
      when (v /= 1) (failGet "unsupported filter version")
      f <- getBinaryFuse
      end <- atEnd
      unless end (failGet "the file has bytes left over")
      pure f

-- | The filter's guess: @False@ means the id is certainly not in the chunk; @True@ means it may be
-- (and is wrong for a stranger about 1 time in 65,536).  No filter (an older chunk): always @True@.
filterMayHave :: Maybe BinaryFuse -> HosiId -> Bool
filterMayHave mf i = maybe True (`bfMember` fromIntegral i) mf

-- | Every event in the chunk, in (id, attribute, seq) order.  Reads the whole body.
readChunkEvents :: ChunkReader -> IO [Event]
readChunkEvents cr = do
  body <- crRead cr (fromIntegral (crBodyStart cr)) (crFooterAt cr - crBodyStart cr)
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
  (i, n, _len) <- getRunHead cr
  replicateM (fromIntegral n) (getRunEvent cr kind (fromIntegral i))

-- | A run's head: (the entity id, how many events, how many bytes they take).
getRunHead :: ChunkReader -> Get (Word64, Word64, Word64)
getRunHead cr
  | crVersion cr >= 2 = do
      d <- getVar; n <- getVar; len <- getVar
      pure (fromIntegral (ciMinId (crInfo cr)) + d, n, len)     -- wraps, as the writer's subtraction did
  | otherwise = do
      i <- getU64; n <- getU32; len <- getU32
      pure (i, fromIntegral n, fromIntegral len)

-- | The longest a run's head can be, so one read of this many bytes always holds all of it.
runHeadMax :: ChunkReader -> Int
runHeadMax cr = if crVersion cr >= 2 then 30 else 16

-- | One event of a run: its attribute, seq and value.
getRunEvent :: ChunkReader -> Kind -> HosiId -> Get Event
getRunEvent cr kind i
  | crVersion cr >= 2 = do
      ai <- getVar
      d  <- getVar
      v  <- getChunkValue cr
      checkAttr cr ai
      pure (Event (ciMinSeq (crInfo cr) + d) kind i (crAttrs cr ! fromIntegral ai) v)
  | otherwise = do
      ai <- getU32
      sq <- getU64
      v  <- getChunkValue cr
      checkAttr cr (fromIntegral ai)
      pure (Event sq kind i (crAttrs cr ! fromIntegral ai) v)

-- | A value as a chunk's runs say it: the plain bytes, or a number in the value dictionary.
getChunkValue :: ChunkReader -> Get Value
getChunkValue cr = do
  tag <- getU8
  if crVersion cr < 2 then getValueBody tag            -- version 1 has no dictionary (tag 5 is an unknown tag there)
  else if tag /= tagDict then getValueBody2 tag else do
    ix <- getVar
    when (ix >= fromIntegral (rangeSize (bounds (crDict cr)))) (failGet "value dictionary index out of range")
    pure (crDict cr ! fromIntegral ix)

checkAttr :: ChunkReader -> Word64 -> Get ()
checkAttr cr ai = when (ai >= fromIntegral (rangeSize (bounds (crAttrs cr)))) (failGet "attribute index out of range")


-- | Point lookup in one chunk, for the events about one id.  With the MPHF index in hand this is the
-- fast path ('lookupByMphf'); without it, the footer's block table finds the block ('lookupByBlock').
lookupInChunk :: ChunkReader -> HosiId -> IO (Probe [Event])
lookupInChunk cr i = case crIndex cr of
  Just (mp, slots) -> lookupByMphf cr mp slots i
  Nothing          -> lookupByBlock cr i

-- | The fast path: MPHF slot, then 「星かな？」 ('confirmSlot': fingerprint, then the key at the slot).
lookupByMphf :: ChunkReader -> Mphf -> B.ByteString -> HosiId -> IO (Probe [Event])
lookupByMphf cr mp slots i
  | mphfSize mp == 0 = pure EmptySky
  | otherwise = do
      let key  = fromIntegral i :: Word64
          slot = mphfSlot mp key
          row  = B.take 12 (B.drop (slot * 12) slots)
          (fpStored, off) = (foldl' (\a b -> a * 256 + fromIntegral b) 0 (B.unpack (B.take 4 row)) :: Word32, getBe64At row 4)
      confirmSlot (fingerprint key) fpStored $ do
        hdr <- crRead cr (fromIntegral off) (runHeadMax cr)
        (rid, n, len, headLen) <- either (corrupt (crDir cr)) pure
          (runGet (getRunHead cr >>= \(a, b, c) -> getPos >>= \p -> pure (a, b, c, p)) hdr 0)
        if rid /= key then pure Nothing else do
          -- a damaged length must not become a huge read
          let start = off + fromIntegral headLen
              limit = fromIntegral (crFooterAt cr) :: Word64
          when (start > limit || len > limit - start) $ corrupt (crDir cr) "a run reaches past the body"
          payload <- crRead cr (fromIntegral start) (fromIntegral len)
          evs <- either (corrupt (crDir cr)) pure $
            runGet (replicateM (fromIntegral n) (getRunEvent cr (ciKind (crInfo cr)) i)) payload 0
          pure (Just evs)

-- | The deep path, for a chunk we hold no MPHF of: the footer's block table says which one block
-- could hold the id (blocks are in id order, and a run is never split), that block is read with one
-- range, and its runs are walked until the id is found or passed.
--
-- The answer uses the same words as the fast path: 'NotStar' when the id is outside every block's
-- range (nothing was read), 'LookAlike' when it is inside a block's range but no run has it (the
-- block was read for nothing: a filter that let a stranger through ends here), 'Star' when found.
lookupByBlock :: ChunkReader -> HosiId -> IO (Probe [Event])
lookupByBlock cr i
  | n == 0 = pure EmptySky
  | otherwise = case blockFor of
      Nothing -> pure NotStar
      Just b  -> do
        let (off, _, _) = blocks ! b
            end         = if b + 1 < n then (\(o, _, _) -> o) (blocks ! (b + 1)) else crFooterAt cr
        bytes <- crRead cr (fromIntegral off) (end - off)
        found <- either (corrupt (crDir cr)) pure (runGet (getU32 >>= scan) bytes 0)
        pure (maybe LookAlike Star found)
  where
    blocks = crBlocks cr
    n      = rangeSize (bounds blocks)
    kind   = ciKind (crInfo cr)
    -- the last block that starts at or before the id, if the id is not past that block's last id
    blockFor = case search 0 (n - 1) Nothing of
      Just b | (\(_, _, l) -> i <= l) (blocks ! b) -> Just b
      _                                           -> Nothing
    search lo hi best
      | lo > hi   = best
      | otherwise = let mid = (lo + hi) `div` 2
                        (_, f, _) = blocks ! mid
                    in if f <= i then search (mid + 1) hi (Just mid) else search lo (mid - 1) best
    -- runs are in id order: skip the ones before the id, stop at the first one after it
    scan :: Word32 -> Get (Maybe [Event])
    scan 0 = pure Nothing
    scan k = do
      (rid, cnt, len) <- getRunHead cr
      let rid' = fromIntegral rid :: HosiId
      if rid' > i then pure Nothing else do
        payload <- getRaw (fromIntegral len)
        if rid' < i then scan (k - 1)
        else either failGet (pure . Just) (runGet (replicateM (fromIntegral cnt) (getRunEvent cr kind i)) payload 0)


-- | (file name, size, sha256) of every file in a chunk directory, for the manifest.  The data file and
-- the MPHF must be there (a missing one throws); the filter is listed only if the chunk has one.
hashChunkFiles :: FilePath -> IO [(String, Int, String)]
hashChunkFiles dir = do
  filterThere <- doesFileExist (dir </> chunkFilterFile)
  forM ([chunkDataFile, chunkMphfFile] ++ [ chunkFilterFile | filterThere ]) $ \f -> do
    bs <- B.readFile (dir </> f)
    pure (f, B.length bs, sha256Hex bs)
