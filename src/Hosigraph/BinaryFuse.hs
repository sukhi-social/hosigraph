-- |
-- Module      : Hosigraph.BinaryFuse
-- Description : A Binary Fuse filter (3 wise, 16-bit prints): "is this key maybe in the chunk?" in a few bytes per key.
--
-- A filter answers one question, cheaply: /might this key be in the set?/  \"No\" is always true.
-- \"Yes\" is wrong about one time in 65,536 for a key that is not in the set (a false positive),
-- and never wrong for a key that is.  It costs about 18 bits per key.
--
-- This is the first gate of the deep path: a chunk that lives far away (S3) is asked here first,
-- and only a \"maybe\" is worth a request.  ("Hosigraph.Mphf" is the guide for a chunk we hold in
-- hand; it can say \"this slot\" for any key, even a stranger, so it needs the key check after it.
-- A filter needs no key check to say \"no\", but it can never say \"certainly yes\".)
--
-- How it works, in four sentences.  The table is a row of 16-bit cells, cut into segments of one
-- power-of-two length.  Each key hashes to three cells, one in each of three neighbouring segments.
-- Building peels the keys: a cell that only one key touches is given to that key, the key is
-- taken out of its other two cells, and so on, until every key has a cell of its own; the cells
-- are then filled backwards so that the three cells of every key xor to the key's 16-bit print.
-- A lookup xors the three cells of the key and compares with its print: equal means \"maybe\".
--
-- It is the construction of Graf and Lemire, \"Binary Fuse Filters: Fast and Smaller Than Xor
-- Filters\" (2022).  The table size and segment length follow their formulas (the table is about
-- 1.125 times the number of keys when there are many).  Not done here: their step that sorts the
-- keys by segment before building (it makes the build kinder to the cache, not different).
module Hosigraph.BinaryFuse
  ( BinaryFuse, buildBinaryFuse, bfMember, bfKeys, bfBytes
  , putBinaryFuse, getBinaryFuse
  ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST (STUArray, newArray, readArray, writeArray, freeze)
import           Data.Array.Unboxed (UArray, listArray, bounds, (!))
import           Data.Bits
import           Data.Ix (rangeSize)
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Set as Set
import           Data.Word

import           Hosigraph.Bin
import           Hosigraph.Mphf (mix64)

data BinaryFuse = BinaryFuse
  { bfSeed     :: !Word64
  , bfN        :: !Int                  -- ^ how many keys it was built from
  , bfSegLen   :: !Int                  -- ^ cells in a segment (a power of two); 0 for an empty filter
  , bfSegCount :: !Int                  -- ^ where a key's first cell may be: segments 0 .. segCount-1
  , bfPrints   :: !(UArray Int Word16)  -- ^ (segCount + 2) * segLen cells
  }

-- | How many keys it was built from.
bfKeys :: BinaryFuse -> Int
bfKeys = bfN

-- | How many bytes the cells take (the same as in a file, apart from a short header).
bfBytes :: BinaryFuse -> Int
bfBytes f = 2 * rangeSize (bounds (bfPrints f))

-- | More keys than this are refused: the counts and the first-cell arithmetic assume they fit 32 bits.
maxKeys :: Int
maxKeys = 2 ^ (28 :: Int)

cellCount :: Int -> Int -> Int
cellCount segLen segCount = (segCount + 2) * segLen

-- | (segment length, segment count) for this many keys: the formulas of the paper's reference code.
-- Few keys get a roomier table (a small set peels less reliably).
fuseShape :: Int -> (Int, Int)
fuseShape n =
  let size     = fromIntegral (max 2 n) :: Double
      segLen   = min (2 ^ (18 :: Int)) (max 4 (2 ^ (floor (logBase 3.33 size + 2.25) :: Int)))
      factor   = max 1.125 (0.875 + 0.25 * log 1000000 / log size)
      capacity = floor (fromIntegral n * factor) :: Int
      initSegs = (capacity + segLen - 1) `div` segLen - 2
      cells0   = (initSegs + 2) * segLen
      segs0    = (cells0 + segLen - 1) `div` segLen
      segCount = if segs0 <= 2 then 1 else segs0 - 2
  in (segLen, segCount)

-- | The hash of a key for one seed.
fuseHash :: Word64 -> Word64 -> Word64
fuseHash seed key = mix64 (key + seed * 0x9E3779B97F4A7C15)

-- | The 16-bit print of a hash.
printOf :: Word64 -> Word16
printOf h = fromIntegral (h `xor` (h `shiftR` 32))

-- | The three cells of a hash.  The first is spread over all the segments that may hold a first
-- cell (from the top 32 bits of the hash, so it does not share bits with the two offsets); the other two
-- are the same place one and two segments on, moved within their segment by other bits of the hash.
cellsOf :: Int -> Int -> Word64 -> (Int, Int, Int)
cellsOf segLen firstSpan h =
  let mask = fromIntegral segLen - 1 :: Word64
      c0   = fromIntegral (((h `shiftR` 32) * fromIntegral firstSpan) `shiftR` 32) :: Int
      c1   = (c0 + segLen)     `xor` fromIntegral ((h `shiftR` 18) .&. mask)
      c2   = (c0 + 2 * segLen) `xor` fromIntegral (h .&. mask)
  in (c0, c1, c2)

-- | Build from keys.  (A key that comes twice counts once.)
buildBinaryFuse :: [Word64] -> BinaryFuse
buildBinaryFuse keys0
  | n == 0       = BinaryFuse 0 0 0 0 (listArray (0, -1) [])
  | n > maxKeys  = error "buildBinaryFuse: too many keys"
  | otherwise    = go 0
  where
    keys = Set.toList (Set.fromList keys0)
    n    = length keys
    (segLen, segCount) = fuseShape n
    go seed
      | seed > 10000 = error "buildBinaryFuse: no seed worked"     -- never seen: each seed works most of the time
      | otherwise    = maybe (go (seed + 1)) id (tryBuild seed n segLen segCount keys)

tryBuild :: Word64 -> Int -> Int -> Int -> [Word64] -> Maybe BinaryFuse
tryBuild seed n segLen segCount keys = runST $ do
  let total  = cellCount segLen segCount
      span'  = segLen * segCount
      cellsAt h = cellsOf segLen span' h
      pick :: (Int, Int, Int) -> Int -> Int
      pick (a, b, c) p = case p of { 0 -> a; 1 -> b; _ -> c }
      hashes = map (fuseHash seed) keys
  -- per cell: (how many keys touch it) * 4 + (xor of the 0/1/2 that says which of its three cells it is for each),
  -- and the xor of their hashes.  When one key is left, the low two bits name its place and the xor is its hash.
  cnt <- newArray (0, total - 1) 0 :: ST s (STUArray s Int Word32)
  hsh <- newArray (0, total - 1) 0 :: ST s (STUArray s Int Word64)
  let touch h p i = do
        c <- readArray cnt i
        writeArray cnt i ((c + 4) `xor` p)
        x <- readArray hsh i
        writeArray hsh i (x `xor` h)
  forM_ hashes $ \h -> do
    let (a, b, c) = cellsAt h
    touch h 0 a >> touch h 1 b >> touch h 2 c
  -- the cells that one key touches
  queue <- newArray (0, total - 1) 0 :: ST s (STUArray s Int Int)
  q0 <- foldM (\q i -> do
                 c <- readArray cnt i
                 if c `shiftR` 2 == 1 then writeArray queue q i >> pure (q + 1) else pure q) 0 [0 .. total - 1]
  stackH <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Word64)
  stackP <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Word8)
  let peel 0 sp = pure sp
      peel q sp = do
        idx <- readArray queue (q - 1)
        c   <- readArray cnt idx
        if c `shiftR` 2 /= 1 then peel (q - 1) sp else do
          h <- readArray hsh idx
          let found = fromIntegral (c .&. 3) :: Int
              cells = cellsAt h
          writeArray stackH sp h
          writeArray stackP sp (fromIntegral found)
          -- take the key out of its other two cells; a cell left with one key goes on the queue
          q' <- foldM (\qq p ->
                  if p == found then pure qq else do
                    let j = pick cells p
                    cj <- readArray cnt j
                    writeArray cnt j ((cj - 4) `xor` fromIntegral p)
                    hj <- readArray hsh j
                    writeArray hsh j (hj `xor` h)
                    if cj `shiftR` 2 == 2 then writeArray queue qq j >> pure (qq + 1) else pure qq)
                (q - 1) [0, 1, 2]
          peel q' (sp + 1)
  done <- peel q0 0
  if done /= n then pure Nothing else do
    prints <- newArray (0, total - 1) 0 :: ST s (STUArray s Int Word16)
    -- last peeled first: the key's own cell is the only one of its three not yet fixed
    forM_ [n - 1, n - 2 .. 0] $ \i -> do
      h <- readArray stackH i
      p <- fromIntegral <$> readArray stackP i
      let cells = cellsAt h
      x <- readArray prints (pick cells ((p + 1) `mod` 3))
      y <- readArray prints (pick cells ((p + 2) `mod` 3))
      writeArray prints (pick cells p) (printOf h `xor` x `xor` y)
    frozen <- freezeW16 prints
    pure (Just (BinaryFuse seed n segLen segCount frozen))

freezeW16 :: STUArray s Int Word16 -> ST s (UArray Int Word16)
freezeW16 = freeze

-- | Might this key be in the set it was built from?  @False@ is certain; @True@ is wrong about
-- 1 time in 65,536 for a key that is not in the set.
bfMember :: BinaryFuse -> Word64 -> Bool
bfMember f key
  | bfN f == 0 = False
  | otherwise  =
      let h = fuseHash (bfSeed f) key
          (a, b, c) = cellsOf (bfSegLen f) (bfSegLen f * bfSegCount f) h
          p = bfPrints f
      in printOf h `xor` (p ! a) `xor` (p ! b) `xor` (p ! c) == 0

-- ---------------------------------------------------------------------------
-- Bytes

-- | seed, number of keys, segment length, segment count, then the cells (none when there are no keys).
putBinaryFuse :: BinaryFuse -> Builder'
putBinaryFuse f =
  putU64 (bfSeed f) <> putU32 (fromIntegral (bfN f)) <> putU32 (fromIntegral (bfSegLen f))
    <> putU32 (fromIntegral (bfSegCount f))
    <> mconcat [ putU16 (bfPrints f ! i) | i <- [0 .. rangeSize (bounds (bfPrints f)) - 1] ]

getBinaryFuse :: Get BinaryFuse
getBinaryFuse = do
  seed <- getU64
  n    <- fromIntegral <$> getU32
  sl   <- fromIntegral <$> getU32
  sc   <- fromIntegral <$> getU32
  if n == 0
    then do
      when (sl /= 0 || sc /= 0) (failGet "an empty binary fuse filter must have no segments")
      pure (BinaryFuse 0 0 0 0 (listArray (0, -1) []))
    else do
      when (n > maxKeys || sl < 4 || sl > 2 ^ (18 :: Int) || popCount sl /= 1 || sc < 1 || sl * sc >= 2 ^ (32 :: Int))
        (failGet "implausible binary fuse header")
      let total = cellCount sl sc
      when (total < n) (failGet "a binary fuse filter with fewer cells than keys")
      raw <- getRaw (2 * total)          -- fails at once if the file is shorter than it says
      let cell i = (fromIntegral (BU.unsafeIndex raw (2 * i)) `shiftL` 8) .|. fromIntegral (BU.unsafeIndex raw (2 * i + 1)) :: Word16
      pure (BinaryFuse seed n sl sc (listArray (0, total - 1) (map cell [0 .. total - 1])))
