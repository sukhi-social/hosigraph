-- |
-- Module      : Hosigraph.Mphf
-- Description : A minimal perfect hash function, PTHash-style (hash, displace, remap).
--
-- Given @n@ distinct keys, 'buildMphf' returns a function that maps them one-to-one
-- onto @[0, n)@, using about two bytes per bucket.  The design calls for PtrHash,
-- PTHash or RecSplit; this is the simplest member of that family, enough to prove the
-- shape (build, serialize, look up) before the choice is made.
--
-- How it works, in four sentences.  Keys are hashed into buckets of about four keys.
-- Biggest buckets first, each bucket searches for a /pilot/ number such that
-- hash(key, pilot) lands all of its keys on distinct, still-free slots of a table that
-- is a little larger than @n@.  The few keys that end up past slot @n@ are moved to
-- the free slots below @n@ through a small remap table.  A lookup is then one hash,
-- one pilot read, one hash, and (rarely) one remap read.
--
-- /A minimal perfect hash function answers every question, including ones about keys
-- that were never in the set./  So the caller must always check the key itself
-- (see "Hosigraph.Probe", the 「星かな？」 step): 'fingerprint' is a cheap first check,
-- the real key at the slot is the last one.
module Hosigraph.Mphf
  ( Mphf, mphfSize, buildMphf, mphfSlot, fingerprint
  , putMphf, getMphf, mix64
  ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST (STUArray, newArray, readArray, writeArray, freeze)
import           Data.Array.Unboxed (UArray, listArray, elems, (!))
import           Data.Bits
import qualified Data.IntMap.Strict as IM
import           Data.List (sortBy)
import           Data.Ord (comparing, Down(..))
import qualified Data.Set as Set
import           Data.Word

import           Hosigraph.Bin

data Mphf = Mphf
  { mSeed   :: !Word64
  , mN      :: !Int                    -- ^ number of keys; the result range is [0, n)
  , mM      :: !Int                    -- ^ table size before remapping (m >= n)
  , mNb     :: !Int                    -- ^ number of buckets
  , mPilots :: !(UArray Int Word16)
  , mRemap  :: !(UArray Int Word32)    -- ^ for slots n..m-1: where that key really goes
  }

mphfSize :: Mphf -> Int
mphfSize = mN

-- | SplitMix64's finalizer: a fast, well-mixed 64-bit permutation.
mix64 :: Word64 -> Word64
mix64 z0 =
  let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
  in z2 `xor` (z2 `shiftR` 31)

keyHash :: Word64 -> Word64 -> Word64
keyHash seed key = mix64 (key + seed * 0x9E3779B97F4A7C15)

bucketOf :: Int -> Word64 -> Int
bucketOf nb h = fromIntegral ((h `shiftR` 7) `mod` fromIntegral nb)

slotOf :: Int -> Word64 -> Int -> Int
slotOf m h pilot = fromIntegral (mix64 (h `xor` (fromIntegral pilot * 0xC2B2AE3D27D4EB4F + 1)) `mod` fromIntegral m)

-- | A 32-bit tag of the key, independent of the placement hash.  Stored next to each
-- slot so most absent keys are turned away without touching the chunk.
fingerprint :: Word64 -> Word32
fingerprint key = fromIntegral (mix64 (key `xor` 0xA5A5A5A5DEADBEEF) `shiftR` 32)

-- | Build from distinct keys.  (Duplicates are a caller bug and raise an error.)
buildMphf :: [Word64] -> Mphf
buildMphf keys
  | Set.size (Set.fromList keys) /= n = error "buildMphf: duplicate keys"
  | n == 0 = Mphf 0 0 0 0 (listArray (0, -1) []) (listArray (0, -1) [])
  | otherwise = go 0
  where
    n  = length keys
    m  = n + n `div` 8 + 1                -- about 89% full before remapping
    nb = max 1 (n `div` 4)
    go seed = case tryBuild seed n m nb keys of
      Just r  -> r
      Nothing -> go (seed + 1)            -- extremely rare: try another seed

tryBuild :: Word64 -> Int -> Int -> Int -> [Word64] -> Maybe Mphf
tryBuild seed n m nb keys = runST $ do
  occ <- newArray (0, m - 1) False :: ST s (STUArray s Int Bool)
  pil <- newArray (0, nb - 1) 0    :: ST s (STUArray s Int Word16)
  let buckets = IM.toList (IM.fromListWith (++) [ (bucketOf nb h, [h]) | k <- keys, let h = keyHash seed k ])
      order   = sortBy (comparing (Down . length . snd)) buckets   -- biggest buckets first
      findPilot hs = loop (0 :: Int)
        where
          loop p
            | p > 65535 = pure Nothing
            | otherwise = do
                let ss = map (\h -> slotOf m h p) hs
                if not (distinct ss) then loop (p + 1) else do
                  free <- allM (\s -> not <$> readArray occ s) ss
                  if free then pure (Just p) else loop (p + 1)
  ok <- foldM (\acc (b, hs) ->
                 if not acc then pure False else do
                   r <- findPilot hs
                   case r of
                     Nothing -> pure False
                     Just p  -> do
                       writeArray pil b (fromIntegral p)
                       forM_ hs $ \h -> writeArray occ (slotOf m h p) True
                       pure True) True order
  if not ok then pure Nothing else do
    occF <- freezeBool occ
    pilF <- freezeW16 pil
    -- every occupied slot >= n gets one of the free slots < n (there are exactly as many)
    let frees = [ i | i <- [0 .. n - 1], not (occF ! i) ]
        highs = [ i | i <- [n .. m - 1], occF ! i ]
        moved = IM.fromList (zip highs frees)
        remap = listArray (0, m - n - 1) [ maybe 0 fromIntegral (IM.lookup (n + j) moved) | j <- [0 .. m - n - 1] ]
    pure (Just (Mphf seed n m nb pilF remap))

freezeBool :: STUArray s Int Bool -> ST s (UArray Int Bool)
freezeBool = freeze

freezeW16 :: STUArray s Int Word16 -> ST s (UArray Int Word16)
freezeW16 = freeze

distinct :: [Int] -> Bool
distinct = go Set.empty
  where go _ [] = True
        go s (x : xs) | x `Set.member` s = False
                      | otherwise = go (Set.insert x s) xs

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ [] = pure True
allM f (x : xs) = f x >>= \b -> if b then allM f xs else pure False

-- | The slot for a key, in @[0, n)@.  For a key that was in the build set this is
-- unique; for any other key it is an arbitrary slot: check the key!
-- Returns -1 only for an empty function.
mphfSlot :: Mphf -> Word64 -> Int
mphfSlot mp key
  | mN mp == 0 = -1
  | otherwise =
      let h = keyHash (mSeed mp) key
          p = fromIntegral (mPilots mp ! bucketOf (mNb mp) h)
          s = slotOf (mM mp) h p
      in if s < mN mp then s else fromIntegral (mRemap mp ! (s - mN mp))

-- ---------------------------------------------------------------------------
-- Bytes

putMphf :: Mphf -> Builder'
putMphf mp =
  putU64 (mSeed mp) <> putU32 (fromIntegral (mN mp)) <> putU32 (fromIntegral (mM mp))
    <> putU32 (fromIntegral (mNb mp))
    <> mconcat [ putU16 p | p <- elems (mPilots mp) ]
    <> mconcat [ putU32 r | r <- elems (mRemap mp) ]

getMphf :: Get Mphf
getMphf = do
  seed <- getU64
  n  <- fromIntegral <$> getU32
  m  <- fromIntegral <$> getU32
  nb <- fromIntegral <$> getU32
  when (n < 0 || m < n || nb < 0 || (n > 0 && nb < 1) || m > 1000000000) (failGet "implausible MPHF header")
  pils <- replicateM nb getU16
  rems <- replicateM (m - n) getU32
  pure (Mphf seed n m nb (listArray (0, nb - 1) pils) (listArray (0, m - n - 1) rems))
