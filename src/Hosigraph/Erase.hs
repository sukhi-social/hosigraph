-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Erase
-- Description : A set of erased ids, kept as sorted ranges, with a small file format.
--
-- Erase does not rewrite a chunk in the same breath.  It writes down /which ids are gone/, and
-- every reader asks that list first.  This module is only the list: a plain value, no I/O.
--
-- An 'EraseSet' is a set of ids kept as sorted, inclusive ranges (@lo, hi@).  One id is a range of
-- one.  A day of posts is a range of one, whatever it holds, because an id is a time.  The ranges
-- are always in one /normal form/: sorted, and no two of them overlap or touch (3..5 and 6..9 are
-- 3..9).  So two sets with the same ids are equal, and @==@ means what it says.
--
-- It is meant to be imported qualified:
--
-- > import qualified Hosigraph.Erase as Erase
--
-- /The file./  'encode' and 'decode' (called @HOSIERS1@; the @1@ is the version, and a layout that
-- cannot be read by this code will be @HOSIERS2@):
--
-- > "HOSIERS1"   8 bytes
-- > count        varint    how many ranges
-- > per range:   varint gap  (the first: lo itself; then: lo - previous hi - 1)
-- >              varint len  (hi - lo)
-- > crc32        4 bytes   of everything before it
--
-- Numbers are counted from the one before them, so ids that are close together take few bytes.  The
-- numbers are 'Word64' arithmetic that wraps, like the chunk files', so every 'Int64' comes back
-- exactly, whatever it is.  The CRC is there so that this file can tell by itself that a byte
-- changed (the manifest's SHA-256 says so too, but the reader of a file should not need to know).
module Hosigraph.Erase
  ( EraseSet
  , empty, null, fromRanges, fromIds, toRanges
  , member, union, overlaps, restrictTo
  , encode, decode
  ) where

import           Prelude hiding (null)

import           Control.Monad (unless, when)
import           Data.Array.Unboxed (UArray, bounds, listArray, (!))
import qualified Data.ByteString as B
import           Data.Int (Int64)
import           Data.List (sortOn)
import           Data.Word (Word64)

import           Hosigraph.Bin
import           Hosigraph.Event (HosiId)

-- | Sorted, disjoint, non-touching, inclusive ranges: @los ! k <= his ! k@ and @his ! k + 1 < los ! (k + 1)@.
data EraseSet = EraseSet !(UArray Int Int64) !(UArray Int Int64)
  deriving Eq

instance Show EraseSet where
  show s = "fromRanges " ++ show (toRanges s)

empty :: EraseSet
empty = EraseSet (listArray (0, -1) []) (listArray (0, -1) [])

null :: EraseSet -> Bool
null s = size s == 0

size :: EraseSet -> Int
size (EraseSet los _) = let (a, b) = bounds los in b - a + 1

-- | The ranges, in order.
toRanges :: EraseSet -> [(HosiId, HosiId)]
toRanges s@(EraseSet los his) = [ (los ! k, his ! k) | k <- [0 .. size s - 1] ]

-- | Any list of inclusive ranges, in any order, overlapping or not.  A pair with @lo > hi@ holds no id and is dropped.
fromRanges :: [(HosiId, HosiId)] -> EraseSet
fromRanges = fromSorted . sortOn fst . filter (uncurry (<=))

fromIds :: [HosiId] -> EraseSet
fromIds is = fromRanges [ (i, i) | i <- is ]

-- | Ranges sorted by their start; joins the ones that overlap or touch.
fromSorted :: [(HosiId, HosiId)] -> EraseSet
fromSorted rs =
  let joined = coalesce rs
      n      = length joined
  in EraseSet (listArray (0, n - 1) (map fst joined)) (listArray (0, n - 1) (map snd joined))

coalesce :: [(HosiId, HosiId)] -> [(HosiId, HosiId)]
coalesce ((a, b) : (c, d) : rest)
  | b == maxBound || c <= b + 1 = coalesce ((a, max b d) : rest)      -- (b + 1 would wrap at the top)
  | otherwise                   = (a, b) : coalesce ((c, d) : rest)
coalesce xs = xs

-- | The index of the last range that starts at or before the id.
lastStartingAtOrBefore :: UArray Int Int64 -> HosiId -> Maybe Int
lastStartingAtOrBefore los i = go 0 (size' los)
  where
    size' a = let (x, y) = bounds a in y - x + 1
    -- invariant: every index below lo starts at or before i; every index from hi on starts after it
    go lo hi
      | lo >= hi  = if lo == 0 then Nothing else Just (lo - 1)
      | otherwise = let mid = (lo + hi) `div` 2
                    in if los ! mid <= i then go (mid + 1) hi else go lo mid

member :: HosiId -> EraseSet -> Bool
member i (EraseSet los his) = case lastStartingAtOrBefore los i of
  Nothing -> False
  Just k  -> i <= his ! k

union :: EraseSet -> EraseSet -> EraseSet
union a b = fromSorted (mergeByStart (toRanges a) (toRanges b))

mergeByStart :: [(HosiId, HosiId)] -> [(HosiId, HosiId)] -> [(HosiId, HosiId)]
mergeByStart xs [] = xs
mergeByStart [] ys = ys
mergeByStart xa@(x : xs) ya@(y : ys)
  | fst x <= fst y = x : mergeByStart xs ya
  | otherwise      = y : mergeByStart xa ys

-- | The first range that ends at or after @lo@ (so it is the first one that could touch @[lo, hi]@).
firstEndingAtOrAfter :: EraseSet -> HosiId -> Int
firstEndingAtOrAfter s@(EraseSet _ his) lo = go 0 (size s)
  where
    go a b
      | a >= b    = a
      | otherwise = let mid = (a + b) `div` 2
                    in if his ! mid >= lo then go a mid else go (mid + 1) b

-- | Does any erased id lie in @[lo, hi]@?
overlaps :: (HosiId, HosiId) -> EraseSet -> Bool
overlaps (lo, hi) s@(EraseSet los _) =
  lo <= hi && let k = firstEndingAtOrAfter s lo in k < size s && los ! k <= hi

-- | The erased ids that lie in @[lo, hi]@ (the ranges cut to fit).
restrictTo :: (HosiId, HosiId) -> EraseSet -> EraseSet
restrictTo (lo, hi) s@(EraseSet los his)
  | lo > hi   = empty
  | otherwise =
      let k0 = firstEndingAtOrAfter s lo
          inside = takeWhile ((<= hi) . fst) [ (los ! k, his ! k) | k <- [k0 .. size s - 1] ]
      in fromSorted [ (max a lo, min b hi) | (a, b) <- inside ]     -- already in normal form; this only cuts the ends

-- ---------------------------------------------------------------------------
-- The file

magic :: B.ByteString
magic = "HOSIERS1"

-- | Write the set as an @HOSIERS1@ file.
encode :: EraseSet -> B.ByteString
encode s =
  let rs   = toRanges s
      gaps = zipWith gapOf (Nothing : map (Just . snd) rs) (map fst rs)
      body = build $ put magic <> putVar (fromIntegral (length rs))
               <> mconcat [ putVar g <> putVar (word hi - word lo) | (g, (lo, hi)) <- zip gaps rs ]
  in body <> be32 (crc32 body)
  where
    word :: Int64 -> Word64
    word = fromIntegral
    gapOf Nothing     lo = word lo                          -- the first start, as it is
    gapOf (Just prev) lo = word lo - word prev - 1          -- never negative: the ranges do not touch

-- | Read an @HOSIERS1@ file.  'Left' says what is wrong; the caller adds the path and calls it 'Corrupt'.
-- A byte that changed, a file cut short or with something after its end, a number that does not fit,
-- or a range that touches the one before it (so the file is not in the form 'encode' writes) are all refused.
decode :: B.ByteString -> Either String EraseSet
decode bs = do
  when (B.length bs < B.length magic + 1 + 4) (Left "an erase record is shorter than its own header")
  let (body, crcBytes) = B.splitAt (B.length bs - 4) bs
  unless (be32 (crc32 body) == crcBytes) (Left "an erase record does not match its checksum")
  runGet parse body 0
  where
    parse = do
      m <- getRaw (B.length magic)
      unless (m == magic) (failGet "not an erase record (wrong magic)")
      n <- getVar
      -- every range takes at least two bytes: a count that cannot fit is refused before anything is allocated
      left <- (\p -> B.length bs - 4 - p) <$> getPos
      when (n > fromIntegral (left `div` 2)) (failGet "an erase record claims more ranges than it has bytes")
      rs <- readRanges (fromIntegral n) Nothing
      done <- atEnd
      unless done (failGet "an erase record has bytes after its last range")
      pure (fromSorted rs)

    readRanges :: Int -> Maybe HosiId -> Get [(HosiId, HosiId)]
    readRanges 0 _ = pure []
    readRanges k prev = do
      g <- getVar
      l <- getVar
      lo <- case prev of
        Nothing -> pure (fromIntegral g)                                          -- wraps back to the Int64 that was written
        Just p  -> do
          when (g == 0) (failGet "an erase record has a range that touches the one before it")   -- our writer joins those
          within (toInteger p + 1 + toInteger g)
      hi <- within (toInteger lo + toInteger l)
      ((lo, hi) :) <$> readRanges (k - 1) (Just hi)

    within :: Integer -> Get HosiId
    within x
      | x > toInteger (maxBound :: Int64) = failGet "an erase record has a range past the largest id"
      | otherwise                         = pure (fromInteger x)
