{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka.Codec
-- Description : Order-preserving, self-delimiting encoding of values and keys.
--
-- The one property everything else relies on:
--
-- > compare (encodeValue a) (encodeValue b) == compare (typeRank a, payload a) (typeRank b, payload b)
--
-- i.e. byte-wise comparison of encodings agrees with value comparison, and an
-- encoded value can be followed by more bytes without ambiguity.  That makes an
-- LMDB range scan over a key prefix the same thing as an index lookup.
--
-- Layout of the two triple indexes (both counted DBIs):
--
-- > EAV key = e:8  ++ attrId:4 ++ repr(value)      entry value = payload(value)
-- > AVE key = attrId:4 ++ repr(value) ++ e:8       entry value = payload(value)
--
-- /Large values/.  LMDB keys are limited to 511 bytes, so a value whose full
-- encoding is longer than 'inlineMax' bytes is indexed by a fixed-size stand-in
-- (@0x60 ++ original type tag ++ MD5@, 18 bytes) and its full encoding is stored as
-- the entry's /value/ ('entryPayload').  Small values are their own 'indexRepr' and
-- have an empty payload.  Equality lookups work for both kinds; /ordering/ of large
-- values is by hash, not by content, so range scans must visit the large-value
-- region separately (see "Takka.Store").
module Takka.Codec
  ( encodeValue, decodeValue
  , encodeEid, encodeAid
  , indexRepr, entryPayload, indexEntry, isBigValue, inlineMax, bigTag
  , eavKey, aveKey, eavKeyR, aveKeyR, eavPrefix, avePrefix
  , decodeEavKey, decodeAveKey
  , be32, be64, getBe32, getBe64
  ) where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Unsafe as BU
import           Data.Word
import           Foreign.Ptr (castPtr)
import           GHC.Fingerprint (Fingerprint(..), fingerprintData)
import           GHC.Float (castDoubleToWord64, castWord64ToDouble)
import           System.IO.Unsafe (unsafeDupablePerformIO)

import           Takka.Types

-- ---------------------------------------------------------------------------
-- Fixed-width big-endian integers

be64 :: Word64 -> B.ByteString
be64 w = B.pack [ fromIntegral (w `shiftR` s) | s <- [56, 48 .. 0] ]

be32 :: Word32 -> B.ByteString
be32 w = B.pack [ fromIntegral (w `shiftR` s) | s <- [24, 16, 8, 0] ]

getBe64 :: B.ByteString -> Word64
getBe64 = B.foldl' (\a b -> (a `shiftL` 8) .|. fromIntegral b) 0

getBe32 :: B.ByteString -> Word32
getBe32 = B.foldl' (\a b -> (a `shiftL` 8) .|. fromIntegral b) 0

signFlip :: Word64
signFlip = 0x8000000000000000

encodeEid :: EntityId -> B.ByteString
encodeEid e = be64 (fromIntegral e `xor` signFlip)

decodeEid :: B.ByteString -> EntityId
decodeEid bs = fromIntegral (getBe64 bs `xor` signFlip)

encodeAid :: Word32 -> B.ByteString
encodeAid = be32

-- ---------------------------------------------------------------------------
-- Values

tagFalse, tagTrue, tagInt, tagDouble, tagStr, tagKw :: Word8
tagFalse = 0x10; tagTrue = 0x11; tagInt = 0x20; tagDouble = 0x30; tagStr = 0x40; tagKw = 0x50

-- Escape NUL as 00 FF and terminate with 00 01, so that "a" < "a\0" < "ab".
escapeText :: Text -> B.ByteString
escapeText t = B.concat [escaped, B.pack [0x00, 0x01]]
  where
    raw = TE.encodeUtf8 t
    -- B.elem and B.split use memchr, so text without NUL bytes (nearly all of it) is copied, not walked
    escaped | B.elem 0 raw = B.intercalate (B.pack [0x00, 0xFF]) (B.split 0 raw)
            | otherwise    = raw

unescape :: B.ByteString -> Maybe (Text, B.ByteString)
unescape = go []
  where
    go acc bs = do
      i <- B.elemIndex 0 bs
      let (chunk, rest) = B.splitAt i bs
      (marker, after) <- B.uncons (B.drop 1 rest)
      case marker of
        0x01 -> finish (reverse (chunk : acc)) after
        0xFF -> go (B.singleton 0 : chunk : acc) after
        _    -> Nothing
    finish parts after = case TE.decodeUtf8' (B.concat parts) of
      Right t -> Just (t, after)
      Left _  -> Nothing

doubleBits :: Double -> Word64
doubleBits d =
  let w = castDoubleToWord64 (if d == 0 then 0 else d)   -- fold -0.0 into 0.0
  in if testBit w 63 then complement w else w `xor` signFlip

unDoubleBits :: Word64 -> Double
unDoubleBits w = castWord64ToDouble (if testBit w 63 then w `xor` signFlip else complement w)

encodeValue :: Value -> B.ByteString
encodeValue v = case v of
  VBool False -> B.singleton tagFalse
  VBool True  -> B.singleton tagTrue
  VInt n      -> B.cons tagInt (be64 (fromIntegral n `xor` signFlip))
  VDouble d   -> B.cons tagDouble (be64 (doubleBits d))
  VStr s      -> B.cons tagStr (escapeText s)
  VKw (Keyword k) -> B.cons tagKw (escapeText k)

-- | Decode one value and hand back the bytes that follow it.
decodeValue :: B.ByteString -> Maybe (Value, B.ByteString)
decodeValue bs = do
  (t, rest) <- B.uncons bs
  case t of
    _ | t == tagFalse  -> Just (VBool False, rest)
      | t == tagTrue   -> Just (VBool True, rest)
      | t == tagInt    -> fixed rest (\w -> VInt (fromIntegral (w `xor` signFlip)))
      | t == tagDouble -> fixed rest (VDouble . unDoubleBits)
      | t == tagStr    -> do (s, r) <- unescape rest; Just (VStr s, r)
      | t == tagKw     -> do (s, r) <- unescape rest; Just (VKw (Keyword s), r)
      | otherwise      -> Nothing
  where
    fixed rest f
      | B.length rest >= 8 = let (a, b) = B.splitAt 8 rest in Just (f (getBe64 a), b)
      | otherwise          = Nothing

-- ---------------------------------------------------------------------------
-- Large values

-- | Tag byte of the stand-in used for values that do not fit in a key.
bigTag :: Word8
bigTag = 0x60

-- | Values whose full encoding is longer than this are stored out of line.
-- (Key limit 511 − 12 bytes of ids; a smaller number keeps index pages dense.)
inlineMax :: Int
inlineMax = 400

md5 :: B.ByteString -> B.ByteString
md5 bs = unsafeDupablePerformIO $
  BU.unsafeUseAsCStringLen bs $ \(p, n) -> do
    Fingerprint a b <- fingerprintData (castPtr p) n
    pure (B.append (be64 a) (be64 b))

isBigValue :: Value -> Bool
isBigValue v = B.length (encodeValue v) > inlineMax

-- | Both halves of an index entry for a value, from a single encoding:
-- the key part ('indexRepr') and the entry's value ('entryPayload').
indexEntry :: Value -> (B.ByteString, B.ByteString)
indexEntry v
  | B.length full <= inlineMax = (full, B.empty)
  | otherwise                  = (B.concat [B.singleton bigTag, B.take 1 full, md5 full], full)
  where full = encodeValue v

-- | What goes into an index key for this value.
indexRepr :: Value -> B.ByteString
indexRepr = fst . indexEntry

-- | What goes into the index /entry's value/: empty for small values, the full
-- encoding for large ones.
entryPayload :: Value -> B.ByteString
entryPayload = snd . indexEntry

-- ---------------------------------------------------------------------------
-- Index keys

eavKey :: EntityId -> Word32 -> Value -> B.ByteString
eavKey e a v = eavKeyR e a (indexRepr v)

aveKey :: Word32 -> Value -> EntityId -> B.ByteString
aveKey a v e = aveKeyR a (indexRepr v) e

-- | Key builders that take an already computed 'indexRepr' (avoids encoding a large value twice).
eavKeyR :: EntityId -> Word32 -> B.ByteString -> B.ByteString
eavKeyR e a repr = B.concat [encodeEid e, encodeAid a, repr]

aveKeyR :: Word32 -> B.ByteString -> EntityId -> B.ByteString
aveKeyR a repr e = B.concat [encodeAid a, repr, encodeEid e]

-- | Prefix for an EAV scan: as many leading components as are known.
eavPrefix :: Maybe EntityId -> Maybe Word32 -> Maybe Value -> B.ByteString
eavPrefix me ma mv = case me of
  Nothing -> B.empty
  Just e  -> B.append (encodeEid e) $ case ma of
    Nothing -> B.empty
    Just a  -> B.append (encodeAid a) (maybe B.empty indexRepr mv)

-- | Prefix for an AVE scan.
avePrefix :: Word32 -> Maybe Value -> B.ByteString
avePrefix a mv = B.append (encodeAid a) (maybe B.empty indexRepr mv)

-- | Decode an EAV entry.  The second argument is the entry's /value/ (the payload),
-- needed only for large values.
decodeEavKey :: B.ByteString -> B.ByteString -> Maybe (EntityId, Word32, Value)
decodeEavKey k payload
  | B.length k < 12 = Nothing
  | otherwise =
      let (e, r1) = B.splitAt 8 k
          (a, r2) = B.splitAt 4 r1
      in do v <- decodeRepr r2 payload
            Just (decodeEid e, getBe32 a, v)

decodeAveKey :: B.ByteString -> B.ByteString -> Maybe (Word32, Value, EntityId)
decodeAveKey k payload
  | B.length k < 12 = Nothing
  | otherwise =
      let (a, r1) = B.splitAt 4 k
          (mid, e) = B.splitAt (B.length r1 - 8) r1
      in do v <- decodeRepr mid payload
            Just (getBe32 a, v, decodeEid e)

-- | Decode a complete 'indexRepr' (nothing may follow it).
decodeRepr :: B.ByteString -> B.ByteString -> Maybe Value
decodeRepr repr payload = case B.uncons repr of
  Just (t, _) | t == bigTag ->
    -- the payload is the value's full encoding; its type tag must match the stand-in's
    case decodeValue payload of
      Just (v, rest) | B.null rest, B.take 1 payload == B.take 1 (B.drop 1 repr) -> Just v
      _ -> Nothing
  _ -> case decodeValue repr of
         Just (v, rest) | B.null rest -> Just v
         _ -> Nothing

_int64 :: Int64
_int64 = 0
