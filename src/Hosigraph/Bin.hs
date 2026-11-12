-- SPDX-License-Identifier: BlueOak-1.0.0
-- |
-- Module      : Hosigraph.Bin
-- Description : Big-endian binary builders and a tiny bounds-checked parser.
module Hosigraph.Bin
  ( -- * Writing
    Builder', put, putU8, putU16, putU32, putU64, putBytes, putText, putVar, putVarBytes, putVarText, zigzag, build
    -- * Reading
  , Get, runGet, failGet, getU8, getU16, getU32, getU64, getRaw, getBytes, getText, getVar, getVarBytes, getVarText, unzigzag, getPos, atEnd
  , be32, be64, getBe64At
  , crc32
  ) where

import           Data.Array.Unboxed (UArray, listArray, (!))
import           Data.Bits
import           Data.Int (Int64)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           Data.Word

-- ---------------------------------------------------------------------------
-- Writing

-- | Renamed so modules do not need to import Data.ByteString.Builder.
type Builder' = Builder

put :: B.ByteString -> Builder
put = byteString

putU8 :: Word8 -> Builder
putU8 = word8

putU16 :: Word16 -> Builder
putU16 = word16BE

putU32 :: Word32 -> Builder
putU32 = word32BE

putU64 :: Word64 -> Builder
putU64 = word64BE

-- | u32 length, then the bytes.
putBytes :: B.ByteString -> Builder
putBytes b = putU32 (fromIntegral (B.length b)) <> byteString b

putText :: Text -> Builder
putText = putBytes . TE.encodeUtf8

-- | A variable-length unsigned number (LEB128): 7 bits at a time, the low bits first, and the top bit
-- of each byte says "more follows".  0-127 take 1 byte, up to 2^14 - 1 take 2, a full 64 bits take 10.
putVar :: Word64 -> Builder
putVar n
  | n < 0x80  = word8 (fromIntegral n)
  | otherwise = word8 (fromIntegral (n .&. 0x7f) .|. 0x80) <> putVar (n `shiftR` 7)

-- | 'putVar' length, then the bytes.
putVarBytes :: B.ByteString -> Builder
putVarBytes b = putVar (fromIntegral (B.length b)) <> byteString b

putVarText :: Text -> Builder
putVarText = putVarBytes . TE.encodeUtf8

-- | Fold the sign into the lowest bit (0, -1, 1, -2, 2 ... become 0, 1, 2, 3, 4 ...), so that a small
-- number of either sign is a small 'putVar'.
zigzag :: Int64 -> Word64
zigzag n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))

unzigzag :: Word64 -> Int64
unzigzag w = fromIntegral (w `shiftR` 1) `xor` negate (fromIntegral (w .&. 1))

build :: Builder -> B.ByteString
build = BL.toStrict . toLazyByteString

be32 :: Word32 -> B.ByteString
be32 = build . putU32

be64 :: Word64 -> B.ByteString
be64 = build . putU64

getBe64At :: B.ByteString -> Int -> Word64
getBe64At bs o = foldl (\a i -> (a `shiftL` 8) .|. fromIntegral (BU.unsafeIndex bs (o + i))) 0 [0 .. 7]

-- ---------------------------------------------------------------------------
-- Reading: a position-tracking parser that fails instead of reading out of bounds.

newtype Get a = Get { unGet :: B.ByteString -> Int -> Either String (a, Int) }

instance Functor Get where
  fmap f (Get g) = Get $ \bs o -> case g bs o of
    Left e        -> Left e
    Right (a, o') -> Right (f a, o')

instance Applicative Get where
  pure a = Get $ \_ o -> Right (a, o)
  Get f <*> Get g = Get $ \bs o -> case f bs o of
    Left e        -> Left e
    Right (h, o') -> case g bs o' of
      Left e         -> Left e
      Right (a, o'') -> Right (h a, o'')

instance Monad Get where
  Get g >>= k = Get $ \bs o -> case g bs o of
    Left e        -> Left e
    Right (a, o') -> unGet (k a) bs o'

-- | Run a parser starting at an offset; returns the value only.
runGet :: Get a -> B.ByteString -> Int -> Either String a
runGet (Get g) bs o = fst <$> g bs o

failGet :: String -> Get a
failGet msg = Get $ \_ o -> Left (msg ++ " at offset " ++ show o)

need :: Int -> Get ()
need n = Get $ \bs o ->
  if n < 0 || o + n > B.length bs
    then Left ("unexpected end of data (wanted " ++ show n ++ " bytes at offset " ++ show o ++ ")")
    else Right ((), o)

getRaw :: Int -> Get B.ByteString
getRaw n = need n >> Get (\bs o -> Right (B.take n (B.drop o bs), o + n))

getU8 :: Get Word8
getU8 = B.head <$> getRaw 1

getU16 :: Get Word16
getU16 = B.foldl' (\a b -> (a `shiftL` 8) .|. fromIntegral b) 0 <$> getRaw 2

getU32 :: Get Word32
getU32 = B.foldl' (\a b -> (a `shiftL` 8) .|. fromIntegral b) 0 <$> getRaw 4

getU64 :: Get Word64
getU64 = B.foldl' (\a b -> (a `shiftL` 8) .|. fromIntegral b) 0 <$> getRaw 8

-- | u32 length, then the bytes.
getBytes :: Get B.ByteString
getBytes = getU32 >>= getRaw . fromIntegral

getText :: Get Text
getText = TE.decodeUtf8With TE.lenientDecode <$> getBytes

-- | Reads what 'putVar' wrote.  More than 10 bytes, or a 10th byte that does not fit in 64 bits, is an error.
getVar :: Get Word64
getVar = go 0 0
  where
    go :: Int -> Word64 -> Get Word64
    go sh acc = do
      b <- getU8
      let acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` sh)
      if sh == 63 && b > 1 then failGet "variable-length number does not fit in 64 bits"
      else if b .&. 0x80 == 0 then pure acc'
      else go (sh + 7) acc'

-- | Reads what 'putVarBytes' wrote.
getVarBytes :: Get B.ByteString
getVarBytes = getVar >>= \n ->
  if n > fromIntegral (maxBound :: Int) then failGet "length out of range" else getRaw (fromIntegral n)

getVarText :: Get Text
getVarText = TE.decodeUtf8With TE.lenientDecode <$> getVarBytes

getPos :: Get Int
getPos = Get $ \_ o -> Right (o, o)

atEnd :: Get Bool
atEnd = Get $ \bs o -> Right (o >= B.length bs, o)

-- ---------------------------------------------------------------------------
-- CRC-32 (IEEE), used for tail-log records so a torn write is noticed.

crcTable :: UArray Int Word32
crcTable = listArray (0, 255) [ go (fromIntegral n) (8 :: Int) | n <- [0 .. 255 :: Int] ]
  where go c 0 = c
        go c k = go (if c .&. 1 == 1 then 0xEDB88320 `xor` (c `shiftR` 1) else c `shiftR` 1) (k - 1)

crc32 :: B.ByteString -> Word32
crc32 = complement . B.foldl' step 0xFFFFFFFF
  where step c b = (crcTable ! fromIntegral ((c `xor` fromIntegral b) .&. 0xFF)) `xor` (c `shiftR` 8)
