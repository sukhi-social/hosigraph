-- |
-- Module      : Hosigraph.Bin
-- Description : Big-endian binary builders and a tiny bounds-checked parser.
module Hosigraph.Bin
  ( -- * Writing
    Builder', put, putU8, putU16, putU32, putU64, putBytes, putText, build
    -- * Reading
  , Get, runGet, failGet, getU8, getU16, getU32, getU64, getRaw, getBytes, getText, getPos, atEnd
  , be32, be64, getBe64At
  , crc32
  ) where

import           Data.Array.Unboxed (UArray, listArray, (!))
import           Data.Bits
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
