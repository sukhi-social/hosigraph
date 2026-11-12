-- |
-- Module      : Hosigraph.Sha256
-- Description : SHA-256 in plain Haskell, so the manifest can be hash-signed
--               without adding a dependency beyond GHC's own packages.
--
-- Speed is not the goal here (it is a few MB/s); correctness is, and the test
-- suite checks it against known vectors.
module Hosigraph.Sha256 (sha256, sha256Hex, toHex) where

import           Control.Monad (forM_)
import           Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import           Data.Array.Unboxed (UArray, listArray, (!))
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import           Data.Word

roundK :: UArray Int Word32
roundK = listArray (0, 63)
  [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
  , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
  , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
  , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
  , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
  , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
  , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
  , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 ]

data H = H !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32

h0 :: H
h0 = H 0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a 0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19

be32At :: B.ByteString -> Int -> Word32
be32At bs o =
  (fromIntegral (BU.unsafeIndex bs o)       `shiftL` 24) .|.
  (fromIntegral (BU.unsafeIndex bs (o + 1)) `shiftL` 16) .|.
  (fromIntegral (BU.unsafeIndex bs (o + 2)) `shiftL` 8)  .|.
   fromIntegral (BU.unsafeIndex bs (o + 3))

schedule :: B.ByteString -> Int -> UArray Int Word32
schedule bs off = runSTUArray $ do
  w <- newArray (0, 63) 0
  forM_ [0 .. 15] $ \i -> writeArray w i (be32At bs (off + 4 * i))
  forM_ [16 .. 63] $ \i -> do
    w15 <- readArray w (i - 15)
    w2  <- readArray w (i - 2)
    w16 <- readArray w (i - 16)
    w7  <- readArray w (i - 7)
    let s0 = rotateR w15 7 `xor` rotateR w15 18 `xor` shiftR w15 3
        s1 = rotateR w2 17 `xor` rotateR w2 19 `xor` shiftR w2 10
    writeArray w i (w16 + s0 + w7 + s1)
  pure w

compress :: H -> B.ByteString -> Int -> H
compress hh@(H a0 b0 c0 d0 e0 f0 g0 h0') bs off = H (a0 + a') (b0 + b') (c0 + c') (d0 + d')
                                                   (e0 + e') (f0 + f') (g0 + g') (h0' + h')
  where
    w = schedule bs off
    H a' b' c' d' e' f' g' h' = foldl' step hh [0 .. 63]
    step (H a b c d e f g h) i =
      let s1  = rotateR e 6 `xor` rotateR e 11 `xor` rotateR e 25
          ch  = (e .&. f) `xor` (complement e .&. g)
          t1  = h + s1 + ch + roundK ! i + w ! i
          s0  = rotateR a 2 `xor` rotateR a 13 `xor` rotateR a 22
          maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
          t2  = s0 + maj
      in H (t1 + t2) a b c (d + t1) e f g

-- | The 32-byte digest.
sha256 :: B.ByteString -> B.ByteString
sha256 msg = B.pack (concatMap word32Bytes [a, b, c, d, e, f, g, h])
  where
    len     = B.length msg
    padLen  = (55 - len) `mod` 64
    bitLen  = fromIntegral len * 8 :: Word64
    padded  = B.concat [ msg, B.singleton 0x80, B.replicate padLen 0
                       , B.pack [ fromIntegral (bitLen `shiftR` s) | s <- [56, 48 .. 0] ] ]
    H a b c d e f g h = foldl' (\st o -> compress st padded o) h0 [0, 64 .. B.length padded - 64]
    word32Bytes x = [ fromIntegral (x `shiftR` s) | s <- [24, 16, 8, 0] ]

toHex :: B.ByteString -> String
toHex = concatMap hex2 . B.unpack
  where hex2 w = [digit (w `shiftR` 4), digit (w .&. 15)]
        digit n = "0123456789abcdef" !! fromIntegral n

sha256Hex :: B.ByteString -> String
sha256Hex = toHex . sha256
