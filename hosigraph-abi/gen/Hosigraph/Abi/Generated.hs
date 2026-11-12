{-# LANGUAGE ForeignFunctionInterface #-}
-- hosi -- written by setu from the description of the hosi API. Do not edit.
--
-- The Haskell half of the C ABI. The functions of hosi.h are in the C layer, which lets a call
-- through to these only while the Haskell runtime is running.
module Hosigraph.Abi.Generated () where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import           Data.Word (Word8, Word64)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..), CSize(..))
import           Foreign.Ptr (FunPtr, Ptr, castPtr)
import           System.IO.Unsafe (unsafePerformIO)

import           Setu.Abi
import           Setu.Ticket (Done, statusCode)
import           Hosigraph.Abi.Library (library)

-- | The callback of the host: id, status, body, length, the host's own pointer.
type CDone = Word64 -> CInt -> Ptr Word8 -> CSize -> Ptr () -> IO ()

-- A safe call: the host's callback may take its time, and may call back into the library.
foreign import ccall "dynamic" callDone :: FunPtr CDone -> CDone

abi :: Abi
abi = unsafePerformIO (newAbi library)
{-# NOINLINE abi #-}

-- | The host's callback as a Done. The body is valid for the length of the call, as the header says.
hostDone :: FunPtr CDone -> Ptr () -> Done
hostDone cb ud i st body =
  BU.unsafeUseAsCStringLen body $ \(p, n) ->
    callDone cb i (fromIntegral (statusCode st)) (castPtr p) (fromIntegral n) ud

hsOpen :: CString -> IO Word64
hsOpen s = B.packCString s >>= abiOpen abi

hsClose :: Word64 -> IO ()
hsClose = abiClose abi

-- | 0 accepted; 1 the id is waiting; 2 too many waiting; 3 closed; 4 not a handle that is open.
-- The request is copied before this returns, so the host may free its buffer.
hsSubmit :: Word64 -> Word64 -> Ptr Word8 -> CSize -> FunPtr CDone -> Ptr () -> IO CInt
hsSubmit h i p n cb ud = do
  req <- B.packCStringLen (castPtr p, fromIntegral n)
  r <- abiSubmit abi h i req (hostDone cb ud)
  pure (fromIntegral (fromEnum r))

-- | 0, or 1 if the handle is not open.
hsCancel :: Word64 -> Word64 -> IO CInt
hsCancel h i = (\found -> if found then 0 else 1) <$> abiCancel abi h i

hsCloseAll :: IO ()
hsCloseAll = abiCloseAll abi

foreign export ccall "hosi_hs_open" hsOpen :: CString -> IO Word64
foreign export ccall "hosi_hs_close" hsClose :: Word64 -> IO ()
foreign export ccall "hosi_hs_submit" hsSubmit :: Word64 -> Word64 -> Ptr Word8 -> CSize -> FunPtr CDone -> Ptr () -> IO CInt
foreign export ccall "hosi_hs_cancel" hsCancel :: Word64 -> Word64 -> IO CInt
foreign export ccall "hosi_hs_close_all" hsCloseAll :: IO ()
