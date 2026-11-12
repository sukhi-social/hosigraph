-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Setu.Gen.HsExport
-- Description : The Haskell half of the C ABI: a module of @foreign export@s, written from an 'Api'.
--
-- The module is a skin.  It turns pointers into 'ByteString's and a C function pointer into a
-- 'Setu.Ticket.Done', and gives everything else to "Setu.Abi".  It exports under the names
-- @<prefix>_hs_*@; the public @<prefix>_*@ functions of the header are the C layer's
-- ("Setu.Gen.CAbi"), which stands in front of these and decides whether a call may reach Haskell
-- at all.
--
-- The library (a 'Setu.Abi.Library' value) is not named here; the module that holds it is given.
module Setu.Gen.HsExport
  ( HsExportConfig(..)
  , hsExport
  , hsSymbols
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Setu.Spec

data HsExportConfig = HsExportConfig
  { hsModule        :: Text   -- ^ the name of the module written, @"Demo.Abi.Generated"@
  , hsLibraryModule :: Text   -- ^ the module that holds the library, @"Demo.Abi.Library"@
  , hsLibraryValue  :: Text   -- ^ the 'Setu.Abi.Library' in it, @"library"@
  } deriving (Eq, Show)

-- | The symbols the module exports, which the C layer calls.
hsSymbols :: Api -> [Text]
hsSymbols api = [ apiName api <> "_hs_" <> n | n <- ["open", "close", "submit", "cancel", "close_all"] ]

hsExport :: HsExportConfig -> Api -> Text
hsExport cfg api = T.unlines
  [ "{-# LANGUAGE ForeignFunctionInterface #-}"
  , "-- " <> p <> " -- written by setu from the description of the " <> p <> " API. Do not edit."
  , "--"
  , "-- The Haskell half of the C ABI. The functions of " <> p <> ".h are in the C layer, which lets a call"
  , "-- through to these only while the Haskell runtime is running."
  , "module " <> hsModule cfg <> " () where"
  , ""
  , "import qualified Data.ByteString as B"
  , "import qualified Data.ByteString.Unsafe as BU"
  , "import           Data.Word (Word8, Word64)"
  , "import           Foreign.C.String (CString)"
  , "import           Foreign.C.Types (CInt(..), CSize(..))"
  , "import           Foreign.Ptr (FunPtr, Ptr, castPtr)"
  , "import           System.IO.Unsafe (unsafePerformIO)"
  , ""
  , "import           Setu.Abi"
  , "import           Setu.Ticket (Done, statusCode)"
  , "import           " <> hsLibraryModule cfg <> " (" <> hsLibraryValue cfg <> ")"
  , ""
  , "-- | The callback of the host: id, status, body, length, the host's own pointer."
  , "type CDone = Word64 -> CInt -> Ptr Word8 -> CSize -> Ptr () -> IO ()"
  , ""
  , "-- A safe call: the host's callback may take its time, and may call back into the library."
  , "foreign import ccall \"dynamic\" callDone :: FunPtr CDone -> CDone"
  , ""
  , "abi :: Abi"
  , "abi = unsafePerformIO (newAbi " <> hsLibraryValue cfg <> ")"
  , "{-# NOINLINE abi #-}"
  , ""
  , "-- | The host's callback as a Done. The body is valid for the length of the call, as the header says."
  , "hostDone :: FunPtr CDone -> Ptr () -> Done"
  , "hostDone cb ud i st body ="
  , "  BU.unsafeUseAsCStringLen body $ \\(p, n) ->"
  , "    callDone cb i (fromIntegral (statusCode st)) (castPtr p) (fromIntegral n) ud"
  , ""
  , "hsOpen :: CString -> IO Word64"
  , "hsOpen s = B.packCString s >>= abiOpen abi"
  , ""
  , "hsClose :: Word64 -> IO ()"
  , "hsClose = abiClose abi"
  , ""
  , "-- | 0 accepted; 1 the id is waiting; 2 too many waiting; 3 closed; 4 not a handle that is open."
  , "-- The request is copied before this returns, so the host may free its buffer."
  , "hsSubmit :: Word64 -> Word64 -> Ptr Word8 -> CSize -> FunPtr CDone -> Ptr () -> IO CInt"
  , "hsSubmit h i p n cb ud = do"
  , "  req <- B.packCStringLen (castPtr p, fromIntegral n)"
  , "  r <- abiSubmit abi h i req (hostDone cb ud)"
  , "  pure (fromIntegral (fromEnum r))"
  , ""
  , "-- | 0, or 1 if the handle is not open."
  , "hsCancel :: Word64 -> Word64 -> IO CInt"
  , "hsCancel h i = (\\found -> if found then 0 else 1) <$> abiCancel abi h i"
  , ""
  , "hsCloseAll :: IO ()"
  , "hsCloseAll = abiCloseAll abi"
  , ""
  , "foreign export ccall \"" <> p <> "_hs_open\" hsOpen :: CString -> IO Word64"
  , "foreign export ccall \"" <> p <> "_hs_close\" hsClose :: Word64 -> IO ()"
  , "foreign export ccall \"" <> p <> "_hs_submit\" hsSubmit :: Word64 -> Word64 -> Ptr Word8 -> CSize -> FunPtr CDone -> Ptr () -> IO CInt"
  , "foreign export ccall \"" <> p <> "_hs_cancel\" hsCancel :: Word64 -> Word64 -> IO CInt"
  , "foreign export ccall \"" <> p <> "_hs_close_all\" hsCloseAll :: IO ()"
  ]
  where p = apiName api
