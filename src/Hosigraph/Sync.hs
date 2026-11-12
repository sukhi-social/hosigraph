{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Hosigraph.Sync
-- Description : fsync for files and directories (durability, not just flushing).
module Hosigraph.Sync (syncFile, syncDir, atomicReplace) where

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..))
import System.Directory (renameFile)
import System.FilePath (takeDirectory)

foreign import ccall unsafe "hosi_fsync_path" c_fsync_path :: CString -> IO CInt
foreign import ccall unsafe "hosi_fsync_dir"  c_fsync_dir  :: CString -> IO CInt

-- | Make a file's bytes durable.
syncFile :: FilePath -> IO ()
syncFile p = withCString p $ \c -> throwErrnoIfMinus1_ ("fsync " ++ p) (c_fsync_path c)

-- | Make a directory's entries (creates, renames) durable.
syncDir :: FilePath -> IO ()
syncDir p = withCString p $ \c -> throwErrnoIfMinus1_ ("fsync dir " ++ p) (c_fsync_dir c)

-- | The "write new, then swap" step: @tmp@ must already be fully written.
-- fsync the new file, rename it over the target (atomic on POSIX), fsync the directory.
atomicReplace :: FilePath -> FilePath -> IO ()
atomicReplace tmp target = do
  syncFile tmp
  renameFile tmp target
  syncDir (takeDirectory target)
