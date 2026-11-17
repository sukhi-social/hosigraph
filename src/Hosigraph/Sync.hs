-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE CPP #-}
-- |
-- Module      : Hosigraph.Sync
-- Description : fsync for files and directories (durability, not just flushing).
module Hosigraph.Sync (syncFile, syncDir, atomicReplace) where

import Control.Exception (bracket)
import System.Directory (renameFile)
import System.FilePath (takeDirectory)
#if defined(mingw32_HOST_OS)
import Data.Bits ((.|.))
import System.Win32.File
  ( closeHandle, createFile, fILE_ATTRIBUTE_NORMAL, fILE_SHARE_DELETE, fILE_SHARE_READ
  , fILE_SHARE_WRITE, flushFileBuffers, gENERIC_WRITE, oPEN_EXISTING )
#else
import System.Posix.IO (OpenMode(ReadOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Unistd (fileSynchronise)
#endif

-- | Make a file's bytes durable.
syncFile :: FilePath -> IO ()
syncFile = fsyncPath

-- | Make a directory's entries (creates, renames) durable.
-- (A directory is opened read-only like a file; POSIX lets fsync work on that descriptor.)
--
-- On Windows this does nothing, for now.  A directory cannot be flushed there the way it is on
-- POSIX, and what to promise instead (say, renames done with MOVEFILE_WRITE_THROUGH) has not
-- been decided.  So on Windows a file that was synced stays, but a create or a rename may be
-- lost if the power goes at the wrong moment.
syncDir :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
syncDir _ = pure ()
#else
syncDir = fsyncPath
#endif

fsyncPath :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
-- FlushFileBuffers wants a handle that can write; the others may go on using the file meanwhile.
fsyncPath p = bracket open closeHandle flushFileBuffers
  where
    open  = createFile p gENERIC_WRITE share Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing
    share = fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE
#else
fsyncPath p = bracket (openFd p ReadOnly defaultFileFlags) closeFd fileSynchronise
#endif

-- | The "write new, then swap" step: @tmp@ must already be fully written.
-- fsync the new file, rename it over the target (atomic on POSIX), fsync the directory.
atomicReplace :: FilePath -> FilePath -> IO ()
atomicReplace tmp target = do
  syncFile tmp
  renameFile tmp target
  syncDir (takeDirectory target)
