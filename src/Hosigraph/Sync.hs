-- |
-- Module      : Hosigraph.Sync
-- Description : fsync for files and directories (durability, not just flushing).
module Hosigraph.Sync (syncFile, syncDir, atomicReplace) where

import Control.Exception (bracket)
import System.Directory (renameFile)
import System.FilePath (takeDirectory)
import System.Posix.IO (OpenMode(ReadOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Unistd (fileSynchronise)

-- | Make a file's bytes durable.
syncFile :: FilePath -> IO ()
syncFile = fsyncPath

-- | Make a directory's entries (creates, renames) durable.
-- (A directory is opened read-only like a file; POSIX lets fsync work on that descriptor.)
syncDir :: FilePath -> IO ()
syncDir = fsyncPath

fsyncPath :: FilePath -> IO ()
fsyncPath p = bracket (openFd p ReadOnly defaultFileFlags) closeFd fileSynchronise

-- | The "write new, then swap" step: @tmp@ must already be fully written.
-- fsync the new file, rename it over the target (atomic on POSIX), fsync the directory.
atomicReplace :: FilePath -> FilePath -> IO ()
atomicReplace tmp target = do
  syncFile tmp
  renameFile tmp target
  syncDir (takeDirectory target)
