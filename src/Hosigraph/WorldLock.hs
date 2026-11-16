-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Hosigraph.WorldLock
-- Description : One opener at a time: a lock on a world directory, held while it is open.
--
-- The files of a world have one writer in mind: the tail log is appended at an offset that
-- the writer remembers, and a seal writes @manifest.tmp@ and renames it.  Two openers of the same
-- directory (two processes, or two 'Hosigraph.Events.Events' in one) overwrite each other's
-- records and both are told that all went well.  So the first opener takes this lock, and
-- the second is refused ('Hosigraph.Event.WorldBusy') before it touches anything.
--
-- It is @flock(2)@ on a file, @LOCK_EX | LOCK_NB@.  The lock belongs to the open file, so
--
-- * it is released when the process dies, however it dies (a lock file left behind means nothing);
-- * a second open in the /same/ process is refused too (a POSIX record lock, @fcntl@, would not refuse it);
-- * the file is closed on @exec@, so the @java@ or @dtlv@ that hosigraph starts does not keep the lock alive.
--
-- On a network file system @flock@ may say yes to two machines, or no to both.  Not promised there.
-- There is no C source in this package; this module only asks libc for @flock@.
module Hosigraph.WorldLock (WorldLock, tryWorldLock, releaseWorldLock) where

import Data.Bits ((.|.))
import Foreign.C.Error (Errno, eAGAIN, eINTR, eWOULDBLOCK, getErrno, throwErrno)
import Foreign.C.Types (CInt(..))
import System.Posix.IO (OpenMode(ReadWrite), OpenFileFlags(..), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd(..))

foreign import ccall unsafe "flock" c_flock :: CInt -> CInt -> IO CInt

-- | A lock that is held.
newtype WorldLock = WorldLock Fd

-- The same on Linux and on macOS.
lockEx, lockNb :: CInt
lockEx = 2
lockNb = 4

-- | Take the lock on this file (made if it is not there).  'Nothing' when someone holds it.
tryWorldLock :: FilePath -> IO (Maybe WorldLock)
tryWorldLock path = do
  fd@(Fd n) <- openFd path ReadWrite defaultFileFlags { creat = Just 0o644, cloexec = True }
  let go = do
        r <- c_flock n (lockEx .|. lockNb)
        if r == 0 then pure (Just (WorldLock fd)) else do
          e <- getErrno
          case () of
            _ | e == eINTR -> go
              | busy e     -> closeFd fd >> pure Nothing
              | otherwise  -> closeFd fd >> throwErrno ("flock " ++ path)
  go
  where busy :: Errno -> Bool
        busy e = e == eWOULDBLOCK || e == eAGAIN

-- | Let go.  (Closing the file is what releases it.)  Call it once: a second call may close
-- a descriptor that has been reused.
releaseWorldLock :: WorldLock -> IO ()
releaseWorldLock (WorldLock fd) = closeFd fd
