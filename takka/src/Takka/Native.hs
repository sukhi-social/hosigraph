{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Takka.Native
-- Description : Raw FFI to dlmdb (Datalevin's fork of LMDB).
--
-- Everything here is a thin, unsafe-ish mirror of the C API.  Use "Takka.KV"
-- for the bracketed, exception-safe interface.
--
-- Safe/unsafe policy: calls that can block or fsync (@env_open@, @txn_begin@,
-- @txn_commit@, @env_sync@) are imported @safe@; the hot-path calls
-- (@get@, @put@, @cursor_get@, rank, count) are imported @unsafe@ because they
-- are short and never call back into Haskell.
module Takka.Native
  ( -- * Opaque C types
    MDB_env, MDB_txn, MDB_cursor, MDB_val, MDB_dbi
    -- * Environment
  , c_env_create, c_env_set_mapsize, c_env_set_maxdbs, c_env_open
  , c_env_close, c_env_sync
    -- * Transactions
  , c_txn_begin, c_txn_commit, c_txn_abort
    -- * DBIs and data access
  , c_dbi_open, c_get, c_put, c_del
    -- * Cursors
  , c_cursor_open, c_cursor_close, c_cursor_get
    -- * dlmdb extensions (order statistics)
  , c_count_all, c_count_range, c_range_count_keys
  , c_get_rank, c_get_key_rank
    -- * Misc
  , c_strerror
    -- * Constants
  , mdbNotFound, mdbBadValSize
  , flagNoTls, flagNoSubDir, flagNoSync, flagRdOnly
  , flagCreate, flagDupSort, flagCounted, flagPrefixCompression
  , countLowerIncl, countUpperIncl
  , opFirst, opNext, opSetRange, opLast, opPrev
    -- * MDB_val marshalling
  , valSize, withVal, peekVal, allocaVal
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import           Foreign
import           Foreign.C
import           System.Posix.Types (CMode(..))

data MDB_env
data MDB_txn
data MDB_cursor
-- | @MDB_val { size_t mv_size; void *mv_data; }@ — handled via byte offsets.
data MDB_val
type MDB_dbi = CUInt

foreign import ccall safe   "mdb_env_create"      c_env_create      :: Ptr (Ptr MDB_env) -> IO CInt
foreign import ccall unsafe "mdb_env_set_mapsize" c_env_set_mapsize :: Ptr MDB_env -> CSize -> IO CInt
foreign import ccall unsafe "mdb_env_set_maxdbs"  c_env_set_maxdbs  :: Ptr MDB_env -> MDB_dbi -> IO CInt
foreign import ccall safe   "mdb_env_open"        c_env_open        :: Ptr MDB_env -> CString -> CUInt -> CMode -> IO CInt
foreign import ccall safe   "mdb_env_close"       c_env_close       :: Ptr MDB_env -> IO ()
foreign import ccall safe   "mdb_env_sync"        c_env_sync        :: Ptr MDB_env -> CInt -> IO CInt

foreign import ccall safe   "mdb_txn_begin"       c_txn_begin       :: Ptr MDB_env -> Ptr MDB_txn -> CUInt -> Ptr (Ptr MDB_txn) -> IO CInt
foreign import ccall safe   "mdb_txn_commit"      c_txn_commit      :: Ptr MDB_txn -> IO CInt
foreign import ccall safe   "mdb_txn_abort"       c_txn_abort       :: Ptr MDB_txn -> IO ()

foreign import ccall unsafe "mdb_dbi_open"        c_dbi_open        :: Ptr MDB_txn -> CString -> CUInt -> Ptr MDB_dbi -> IO CInt
foreign import ccall unsafe "mdb_get"             c_get             :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import ccall unsafe "mdb_put"             c_put             :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
foreign import ccall unsafe "mdb_del"             c_del             :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt

foreign import ccall unsafe "mdb_cursor_open"     c_cursor_open     :: Ptr MDB_txn -> MDB_dbi -> Ptr (Ptr MDB_cursor) -> IO CInt
foreign import ccall unsafe "mdb_cursor_close"    c_cursor_close    :: Ptr MDB_cursor -> IO ()
foreign import ccall unsafe "mdb_cursor_get"      c_cursor_get      :: Ptr MDB_cursor -> Ptr MDB_val -> Ptr MDB_val -> CInt -> IO CInt

-- dlmdb: order statistics (databases opened with MDB_COUNTED)
foreign import ccall unsafe "mdb_count_all"         c_count_all         :: Ptr MDB_txn -> MDB_dbi -> CUInt -> Ptr Word64 -> IO CInt
foreign import ccall unsafe "mdb_count_range"       c_count_range       :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> Ptr Word64 -> IO CInt
foreign import ccall unsafe "mdb_range_count_keys"  c_range_count_keys  :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> Ptr Word64 -> IO CInt
foreign import ccall unsafe "mdb_get_rank"          c_get_rank          :: Ptr MDB_txn -> MDB_dbi -> Word64 -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import ccall unsafe "mdb_get_key_rank"      c_get_key_rank      :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> Ptr Word64 -> IO CInt

foreign import ccall unsafe "mdb_strerror"        c_strerror        :: CInt -> IO CString

-- ---------------------------------------------------------------------------
-- Constants (values taken from dlmdb.h)

mdbNotFound, mdbBadValSize :: CInt
mdbNotFound   = -30798
mdbBadValSize = -30781

flagNoSubDir, flagNoSync, flagRdOnly, flagNoTls :: CUInt
flagNoSubDir = 0x4000
flagNoSync   = 0x10000
flagRdOnly   = 0x20000
flagNoTls    = 0x200000

flagCreate, flagDupSort, flagCounted, flagPrefixCompression :: CUInt
flagCreate            = 0x40000
flagDupSort           = 0x04
flagCounted           = 0x80
flagPrefixCompression = 0x100

-- | Experiments with dlmdb show that @flags = 0@ makes /both/ ends exclusive.
countLowerIncl, countUpperIncl :: CUInt
countLowerIncl = 0x02
countUpperIncl = 0x04

opFirst, opNext, opSetRange, opLast, opPrev :: CInt
opFirst    = 0
opLast     = 6
opNext     = 8
opPrev     = 12
opSetRange = 17

-- ---------------------------------------------------------------------------
-- MDB_val helpers

-- | Size in bytes of an @MDB_val@ on this platform.
valSize :: Int
valSize = 2 * sizeOf (undefined :: CSize)

allocaVal :: (Ptr MDB_val -> IO a) -> IO a
allocaVal = allocaBytes valSize

-- | Expose a 'B.ByteString' as an @MDB_val@ (no copy; valid inside the action).
withVal :: B.ByteString -> (Ptr MDB_val -> IO a) -> IO a
withVal bs act =
  BU.unsafeUseAsCStringLen bs $ \(p, n) ->
    allocaVal $ \v -> do
      pokeByteOff v 0 (fromIntegral n :: CSize)
      pokeByteOff v (sizeOf (undefined :: CSize)) p
      act v

-- | Copy the bytes an @MDB_val@ points at (LMDB memory is only valid until the
-- transaction ends, so we always copy).
peekVal :: Ptr MDB_val -> IO B.ByteString
peekVal v = do
  n <- peekByteOff v 0 :: IO CSize
  p <- peekByteOff v (sizeOf (undefined :: CSize)) :: IO (Ptr CChar)
  B.packCStringLen (p, fromIntegral n)
