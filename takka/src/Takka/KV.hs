-- |
-- Module      : Takka.KV
-- Description : Exception-safe ordered key-value store on top of dlmdb.
--
-- * Readers never block writers (MVCC) — a read transaction is a snapshot.
-- * Writers are serialised by an 'MVar' (so Haskell threads queue up instead
--   of blocking an OS thread inside a C mutex) and run in a /bound/ thread
--   when the threaded RTS is used, because LMDB write transactions have
--   OS-thread affinity.
-- * Do not nest 'withWriteTxn' inside 'withWriteTxn' (it would wait forever
--   for its own lock).
module Takka.KV
  ( -- * Errors
    LmdbError(..)
    -- * Environment
  , Env, EnvConfig(..), defaultEnvConfig
  , openEnv, closeEnv, withEnv, syncEnv
    -- * Transactions
  , Txn, withReadTxn, withWriteTxn
    -- * Databases (DBIs)
  , Dbi, DbiFlags, dbiPlain, dbiCounted, dbiPrefixCompression, dbiDupSort, openDbi
    -- * Reads and writes
  , put, get, del
    -- * Range scans
  , scanFrom, scanPrefix
    -- * Order statistics (dlmdb extension; DBI must be 'dbiCounted')
  , countAll, countRange, countPrefix, getRank, keyRank
    -- * Key helpers
  , prefixSuccessor, maxKeySize
  ) where

import           Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import           Control.Concurrent.MVar
import           Control.Exception
import qualified Data.ByteString as B
import           Foreign
import           Foreign.C
import           System.Directory (createDirectoryIfMissing)

import           Takka.Native

-- ---------------------------------------------------------------------------
-- Errors

data LmdbError = LmdbError
  { lmdbCode    :: !Int
  , lmdbMessage :: !String
  , lmdbContext :: !String
  }

instance Show LmdbError where
  show (LmdbError c m ctx) = "LmdbError(" ++ show c ++ ") in " ++ ctx ++ ": " ++ m

instance Exception LmdbError

check :: String -> CInt -> IO ()
check ctx rc
  | rc == 0   = pure ()
  | otherwise = do
      msg <- c_strerror rc >>= peekCString
      throwIO (LmdbError (fromIntegral rc) msg ctx)

-- | LMDB's compile-time limit for a key (and for DUPSORT values).
maxKeySize :: Int
maxKeySize = 511

-- ---------------------------------------------------------------------------
-- Environment

data Env = Env !(Ptr MDB_env) !(MVar ())

envWriteLock :: Env -> MVar ()
envWriteLock (Env _ l) = l

data EnvConfig = EnvConfig
  { cfgMapSize :: !Word64   -- ^ Maximum database size in bytes (sparse; 1 GiB default).
  , cfgMaxDbs  :: !Int      -- ^ Maximum number of named DBIs.
  , cfgNoSync  :: !Bool     -- ^ Skip fsync on commit (fast, but not durable on power loss).
  }

defaultEnvConfig :: EnvConfig
defaultEnvConfig = EnvConfig { cfgMapSize = 1 `shiftL` 30, cfgMaxDbs = 16, cfgNoSync = False }

openEnv :: FilePath -> EnvConfig -> IO Env
openEnv path cfg = do
  createDirectoryIfMissing True path
  p <- alloca $ \pp -> do
    check "mdb_env_create" =<< c_env_create pp
    peek pp
  let flags = flagNoTls .|. (if cfgNoSync cfg then flagNoSync else 0)
      go = do
        check "mdb_env_set_mapsize" =<< c_env_set_mapsize p (fromIntegral (cfgMapSize cfg))
        check "mdb_env_set_maxdbs"  =<< c_env_set_maxdbs p (fromIntegral (cfgMaxDbs cfg))
        withCString path $ \cpath ->
          check "mdb_env_open" =<< c_env_open p cpath flags 0o664
  go `onException` c_env_close p
  lock <- newMVar ()
  pure (Env p lock)

closeEnv :: Env -> IO ()
closeEnv (Env p _) = c_env_close p

withEnv :: FilePath -> EnvConfig -> (Env -> IO a) -> IO a
withEnv path cfg = bracket (openEnv path cfg) closeEnv

syncEnv :: Env -> IO ()
syncEnv (Env p _) = check "mdb_env_sync" =<< c_env_sync p 1

-- ---------------------------------------------------------------------------
-- Transactions

-- | A transaction handle (pointer, is-writable).
data Txn = Txn !(Ptr MDB_txn) !Bool

beginTxn :: Env -> CUInt -> IO (Ptr MDB_txn)
beginTxn (Env e _) flags =
  alloca $ \pp -> do
    check "mdb_txn_begin" =<< c_txn_begin e nullPtr flags pp
    peek pp

-- | Run an action against a consistent snapshot.
withReadTxn :: Env -> (Txn -> IO a) -> IO a
withReadTxn env act =
  bracket (beginTxn env flagRdOnly) c_txn_abort (\p -> act (Txn p False))

inBound :: IO a -> IO a
inBound = if rtsSupportsBoundThreads then runInBoundThread else id

-- | Run an action in a write transaction; commit on success, abort on any
-- exception.  All-or-nothing.
withWriteTxn :: Env -> (Txn -> IO a) -> IO a
withWriteTxn env act =
  withMVar (envWriteLock env) $ \_ -> inBound $ mask $ \restore -> do
    p <- beginTxn env 0
    r <- restore (act (Txn p True)) `onException` c_txn_abort p
    rc <- c_txn_commit p          -- on failure LMDB has already freed the txn
    check "mdb_txn_commit" rc
    pure r

-- ---------------------------------------------------------------------------
-- DBIs

newtype Dbi = Dbi MDB_dbi deriving (Eq, Show)

newtype DbiFlags = DbiFlags CUInt

instance Semigroup DbiFlags where DbiFlags a <> DbiFlags b = DbiFlags (a .|. b)
instance Monoid    DbiFlags where mempty = DbiFlags 0

dbiPlain, dbiCounted, dbiPrefixCompression, dbiDupSort :: DbiFlags
dbiPlain             = mempty
dbiCounted           = DbiFlags flagCounted
dbiPrefixCompression = DbiFlags flagPrefixCompression
dbiDupSort           = DbiFlags flagDupSort

-- | Open (creating if needed) a named database.  Do this once, in a write
-- transaction, and keep the handle.
openDbi :: Txn -> String -> DbiFlags -> IO Dbi
openDbi (Txn t _) name (DbiFlags fl) =
  withCString name $ \cname -> alloca $ \pd -> do
    check ("mdb_dbi_open " ++ name) =<< c_dbi_open t cname (fl .|. flagCreate) pd
    Dbi <$> peek pd

-- ---------------------------------------------------------------------------
-- Reads / writes

put :: Txn -> Dbi -> B.ByteString -> B.ByteString -> IO ()
put (Txn t _) (Dbi d) k v =
  withVal k $ \kp -> withVal v $ \vp ->
    check "mdb_put" =<< c_put t d kp vp 0

get :: Txn -> Dbi -> B.ByteString -> IO (Maybe B.ByteString)
get (Txn t _) (Dbi d) k =
  withVal k $ \kp -> allocaVal $ \vp -> do
    rc <- c_get t d kp vp
    if rc == mdbNotFound then pure Nothing
      else check "mdb_get" rc >> Just <$> peekVal vp

-- | Delete a key; 'False' if it was not there.
del :: Txn -> Dbi -> B.ByteString -> IO Bool
del (Txn t _) (Dbi d) k =
  withVal k $ \kp -> do
    rc <- c_del t d kp nullPtr
    if rc == mdbNotFound then pure False
      else check "mdb_del" rc >> pure True

-- ---------------------------------------------------------------------------
-- Scans

-- | Walk forward from the first key @>= start@ while @keep key@ holds.
scanFrom :: Txn -> Dbi -> B.ByteString -> (B.ByteString -> Bool) -> IO [(B.ByteString, B.ByteString)]
scanFrom (Txn t _) (Dbi d) start keep =
  bracket openCursor c_cursor_close $ \cur ->
    allocaVal $ \kp -> allocaVal $ \vp -> do
      rc0 <- if B.null start            -- LMDB rejects an empty key for SET_RANGE
               then c_cursor_get cur kp vp opFirst
               else withVal start $ \sp -> do
                      -- SET_RANGE reads *and rewrites* the key slot, so copy our
                      -- bytes into a fresh slot owned by this call.
                      copyVal sp kp
                      c_cursor_get cur kp vp opSetRange
      loop cur kp vp rc0 []
  where
    openCursor = alloca $ \pc -> do
      check "mdb_cursor_open" =<< c_cursor_open t d pc
      peek pc
    loop cur kp vp rc acc
      | rc == mdbNotFound = pure (reverse acc)
      | otherwise = do
          check "mdb_cursor_get" rc
          k <- peekVal kp
          if keep k
            then do v <- peekVal vp
                    rc' <- c_cursor_get cur kp vp opNext
                    loop cur kp vp rc' ((k, v) : acc)
            else pure (reverse acc)
    copyVal src dst = do
      n <- peekByteOff src 0 :: IO CSize
      p <- peekByteOff src (sizeOf (undefined :: CSize)) :: IO (Ptr ())
      pokeByteOff dst 0 n
      pokeByteOff dst (sizeOf (undefined :: CSize)) p

-- | All entries whose key starts with the given prefix, in key order.
scanPrefix :: Txn -> Dbi -> B.ByteString -> IO [(B.ByteString, B.ByteString)]
scanPrefix txn dbi prefix = scanFrom txn dbi prefix (B.isPrefixOf prefix)

-- ---------------------------------------------------------------------------
-- Order statistics (O(log n))

countAll :: Txn -> Dbi -> IO Word64
countAll (Txn t _) (Dbi d) = alloca $ \po -> do
  check "mdb_count_all" =<< c_count_all t d 0 po
  peek po

withMaybeVal :: Maybe B.ByteString -> (Ptr MDB_val -> IO a) -> IO a
withMaybeVal Nothing   act = act nullPtr
withMaybeVal (Just bs) act = withVal bs act

-- | Number of keys in @[low, high)@.  'Nothing' means unbounded on that side.
countRange :: Txn -> Dbi -> Maybe B.ByteString -> Maybe B.ByteString -> IO Word64
countRange (Txn t _) (Dbi d) lo hi = do
  refreshDbi t d
  withMaybeVal lo $ \lp -> withMaybeVal hi $ \hp -> alloca $ \po -> do
    check "mdb_count_range" =<< c_count_range t d lp hp countLowerIncl po
    peek po

-- | Work around a dlmdb quirk found while building this library: in a fresh
-- transaction the per-DBI metadata is \"stale\" until something refreshes it.
-- @mdb_count_all@ does that refresh, but @mdb_count_range@ reads the entry count
-- /before/ any refresh and therefore reports 0 on its first calls.  Calling
-- @mdb_count_all@ first makes the result correct (afterwards it is just a flag
-- check).  There is a regression test for this in @test/Spec.hs@.
refreshDbi :: Ptr MDB_txn -> MDB_dbi -> IO ()
refreshDbi t d = alloca $ \po -> check "mdb_count_all" =<< c_count_all t d 0 po

-- | Number of keys that start with the prefix — the planner's selectivity probe.
countPrefix :: Txn -> Dbi -> B.ByteString -> IO Word64
countPrefix txn dbi prefix =
  countRange txn dbi (if B.null prefix then Nothing else Just prefix) (prefixSuccessor prefix)

-- | Entry at a zero-based position in key order.
getRank :: Txn -> Dbi -> Word64 -> IO (Maybe (B.ByteString, B.ByteString))
getRank (Txn t _) (Dbi d) r = allocaVal $ \kp -> allocaVal $ \vp -> do
  rc <- c_get_rank t d r kp vp
  if rc == mdbNotFound then pure Nothing
    else do check "mdb_get_rank" rc
            k <- peekVal kp
            v <- peekVal vp
            pure (Just (k, v))

-- | Zero-based position of an existing key ('Nothing' if absent).
keyRank :: Txn -> Dbi -> B.ByteString -> IO (Maybe Word64)
keyRank (Txn t _) (Dbi d) k = withVal k $ \kp -> alloca $ \pr -> do
  rc <- c_get_key_rank t d kp nullPtr pr
  if rc == mdbNotFound then pure Nothing
    else check "mdb_get_key_rank" rc >> Just <$> peek pr

-- ---------------------------------------------------------------------------

-- | Smallest key that is greater than every key with this prefix
-- ('Nothing' when no such key exists, e.g. empty or all-0xFF prefix).
prefixSuccessor :: B.ByteString -> Maybe B.ByteString
prefixSuccessor p =
  case B.dropWhileEnd (== 0xFF) p of
    stripped | B.null stripped -> Nothing
             | otherwise       -> Just (B.snoc (B.init stripped) (B.last stripped + 1 :: Word8))

