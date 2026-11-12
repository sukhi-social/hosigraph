{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka.Store
-- Description : A durable EAV/AVE triple store on dlmdb.
--
-- Storage layout (one LMDB environment, four named DBIs):
--
-- * @eav@   — counted (optionally prefix-compressed); key = e ++ attrId ++ value; empty value
-- * @ave@   — counted (optionally prefix-compressed); key = attrId ++ value ++ e; empty value
-- * @attrs@ — attribute name -> (attrId, schema)
-- * @meta@  — counters: @max-eid@, @max-aid@, @schema-version@
--
-- Reads happen inside 'withDb' (an MVCC snapshot).  Writes are 'transact' calls,
-- each one atomic: either every operation applies or none does.
module Takka.Store
  ( -- * Connection
    Conn, openConn, openConnWith, closeConn, withConn, withConnWith
  , ConnConfig(..), defaultConnConfig
  , EnvConfig(..), defaultEnvConfig
    -- * Schema
  , updateSchema, getSchema
    -- * Writing
  , EntRef(..), TxVal(..), TxOp(..), TxReport(..), transact
    -- * Reading
  , Db, withDb, dbSchema
  , datoms, countDatoms, distinctEstimate
  , Bound(..), datomsInRange, countInRange
  , lookupRef, entity, allDatoms, datomCount
  , coerceValue
  ) where

import           Control.Exception (onException, throwIO)
import           Control.Monad
import qualified Data.ByteString as B
import           Data.IORef
import qualified Data.IntMap.Strict as IM
import           Data.List (nub)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word

import           Takka.Codec
import           Takka.KV
import           Takka.Types

-- ---------------------------------------------------------------------------
-- Attribute table

data AttrInfo = AttrInfo
  { aiId     :: !Word32
  , aiSchema :: !AttrSchema
  } deriving (Eq, Show)

data AttrTable = AttrTable
  { atByName :: !(M.Map Keyword AttrInfo)
  , atById   :: !(IM.IntMap Keyword)
  }

emptyTable :: AttrTable
emptyTable = AttrTable M.empty IM.empty

insertAttr :: Keyword -> AttrInfo -> AttrTable -> AttrTable
insertAttr k ai (AttrTable n i) =
  AttrTable (M.insert k ai n) (IM.insert (fromIntegral (aiId ai)) k i)

typeName :: ValueType -> T.Text
typeName t = case t of
  TAny -> "any"; TString -> "string"; TLong -> "long"; TDouble -> "double"
  TBool -> "boolean"; TKeyword -> "keyword"; TRef -> "ref"

encodeAttrVal :: AttrInfo -> B.ByteString
encodeAttrVal (AttrInfo aid (AttrSchema t c u)) =
  B.append (be32 aid) . TE.encodeUtf8 . T.intercalate "," $
    [ typeName t
    , case c of One -> "one"; Many -> "many"
    , case u of NotUnique -> "none"; UniqueIdentity -> "identity"; UniqueValue -> "value" ]

decodeAttrVal :: B.ByteString -> Maybe AttrInfo
decodeAttrVal bs
  | B.length bs < 4 = Nothing
  | otherwise = do
      let (a, rest) = B.splitAt 4 bs
      [t, c, u] <- Just (T.splitOn "," (TE.decodeUtf8 rest))
      ty <- lookup t [(typeName x, x) | x <- [minBound .. maxBound]]
      ca <- lookup c [("one", One), ("many", Many)]
      un <- lookup u [("none", NotUnique), ("identity", UniqueIdentity), ("value", UniqueValue)]
      pure (AttrInfo (getBe32 a) (AttrSchema ty ca un))

-- ---------------------------------------------------------------------------
-- Connection

data Conn = Conn
  { cEnv   :: !Env
  , cEav   :: !Dbi
  , cAve   :: !Dbi
  , cAttrs :: !Dbi
  , cMeta  :: !Dbi
  , cCache :: !(IORef (Word64, AttrTable))   -- ^ (schema-version, table)
  }

data ConnConfig = ConnConfig
  { ccEnv               :: EnvConfig
  , ccPrefixCompression :: Bool
    -- ^ Store index keys with shared-prefix compression (about 36% smaller files in our
    -- measurements).  __Off by default__: dlmdb's prefix compression can lose keys when
    -- adjacent keys share no leading byte and contain NUL bytes (see
    -- @docs/dlmdb-findings.md@).  Takka's own keys always share their first bytes and
    -- survived 460 fuzz trials, but until the upstream bug is fixed this is opt-in.
    -- The flag only matters when the database is /created/; a reopened database keeps
    -- the flags it was created with.
  }

defaultConnConfig :: ConnConfig
defaultConnConfig = ConnConfig { ccEnv = defaultEnvConfig, ccPrefixCompression = False }

-- | Open (creating if needed) a database directory, then apply a schema.
openConn :: FilePath -> Schema -> IO Conn
openConn = openConnWith defaultConnConfig

openConnWith :: ConnConfig -> FilePath -> Schema -> IO Conn
openConnWith cfg path schema = do
  env <- openEnv path (ccEnv cfg)
  let indexFlags = dbiCounted <> (if ccPrefixCompression cfg then dbiPrefixCompression else dbiPlain)
  (eav, ave, attrs, meta) <- withWriteTxn env $ \t -> do
    eav   <- openDbi t "eav"   indexFlags
    ave   <- openDbi t "ave"   indexFlags
    attrs <- openDbi t "attrs" dbiPlain
    meta  <- openDbi t "meta"  dbiPlain
    pure (eav, ave, attrs, meta)
  cache <- newIORef (maxBound, emptyTable)
  let conn = Conn env eav ave attrs meta cache
  updateSchema conn schema `onException` closeEnv env
  pure conn

closeConn :: Conn -> IO ()
closeConn = closeEnv . cEnv

withConn :: FilePath -> Schema -> (Conn -> IO a) -> IO a
withConn = withConnWith defaultConnConfig

withConnWith :: ConnConfig -> FilePath -> Schema -> (Conn -> IO a) -> IO a
withConnWith cfg path schema act = do
  c <- openConnWith cfg path schema
  r <- act c `onException` closeConn c
  closeConn c
  pure r

metaGet :: Conn -> Txn -> B.ByteString -> IO Word64
metaGet c t k = maybe 0 getBe64 <$> get t (cMeta c) k

metaPut :: Conn -> Txn -> B.ByteString -> Word64 -> IO ()
metaPut c t k w = put t (cMeta c) k (be64 w)

loadTable :: Conn -> Txn -> IO AttrTable
loadTable c t = do
  kvs <- scanPrefix t (cAttrs c) B.empty
  pure $ foldr (\(k, v) tb -> case decodeAttrVal v of
                   Just ai -> insertAttr (Keyword (TE.decodeUtf8 k)) ai tb
                   Nothing -> tb) emptyTable kvs

-- | Cached by schema-version: the version is read inside the same snapshot as
-- the data, so a cached table always matches the snapshot it is used with.
readTable :: Conn -> Txn -> IO AttrTable
readTable c t = do
  v <- metaGet c t "schema-version"
  (cv, tbl) <- readIORef (cCache c)
  if cv == v then pure tbl else do
    tbl' <- loadTable c t
    atomicModifyIORef' (cCache c) $ \old@(ov, _) ->
      if ov == maxBound || ov < v then ((v, tbl'), ()) else (old, ())
    pure tbl'

bumpSchemaVersion :: Conn -> Txn -> IO ()
bumpSchemaVersion c t = do
  v <- metaGet c t "schema-version"
  metaPut c t "schema-version" (v + 1)

-- ---------------------------------------------------------------------------
-- Schema

mergeSchema :: Keyword -> AttrSchema -> AttrSchema -> Either String AttrSchema
mergeSchema k old new
  | old == new                = Right new
  | old == defaultAttrSchema  = Right new       -- auto-created attribute being declared
  | asType old == asType new && asUnique old == asUnique new
    && asCard old == One && asCard new == Many = Right new   -- widening is lossless
  | otherwise = Left $ "schema conflict for " ++ show k ++ ": " ++ show old
                    ++ " -> " ++ show new ++ " (this change needs a data migration)"

-- | Declare or widen attribute definitions.  Safe to call repeatedly.
updateSchema :: Conn -> Schema -> IO ()
updateSchema c sch
  | M.null sch = pure ()
  | otherwise  = withWriteTxn (cEnv c) $ \t -> do
      tbl  <- loadTable c t
      maxA <- metaGet c t "max-aid"
      (changed, _, maxA') <- foldM (step t) (False, tbl, maxA) (M.toList sch)
      when changed $ do
        metaPut c t "max-aid" maxA'
        bumpSchemaVersion c t
  where
    step t (ch, tb, mx) (k, new) = case M.lookup k (atByName tb) of
      Nothing -> do
        let aid = mx + 1
            ai  = AttrInfo (fromIntegral aid) new
        put t (cAttrs c) (TE.encodeUtf8 (kwText k)) (encodeAttrVal ai)
        pure (True, insertAttr k ai tb, aid)
      Just old
        | aiSchema old == new -> pure (ch, tb, mx)
        | otherwise -> case mergeSchema k (aiSchema old) new of
            Left err     -> throwIO (SchemaError err)
            Right merged -> do
              let ai = old { aiSchema = merged }
              put t (cAttrs c) (TE.encodeUtf8 (kwText k)) (encodeAttrVal ai)
              pure (True, insertAttr k ai tb, mx)

-- | Current schema, including auto-created attributes.
getSchema :: Conn -> IO Schema
getSchema c = withDb c (pure . dbSchema)

-- ---------------------------------------------------------------------------
-- Transactions

-- | How tx-data refers to an entity.
data EntRef
  = EId     !EntityId           -- ^ an existing (or explicitly chosen) id
  | ETemp   !EntityId           -- ^ a negative temp id, resolved within this transaction
  | ELookup !Keyword !Value     -- ^ lookup ref on a unique attribute
  | ENew                        -- ^ allocate a fresh entity
  deriving (Eq, Show)

-- | What can appear in value position.
data TxVal
  = TVal    !Value
  | TTemp   !EntityId
  | TLookup !Keyword !Value
  | TNest   [(Keyword, TxVal)]  -- ^ nested entity (reference-typed attribute)
  | TMany   [TxVal]             -- ^ several values (cardinality-many attribute)
  deriving (Eq, Show)

data TxOp
  = Add          EntRef Keyword TxVal
  | Retract      EntRef Keyword Value
  | RetractEntity EntRef
  | EntityMap    EntRef [(Keyword, TxVal)]
  deriving (Eq, Show)

data TxReport = TxReport
  { txTempids :: M.Map EntityId EntityId    -- ^ temp id -> real id
  , txData    :: [(Bool, Datom)]            -- ^ (True = asserted, False = retracted)
  } deriving Show

data TxState = TxState
  { tsTable   :: !AttrTable
  , tsMaxAid  :: !Word64
  , tsMaxEid  :: !EntityId
  , tsTemps   :: !(M.Map EntityId EntityId)
  , tsOut     :: [(Bool, Datom)]
  , tsNewAttr :: !Bool
  }

data TxCtx = TxCtx { xConn :: !Conn, xTxn :: !Txn, xRef :: !(IORef TxState) }

txErr :: String -> IO a
txErr = throwIO . TxError

-- | Apply operations atomically.
transact :: Conn -> [TxOp] -> IO TxReport
transact c ops = withWriteTxn (cEnv c) $ \t -> do
  tbl  <- loadTable c t
  maxA <- metaGet c t "max-aid"
  maxE <- fromIntegral <$> metaGet c t "max-eid"
  ref  <- newIORef (TxState tbl maxA maxE M.empty [] False)
  let ctx = TxCtx c t ref
  mapM_ (applyOp ctx) ops
  st <- readIORef ref
  metaPut c t "max-eid" (fromIntegral (tsMaxEid st))
  when (tsNewAttr st) $ do
    metaPut c t "max-aid" (tsMaxAid st)
    bumpSchemaVersion c t
  pure (TxReport (tsTemps st) (reverse (tsOut st)))

modifyTx :: TxCtx -> (TxState -> TxState) -> IO ()
modifyTx ctx = modifyIORef' (xRef ctx)

record :: TxCtx -> Bool -> Datom -> IO ()
record ctx added d = modifyTx ctx (\s -> s { tsOut = (added, d) : tsOut s })

newEid :: TxCtx -> IO EntityId
newEid ctx = do
  s <- readIORef (xRef ctx)
  let e = tsMaxEid s + 1
  writeIORef (xRef ctx) s { tsMaxEid = e }
  pure e

noteEid :: TxCtx -> EntityId -> IO ()
noteEid ctx e = modifyTx ctx (\s -> if e > tsMaxEid s then s { tsMaxEid = e } else s)

ensureAttr :: TxCtx -> Keyword -> IO AttrInfo
ensureAttr ctx k = do
  s <- readIORef (xRef ctx)
  case M.lookup k (atByName (tsTable s)) of
    Just ai -> pure ai
    Nothing -> do
      let aid = tsMaxAid s + 1
          ai  = AttrInfo (fromIntegral aid) defaultAttrSchema
      put (xTxn ctx) (cAttrs (xConn ctx)) (TE.encodeUtf8 (kwText k)) (encodeAttrVal ai)
      writeIORef (xRef ctx) s { tsTable = insertAttr k ai (tsTable s), tsMaxAid = aid, tsNewAttr = True }
      pure ai

attrNameTx :: TxCtx -> Word32 -> IO Keyword
attrNameTx ctx aid = do
  s <- readIORef (xRef ctx)
  maybe (txErr ("unknown attribute id " ++ show aid)) pure (IM.lookup (fromIntegral aid) (atById (tsTable s)))

-- | Values must match the declared type; ints widen to doubles.
coerceValue :: AttrSchema -> Value -> Either String Value
coerceValue sch v = case (asType sch, v) of
  (TAny, _)              -> Right v
  (TString, VStr _)      -> Right v
  (TLong, VInt _)        -> Right v
  (TRef, VInt _)         -> Right v
  (TDouble, VDouble _)   -> Right v
  (TDouble, VInt n)      -> Right (VDouble (fromIntegral n))
  (TBool, VBool _)       -> Right v
  (TKeyword, VKw _)      -> Right v
  (t, _) -> Left ("expected " ++ T.unpack (typeName t) ++ ", got " ++ valueTypeName v)

valueFor :: TxCtx -> Keyword -> AttrInfo -> Value -> IO Value
valueFor ctx a ai v
  | asType (aiSchema ai) == TRef = case v of
      VInt n | n < 0     -> VInt <$> resolveEnt ctx (ETemp n)
             | n == 0    -> txErr ("entity id 0 is not valid for " ++ show a)
             | otherwise -> noteEid ctx n >> pure v
      _ -> txErr ("attribute " ++ show a ++ " is a ref, got " ++ valueTypeName v)
  | otherwise = either (\m -> txErr ("attribute " ++ show a ++ ": " ++ m)) pure
                       (coerceValue (aiSchema ai) v)

resolveEnt :: TxCtx -> EntRef -> IO EntityId
resolveEnt ctx er = case er of
  EId n
    | n <= 0    -> txErr ("entity id must be positive, got " ++ show n)
    | otherwise -> noteEid ctx n >> pure n
  ENew -> newEid ctx
  ETemp t -> do
    s <- readIORef (xRef ctx)
    case M.lookup t (tsTemps s) of
      Just e  -> pure e
      Nothing -> do e <- newEid ctx
                    modifyTx ctx (\s' -> s' { tsTemps = M.insert t e (tsTemps s') })
                    pure e
  ELookup a v -> do
    s <- readIORef (xRef ctx)
    case M.lookup a (atByName (tsTable s)) of
      Nothing -> txErr ("lookup ref on unknown attribute " ++ show a)
      Just ai
        | asUnique (aiSchema ai) == NotUnique ->
            txErr ("lookup ref requires a unique attribute, " ++ show a ++ " is not")
        | otherwise -> do
            v' <- either (\m -> txErr m) pure (coerceValue (aiSchema ai) v)
            r  <- uniqueOwner ctx ai v'
            maybe (txErr ("lookup ref not found: " ++ show a ++ " " ++ show v)) pure r

uniqueOwner :: TxCtx -> AttrInfo -> Value -> IO (Maybe EntityId)
uniqueOwner ctx ai v = do
  kvs <- scanPrefix (xTxn ctx) (cAve (xConn ctx)) (avePrefix (aiId ai) (Just v))
  pure $ case mapMaybe (uncurry decodeAveKey) kvs of
    ((_, _, e) : _) -> Just e
    []              -> Nothing

resolveVals :: TxCtx -> Keyword -> AttrInfo -> TxVal -> IO [Value]
resolveVals ctx a ai tv = case tv of
  TVal v       -> (: []) <$> valueFor ctx a ai v
  TTemp n      -> (: []) . VInt <$> resolveEnt ctx (ETemp n)
  TLookup la v -> (: []) . VInt <$> resolveEnt ctx (ELookup la v)
  TNest pairs
    | asType (aiSchema ai) == TRef -> (: []) . VInt <$> entityMap ctx ENew pairs
    | otherwise -> txErr ("nested entity under " ++ show a ++ " requires a ref attribute (declare it with `ref`)")
  TMany xs
    | asCard (aiSchema ai) == Many -> concat <$> mapM (resolveVals ctx a ai) xs
    | otherwise -> txErr ("attribute " ++ show a ++ " has cardinality one but several values were given")

applyOp :: TxCtx -> TxOp -> IO ()
applyOp ctx op = case op of
  Add er a tv -> do
    ai <- ensureAttr ctx a
    e  <- resolveEnt ctx er
    vs <- resolveVals ctx a ai tv
    forM_ vs (addDatom ctx e a ai)
  Retract er a v -> do
    s <- readIORef (xRef ctx)
    case M.lookup a (atByName (tsTable s)) of
      Nothing -> pure ()
      Just ai -> do
        e <- case er of
               ENew    -> txErr "cannot retract from a new entity"
               ETemp _ -> txErr "cannot retract from a temp id"
               _       -> resolveEnt ctx er
        case coerceValue (aiSchema ai) v of
          Left _   -> pure ()
          Right v' -> do
            present <- hasDatom ctx e ai v'
            when present $ retractDatom ctx e a ai v'
  RetractEntity er -> do
    e <- case er of
           ENew    -> txErr "cannot retract a new entity"
           ETemp _ -> txErr "cannot retract a temp id"
           _       -> resolveEnt ctx er
    retractEntity ctx e
  EntityMap er pairs -> void (entityMap ctx er pairs)

entityMap :: TxCtx -> EntRef -> [(Keyword, TxVal)] -> IO EntityId
entityMap ctx er pairs = do
  infos <- forM pairs $ \(a, tv) -> do ai <- ensureAttr ctx a; pure (a, ai, tv)
  -- upsert: identity attributes may point at an entity that already exists
  hits <- fmap catMaybes . forM infos $ \(a, ai, tv) ->
    case (asUnique (aiSchema ai), tv) of
      (UniqueIdentity, TVal v) -> do v' <- valueFor ctx a ai v
                                     uniqueOwner ctx ai v'
      _ -> pure Nothing
  e <- case (er, nub hits) of
    (EId n, hs) | any (/= n) hs -> txErr ("identity attributes resolve to a different entity than " ++ show n)
    (_, hs@(_ : _ : _))         -> txErr ("identity attributes resolve to different entities: " ++ show hs)
    (ETemp t, [h]) -> do
      s <- readIORef (xRef ctx)
      case M.lookup t (tsTemps s) of
        Just prev | prev /= h -> txErr ("temp id " ++ show t ++ " already refers to entity " ++ show prev
                                        ++ " but upsert resolves to " ++ show h)
        _ -> modifyTx ctx (\s' -> s' { tsTemps = M.insert t h (tsTemps s') }) >> pure h
    (ENew, [h])    -> pure h
    _              -> resolveEnt ctx er
  forM_ infos $ \(a, ai, tv) -> do
    vs <- resolveVals ctx a ai tv
    forM_ vs (addDatom ctx e a ai)
  pure e

valuesOf :: TxCtx -> EntityId -> Word32 -> IO [Value]
valuesOf ctx e aid = do
  kvs <- scanPrefix (xTxn ctx) (cEav (xConn ctx)) (eavPrefix (Just e) (Just aid) Nothing)
  pure [ v | (k, p) <- kvs, Just (_, _, v) <- [decodeEavKey k p] ]

hasDatom :: TxCtx -> EntityId -> AttrInfo -> Value -> IO Bool
hasDatom ctx e ai v = do
  let (repr, pay) = indexEntry v
  r <- get (xTxn ctx) (cEav (xConn ctx)) (eavKeyR e (aiId ai) repr)
  -- for large values the entry holds the full encoding, so this also rules out a hash collision
  pure (r == Just pay)

putRaw, delRaw :: TxCtx -> EntityId -> Word32 -> Value -> IO ()
putRaw ctx e aid v = do
  let (repr, pay) = indexEntry v
      ek = eavKeyR e aid repr
  unless (B.null pay) $ do
    -- large value: never silently overwrite a different value that hashes to the same stand-in
    prior <- get (xTxn ctx) (cEav (xConn ctx)) ek
    case prior of
      Just p | p /= pay -> txErr "hash collision between two large values (please report this)"
      _                 -> pure ()
  put (xTxn ctx) (cEav (xConn ctx)) ek pay
  put (xTxn ctx) (cAve (xConn ctx)) (aveKeyR aid repr e) pay
delRaw ctx e aid v = do
  let repr = indexRepr v
  void (del (xTxn ctx) (cEav (xConn ctx)) (eavKeyR e aid repr))
  void (del (xTxn ctx) (cAve (xConn ctx)) (aveKeyR aid repr e))

addDatom :: TxCtx -> EntityId -> Keyword -> AttrInfo -> Value -> IO ()
addDatom ctx e a ai v = do
  let sch = aiSchema ai
      aid = aiId ai
  when (asUnique sch /= NotUnique) $ do
    owner <- uniqueOwner ctx ai v
    case owner of
      Just o | o /= e -> txErr ("unique constraint violated: " ++ show a ++ " " ++ T.unpack (renderValue v)
                                ++ " already belongs to entity " ++ show o)
      _ -> pure ()
  existing <- valuesOf ctx e aid
  unless (v `elem` existing) $ do
    when (asCard sch == One) $ forM_ existing $ \old -> do
      delRaw ctx e aid old
      record ctx False (Datom e a old)
    putRaw ctx e aid v
    record ctx True (Datom e a v)

retractDatom :: TxCtx -> EntityId -> Keyword -> AttrInfo -> Value -> IO ()
retractDatom ctx e a ai v = do
  delRaw ctx e (aiId ai) v
  record ctx False (Datom e a v)

retractEntity :: TxCtx -> EntityId -> IO ()
retractEntity ctx e = do
  own <- scanPrefix (xTxn ctx) (cEav (xConn ctx)) (eavPrefix (Just e) Nothing Nothing)
  forM_ own $ \(k, p) -> case decodeEavKey k p of
    Just (_, aid, v) -> do a <- attrNameTx ctx aid
                           delRaw ctx e aid v
                           record ctx False (Datom e a v)
    Nothing -> pure ()
  -- datoms elsewhere that point at e
  s <- readIORef (xRef ctx)
  forM_ (M.toList (atByName (tsTable s))) $ \(a, ai) ->
    when (asType (aiSchema ai) == TRef) $ do
      inbound <- scanPrefix (xTxn ctx) (cAve (xConn ctx)) (avePrefix (aiId ai) (Just (VInt e)))
      forM_ inbound $ \(k, p) -> case decodeAveKey k p of
        Just (_, _, e') -> do delRaw ctx e' (aiId ai) (VInt e)
                              record ctx False (Datom e' a (VInt e))
        Nothing -> pure ()

-- ---------------------------------------------------------------------------
-- Reading

-- | A consistent snapshot.  Only valid inside 'withDb'.
data Db = Db
  { dbConn  :: !Conn
  , dbTxn   :: !Txn
  , dbTable :: !AttrTable
  }

withDb :: Conn -> (Db -> IO a) -> IO a
withDb c act = withReadTxn (cEnv c) $ \t -> do
  tbl <- readTable c t
  act (Db c t tbl)

dbSchema :: Db -> Schema
dbSchema = M.map aiSchema . atByName . dbTable

data Plan = Plan
  { plEav    :: !Bool
  , plDbi    :: !Dbi
  , plPrefix :: !B.ByteString
  }

-- | Pick an index and key prefix for a partially-known pattern.  'Nothing'
-- means the pattern can match nothing (unknown attribute, or a value that
-- cannot be of the attribute's type).  Also returns the (coerced) value.
planPattern :: Db -> Maybe EntityId -> Maybe Keyword -> Maybe Value -> Maybe (Plan, Maybe Value)
planPattern db me ma mv = do
  mai <- case ma of
           Nothing -> Just Nothing
           Just k  -> Just <$> M.lookup k (atByName (dbTable db))
  mv' <- case (mai, mv) of
           (Just ai, Just v) -> either (const Nothing) (Just . Just) (coerceValue (aiSchema ai) v)
           _                 -> Just mv
  let aid = aiId <$> mai
      c   = dbConn db
  pure $ case (me, aid) of
    (Just _, _)       -> (Plan True  (cEav c) (eavPrefix me aid mv'), mv')
    (Nothing, Just a) -> (Plan False (cAve c) (avePrefix a mv'),      mv')
    (Nothing, Nothing)-> (Plan True  (cEav c) B.empty,                mv')

decodeWith :: Db -> Plan -> B.ByteString -> B.ByteString -> Maybe Datom
decodeWith db pl k payload
  | plEav pl  = do (e, aid, v) <- decodeEavKey k payload
                   a <- IM.lookup (fromIntegral aid) (atById (dbTable db))
                   pure (Datom e a v)
  | otherwise = do (aid, v, e) <- decodeAveKey k payload
                   a <- IM.lookup (fromIntegral aid) (atById (dbTable db))
                   pure (Datom e a v)

-- | All datoms matching a pattern (each component optional).
datoms :: Db -> Maybe EntityId -> Maybe Keyword -> Maybe Value -> IO [Datom]
datoms db me ma mv = case planPattern db me ma mv of
  Nothing -> pure []
  Just (pl, mv') -> do
    kvs <- scanPrefix (dbTxn db) (plDbi pl) (plPrefix pl)
    pure [ d | (k, p) <- kvs, Just d <- [decodeWith db pl k p]
             , maybe True (== dE d) me, maybe True (== dA d) ma, maybe True (== dV d) mv' ]

-- | Upper bound on the number of matches, computed in O(log n) from the
-- counted B+tree — this is what the query planner uses.
countDatoms :: Db -> Maybe EntityId -> Maybe Keyword -> Maybe Value -> IO Word64
countDatoms db me ma mv = case planPattern db me ma mv of
  Nothing        -> pure 0
  Just (pl, _)   -> countPrefix (dbTxn db) (plDbi pl) (plPrefix pl)

-- | Estimated number of distinct values of an attribute, from a few rank-based
-- samples of the AVE index (Good–Turing style: values seen once in the sample
-- suggest many unseen ones).
distinctEstimate :: Db -> Keyword -> IO Double
distinctEstimate db a = case M.lookup a (atByName (dbTable db)) of
  Nothing -> pure 1
  Just ai -> do
    let txn = dbTxn db
        ave = cAve (dbConn db)
        pre = avePrefix (aiId ai) Nothing
    n <- countPrefix txn ave pre
    if n == 0 then pure 1
    else if asUnique (aiSchema ai) /= NotUnique then pure (fromIntegral n)
    else do
      start <- countRange txn ave Nothing (Just pre)
      let k   = min 32 n
          pos = [ start + (i * n) `div` k | i <- [0 .. k - 1] ]
      -- a sample's identity is the value part of its key (no need to decode the value)
      samples <- forM pos $ \p -> do
        r <- getRank txn ave p
        pure $ do (key, _) <- r
                  guard (B.length key >= 12)
                  Just (B.take (B.length key - 12) (B.drop 4 key))
      let vals    = catMaybes samples
          counts  = M.elems (M.fromListWith (+) [(v, 1 :: Int) | v <- vals])
          dS      = fromIntegral (length counts) :: Double
          f1      = fromIntegral (length (filter (== 1) counts)) :: Double
          kD      = fromIntegral (length vals) :: Double
          nD      = fromIntegral n :: Double
          est     = if kD == 0 then 1 else dS + (f1 / kD) * (nD - kD)
      pure (max 1 (min nD est))

-- ---------------------------------------------------------------------------
-- Range scans over one attribute (the AVE index is ordered by value)

-- | One end of a value range.
data Bound = Bound
  { boundValue     :: !Value
  , boundInclusive :: !Bool
  } deriving (Eq, Show)

data VClass = ClsNum | ClsStr | ClsKw deriving (Eq, Show)

classOf :: Value -> Maybe VClass
classOf v = case v of
  VInt _ -> Just ClsNum; VDouble d | not (isNaN d) -> Just ClsNum
  VStr _ -> Just ClsStr; VKw _ -> Just ClsKw
  _ -> Nothing                       -- booleans and NaN: not worth (or not safe) optimizing

data Target = TgInt | TgDbl | TgStr | TgKw deriving (Eq, Show)

-- | The first byte of a type's encoding (its tag).
targetTag :: Target -> B.ByteString
targetTag t = B.take 1 $ encodeValue $ case t of
  TgInt -> VInt 0; TgDbl -> VDouble 0; TgStr -> VStr ""; TgKw -> VKw (Keyword "")

endOf :: B.ByteString -> B.ByteString
endOf p = fromMaybe (B.snoc p 0xFF) (prefixSuccessor p)

-- | Key ranges @[low, high)@ in AVE that contain every value of attribute @ai@ lying inside the
-- bounds.  /Wider than necessary is fine/ — the caller keeps its predicate — but never narrower.
-- 'Nothing' means "cannot optimize" (mixed or unsupported types).
rangeSpans :: AttrInfo -> Maybe Bound -> Maybe Bound -> Maybe [(B.ByteString, B.ByteString)]
rangeSpans ai lo hi = do
  let bounds = catMaybes [lo, hi]
  when (null bounds) Nothing
  cls <- case nub (map (classOf . boundValue) bounds) of
           [Just c] -> Just c
           _        -> Nothing
  let ty = asType (aiSchema ai)
      targets = case (cls, ty) of
        (ClsNum, TLong)    -> [TgInt]
        (ClsNum, TRef)     -> [TgInt]
        (ClsNum, TDouble)  -> [TgDbl]
        (ClsNum, TAny)     -> [TgInt, TgDbl]
        (ClsStr, TString)  -> [TgStr]
        (ClsStr, TAny)     -> [TgStr]
        (ClsKw,  TKeyword) -> [TgKw]
        (ClsKw,  TAny)     -> [TgKw]
        _                  -> []        -- the predicate can never hold for this attribute's type
  Just (concatMap spansFor targets)
  where
    aid = encodeAid (aiId ai)
    spansFor tg =
      let tag    = targetTag tg
          region = B.append aid tag
          low = case lo >>= convert True tg of
                  Nothing -> region
                  Just (Bound v incl)
                    | isBigValue v -> B.append aid (B.take inlineMax (encodeValue v))   -- widen
                    | incl         -> B.append aid (encodeValue v)
                    | otherwise    -> endOf (B.append aid (encodeValue v))
          high = case hi >>= convert False tg of
                  Nothing -> endOf region
                  Just (Bound v incl)
                    | isBigValue v -> endOf (B.append aid (B.take inlineMax (encodeValue v)))
                    | incl         -> endOf (B.append aid (encodeValue v))
                    | otherwise    -> B.append aid (encodeValue v)
          -- large strings/keywords are indexed by hash, so they live in their own region
          bigRegion = [ (r, endOf r) | tg `elem` [TgStr, TgKw], let r = B.concat [aid, B.singleton bigTag, tag] ]
      in (low, high) : bigRegion
    -- Express a bound in the target type's value space (widening when it is not exact).
    convert isLower tg b@(Bound v incl) = case (tg, v) of
      (TgInt, VInt _)     -> Just b
      (TgInt, VDouble d)
        | abs d < 4.0e18  -> Just (Bound (VInt (if isLower then floor d else ceiling d)) True)
        | otherwise       -> Nothing
      (TgDbl, VInt c)     -> Just (Bound (VDouble (fromIntegral c)) incl)
      (TgDbl, VDouble _)  -> Just b
      (TgStr, VStr _)     -> Just b
      (TgKw,  VKw _)      -> Just b
      _                   -> Nothing

-- | Datoms of one attribute whose value lies within the bounds, read from the AVE index in
-- value order.  The result may include a few values just outside the bounds; filter again.
-- 'Nothing' means the range cannot be used and the caller should scan normally.
datomsInRange :: Db -> Keyword -> Maybe Bound -> Maybe Bound -> IO (Maybe [Datom])
datomsInRange db a lo hi = case M.lookup a (atByName (dbTable db)) of
  Nothing -> pure (Just [])
  Just ai -> case rangeSpans ai lo hi of
    Nothing    -> pure Nothing
    Just spans -> do
      let pl = Plan False (cAve (dbConn db)) B.empty
      chunks <- forM spans $ \(l, h) -> do
        kvs <- scanFrom (dbTxn db) (cAve (dbConn db)) l (< h)
        pure [ d | (k, p) <- kvs, Just d <- [decodeWith db pl k p] ]
      pure (Just (concat chunks))

-- | Upper bound on what 'datomsInRange' would return, in O(log n).
countInRange :: Db -> Keyword -> Maybe Bound -> Maybe Bound -> IO (Maybe Word64)
countInRange db a lo hi = case M.lookup a (atByName (dbTable db)) of
  Nothing -> pure (Just 0)
  Just ai -> case rangeSpans ai lo hi of
    Nothing    -> pure Nothing
    Just spans -> do
      ns <- forM spans $ \(l, h) ->
        if l >= h then pure 0 else countRange (dbTxn db) (cAve (dbConn db)) (Just l) (Just h)
      pure (Just (sum ns))

-- | Entity owning a unique (attribute, value), if any.
lookupRef :: Db -> Keyword -> Value -> IO (Maybe EntityId)
lookupRef db a v = do
  ds <- datoms db Nothing (Just a) (Just v)
  pure $ case ds of (d : _) -> Just (dE d); [] -> Nothing

-- | All attributes and values of one entity.
entity :: Db -> EntityId -> IO (M.Map Keyword [Value])
entity db e = do
  ds <- datoms db (Just e) Nothing Nothing
  pure (M.map reverse (M.fromListWith (++) [ (dA d, [dV d]) | d <- ds ]))   -- keeps index order, O(n)

-- | Every datom, in EAV order.
allDatoms :: Db -> IO [Datom]
allDatoms db = datoms db Nothing Nothing Nothing

-- | Number of datoms (O(log n)).
datomCount :: Db -> IO Word64
datomCount db = countAll (dbTxn db) (cEav (dbConn db))

