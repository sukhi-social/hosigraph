{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka.JsonLD
-- Description : Import and export a practical subset of JSON-LD.
--
-- Mapping (the EAV model is already a graph, so this is mostly bookkeeping):
--
-- * a node            -> an entity
-- * @\@id@            -> attribute @:jsonld/id@ (string, unique identity => re-importing upserts)
-- * @\@type@          -> attribute @:jsonld/type@ (strings, many)
-- * a property        -> an attribute named by its /expanded IRI/ (cardinality many)
-- * a nested node or @{"\@id": …}@ or a term with @"\@type": "\@id"@ -> a @ref@
-- * @\@value@ objects -> their scalar (@\@type@/@\@language@ on literals are dropped)
--
-- Supported context features: @\@vocab@, string terms, expanded term
-- definitions with @\@id@ and @"\@type": "\@id"@, compact IRIs (@schema:name@),
-- arrays of contexts, scoped contexts on nodes, and @\@graph@.
--
-- Not supported (reported as errors, not silently ignored): remote contexts,
-- @\@list@, @\@reverse@, @\@nest@, @\@included@, framing, and the full
-- expansion/compaction algorithms.  Blank node labels (@_:b0@) are treated as
-- ordinary ids.
module Takka.JsonLD
  ( importJsonLD
  , exportJsonLD, exportByIri
  , ExportOptions(..), defaultExportOptions
  , idAttr, typeAttr
  ) where

import           Control.Exception (throwIO)
import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import           Data.Int (Int64)
import           Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.Ord (Down(..))
import           Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import           Takka.Store
import           Takka.Types

idAttr, typeAttr :: Keyword
idAttr   = Keyword "jsonld/id"
typeAttr = Keyword "jsonld/type"

-- ---------------------------------------------------------------------------
-- Context

data RawDef = RawDef { rdId :: Maybe Text, rdRef :: Bool }

data JCtx = JCtx { jcVocab :: Maybe Text, jcDefs :: M.Map Text RawDef }

emptyCtx :: JCtx
emptyCtx = JCtx Nothing M.empty

extendCtx :: JCtx -> A.Value -> Either String JCtx
extendCtx cx v = case v of
  A.Null     -> Right emptyCtx
  A.String _ -> Left "remote contexts (URLs) are not supported; inline the context object"
  A.Array xs -> foldM extendCtx cx (V.toList xs)
  A.Object o -> foldM def cx (KM.toList o)
  _          -> Left "invalid @context"
  where
    def c (k, val) =
      let key = K.toText k in
      case key of
        "@vocab" -> case val of
          A.String s -> Right c { jcVocab = Just s }
          A.Null     -> Right c { jcVocab = Nothing }
          _          -> Left "@vocab must be a string"
        _ | "@" `T.isPrefixOf` key -> Right c        -- @base, @version, @language, @protected …
          | otherwise -> case val of
              A.String s -> Right c { jcDefs = M.insert key (RawDef (Just s) False) (jcDefs c) }
              A.Object d -> Right c { jcDefs = M.insert key
                              (RawDef (case KM.lookup "@id" d of Just (A.String s) -> Just s; _ -> Nothing)
                                      (KM.lookup "@type" d == Just (A.String "@id"))) (jcDefs c) }
              A.Null     -> Right c { jcDefs = M.delete key (jcDefs c) }
              _          -> Left ("unsupported definition for term " ++ T.unpack key)

isAbsolute :: Text -> Bool
isAbsolute k = "://" `T.isInfixOf` k || "urn:" `T.isPrefixOf` k || "_:" `T.isPrefixOf` k

-- | Expand a term / compact IRI.  @useVocab@: fall back to @\@vocab@ for bare terms.
expandTerm :: JCtx -> Bool -> Text -> Text
expandTerm cx useVocab = go (8 :: Int)
  where
    vocab = if useVocab then jcVocab cx else Nothing
    go 0 k = k
    go n k = case M.lookup k (jcDefs cx) of
      Just d -> case rdId d of
        Just i | i /= k -> go (n - 1) i
        _               -> maybe k (<> k) vocab
      Nothing ->
        let (p, rest) = T.breakOn ":" k
        in case (T.null rest, M.lookup p (jcDefs cx)) of
             (False, Just d) | not ("//" `T.isPrefixOf` T.drop 1 rest), Just pi' <- rdId d
                -> go (n - 1) pi' <> T.drop 1 rest
             _ | isAbsolute k -> k
               | otherwise    -> maybe k (<> k) vocab

-- ---------------------------------------------------------------------------
-- Import

data Kind = KRef | KScalar deriving (Eq, Show)

type Needs = M.Map Keyword Kind

mergeNeeds :: Needs -> Needs -> Either String Needs
mergeNeeds a b = foldM ins a (M.toList b)
  where ins acc (k, kd) = case M.lookup k acc of
          Just kd' | kd' /= kd -> Left ("property " ++ show k ++ " is used both as a node reference and as a literal; "
                                        ++ "this importer needs one or the other per property")
          _ -> Right (M.insert k kd acc)

-- | Import a JSON-LD document (object, array of nodes, or @{"\@graph": […]}@).
-- Attribute definitions are derived from the data and added to the schema.
importJsonLD :: Conn -> A.Value -> IO TxReport
importJsonLD conn doc = do
  (ops, needs) <- either (throwIO . ParseError) pure (docToOps doc)
  let fixed = M.fromList
        [ (idAttr,   identity (attr TString))
        , (typeAttr, many (attr TString)) ]
      derived = M.mapWithKey (\_ kd -> case kd of KRef -> many ref; KScalar -> many (attr TAny)) needs
  updateSchema conn (M.union fixed derived)
  transact conn ops

docToOps :: A.Value -> Either String ([TxOp], Needs)
docToOps doc = case doc of
  A.Array xs  -> nodes emptyCtx (V.toList xs)
  A.Object o  -> do
    cx <- maybe (Right emptyCtx) (extendCtx emptyCtx) (KM.lookup "@context" o)
    case KM.lookup "@graph" o of
      Just (A.Array xs) -> nodes cx (V.toList xs)
      Just _            -> Left "@graph must be an array"
      Nothing           -> nodes cx [doc]
  _ -> Left "a JSON-LD document must be an object or an array of objects"
  where
    nodes cx vs = do
      rs <- forM vs $ \v -> case v of
              A.Object o -> do (ps, nd) <- nodePairs cx o
                               pure (EntityMap ENew ps, nd)
              _ -> Left "top-level nodes must be objects"
      nd <- foldM mergeNeeds M.empty (map snd rs)
      pure (map fst rs, nd)

nodePairs :: JCtx -> A.Object -> Either String ([(Keyword, TxVal)], Needs)
nodePairs cx0 o = do
  cx <- maybe (Right cx0) (extendCtx cx0) (KM.lookup "@context" o)
  foldM (\(ps, nd) (k, v) -> do
            (ps', nd') <- prop cx (K.toText k) v
            nd'' <- mergeNeeds nd nd'
            pure (ps ++ ps', nd'')) ([], M.empty) (KM.toList o)

prop :: JCtx -> Text -> A.Value -> Either String ([(Keyword, TxVal)], Needs)
prop cx key v = case key of
  "@context" -> Right ([], M.empty)
  "@id" -> case v of
    A.String s -> Right ([(idAttr, TVal (VStr (expandTerm cx False s)))], M.empty)
    _          -> Left "@id must be a string"
  "@type" -> do
    ts <- case v of
      A.String s -> Right [s]
      A.Array xs -> forM (V.toList xs) $ \x -> case x of
                      A.String s -> Right s
                      _          -> Left "@type values must be strings"
      _ -> Left "@type must be a string or an array of strings"
    Right ([(typeAttr, TMany [ TVal (VStr (expandTerm cx True t)) | t <- ts ])], M.empty)
  _ | "@" `T.isPrefixOf` key -> Left ("unsupported JSON-LD keyword: " ++ T.unpack key)
    | otherwise -> do
        let a     = Keyword (expandTerm cx True key)
            isRef = maybe False rdRef (M.lookup key (jcDefs cx))
        (vals, nd) <- values cx isRef v
        case nub (map snd vals) of
          []  -> Right ([], nd)
          [kd] -> do nd' <- mergeNeeds nd (M.singleton a kd)
                     Right ([(a, TMany (map fst vals))], nd')
          _   -> Left ("property " ++ T.unpack key ++ " mixes node references and literals")

refTo :: Text -> TxVal
refTo iri = TNest [(idAttr, TVal (VStr iri))]

scalarOf :: A.Value -> Maybe Value
scalarOf v = case v of
  A.String s -> Just (VStr s)
  A.Bool b   -> Just (VBool b)
  A.Number n -> Just (numValue n)
  _          -> Nothing

numValue :: Scientific -> Value
numValue n = case floatingOrInteger n :: Either Double Integer of
  Right i | i >= toInteger (minBound :: Int64) && i <= toInteger (maxBound :: Int64) -> VInt (fromInteger i)
          | otherwise -> VDouble (fromInteger i)
  Left d -> VDouble d

values :: JCtx -> Bool -> A.Value -> Either String ([(TxVal, Kind)], Needs)
values cx isRef v = case v of
  A.Null     -> Right ([], M.empty)
  A.Array xs -> do
    rs <- mapM (values cx isRef) (V.toList xs)
    nd <- foldM mergeNeeds M.empty (map snd rs)
    Right (concatMap fst rs, nd)
  A.String s
    | isRef     -> Right ([(refTo (expandTerm cx False s), KRef)], M.empty)
    | otherwise -> Right ([(TVal (VStr s), KScalar)], M.empty)
  A.Object o
    | Just inner <- KM.lookup "@value" o ->
        case scalarOf inner of
          Just sv -> Right ([(TVal sv, KScalar)], M.empty)
          Nothing -> Left "@value must be a string, number or boolean"
    | KM.member "@list" o     -> Left "@list is not supported (plain arrays are treated as unordered sets)"
    | KM.member "@reverse" o  -> Left "@reverse is not supported"
    | Just inner <- KM.lookup "@set" o -> values cx isRef inner
    | [("@id", A.String s)] <- [ (K.toText k, x) | (k, x) <- KM.toList o ] ->
        Right ([(refTo (expandTerm cx False s), KRef)], M.empty)
    | otherwise -> do
        (ps, nd) <- nodePairs cx o
        Right ([(TNest ps, KRef)], nd)
  other -> case scalarOf other of
    Just sv -> Right ([(TVal sv, KScalar)], M.empty)
    Nothing -> Left "unsupported JSON value"

-- ---------------------------------------------------------------------------
-- Export

data ExportOptions = ExportOptions
  { eoContext :: M.Map Text Text   -- ^ term/prefix -> IRI, used to compact names and emitted as @\@context@
  , eoDepth   :: Int               -- ^ how many levels of referenced nodes to embed
  }

defaultExportOptions :: ExportOptions
defaultExportOptions = ExportOptions M.empty 2

-- | Look nodes up by @\@id@ and export them.
exportByIri :: Db -> ExportOptions -> [Text] -> IO A.Value
exportByIri db opts iris = do
  es <- fmap (mapMaybe id) . forM iris $ \i -> lookupRef db idAttr (VStr i)
  exportJsonLD db opts es

exportJsonLD :: Db -> ExportOptions -> [EntityId] -> IO A.Value
exportJsonLD db opts eids = do
  ns <- mapM (nodeJson (eoDepth opts) S.empty) eids
  let ctxObj = A.Object (KM.fromList [ (K.fromText t, A.String i) | (t, i) <- M.toList (eoContext opts) ])
  pure $ case (M.null (eoContext opts), ns) of
    (True,  [n])         -> n
    (True,  _)           -> A.toJSON ns
    (False, [A.Object o]) -> A.Object (KM.insert "@context" ctxObj o)
    (False, _)           -> A.object ["@context" A..= ctxObj, "@graph" A..= ns]
  where
    schema = dbSchema db
    isRefAttr a = fmap asType (M.lookup a schema) == Just TRef

    -- longest matching prefix wins; exact term match beats prefixes
    compact :: Text -> Text
    compact iri =
      case [ t | (t, i) <- M.toList (eoContext opts), i == iri ] of
        (t : _) -> t
        [] -> case sortOn (Down . T.length . snd)
                     [ (t, i) | (t, i) <- M.toList (eoContext opts), not (T.null i)
                              , i `T.isPrefixOf` iri, T.last i `elem` ("/#:" :: String) ] of
                ((t, i) : _) -> t <> ":" <> T.drop (T.length i) iri
                []           -> iri

    nodeJson :: Int -> S.Set EntityId -> EntityId -> IO A.Value
    nodeJson depth seen e = do
      attrs <- entity db e
      let seen' = S.insert e seen
          idPair = case M.lookup idAttr attrs of
                     Just (VStr s : _) -> [("@id", A.String s)]
                     _                 -> []
          typePair = case M.lookup typeAttr attrs of
                       Just [VStr t] -> [("@type", A.String (compact t))]
                       Just ts@(_ : _) -> [("@type", A.toJSON [ compact t | VStr t <- ts ])]
                       _ -> []
      props <- forM [ (a, vs) | (a, vs) <- M.toList attrs, a /= idAttr, a /= typeAttr ] $ \(a, vs) -> do
        js <- mapM (valueJson depth seen' (isRefAttr a)) vs
        pure (K.fromText (compact (kwText a)), case js of [j] -> j; _ -> A.toJSON js)
      pure (A.Object (KM.fromList (idPair ++ typePair ++ props)))

    valueJson :: Int -> S.Set EntityId -> Bool -> Value -> IO A.Value
    valueJson depth seen isRef v = case v of
      VInt n | isRef -> if depth > 0 && not (S.member n seen)
                          then nodeJson (depth - 1) seen n
                          else do
                            attrs <- entity db n
                            pure $ A.object ["@id" A..= case M.lookup idAttr attrs of
                                                          Just (VStr s : _) -> s
                                                          _ -> "_:e" <> T.pack (show n)]
      VInt n    -> pure (A.toJSON n)
      VDouble d -> pure (A.toJSON d)
      VStr s    -> pure (A.String s)
      VBool b   -> pure (A.Bool b)
      VKw k     -> pure (A.String (T.pack (show k)))

