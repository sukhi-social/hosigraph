{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka.Pull
-- Description : Pull entities as trees, driven by a pattern.
--
-- > [:person/name                         ; an attribute
-- >  {:person/friend [:person/name]}      ; follow a reference, with its own pattern
-- >  {:person/friend 2}                   ; ... recursively, to a depth
-- >  {:person/friend ...}                 ; ... recursively, without limit (cycles are cut)
-- >  :person/_friend                      ; reverse reference: who points at me
-- >  (:person/age :default 0)             ; default when missing
-- >  (:person/name :as "n")               ; rename in the result
-- >  (limit :person/alias 2)              ; at most two values
-- >  :db/id                               ; the entity id
-- >  *]                                   ; every attribute (explicit items override it)
--
-- Reference-valued attributes without a nested pattern come back as @{:db/id n}@.
-- Many-valued attributes (and reverse references) come back as lists, in index order.
module Takka.Pull
  ( Pulled(..), PullItem(..), PullAttr(..), Recur(..), Pattern
  , parsePull, parsePullText
  , pull, pullMany, pullLookup
  , renderPulled, pulledToJSON
  ) where

import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import           Takka.Edn
import           Takka.Store
import           Takka.Types

-- | The result: an ordered tree.
data Pulled
  = PScalar Value
  | PMany   [Pulled]
  | PEntity [(Keyword, Pulled)]
  deriving (Eq, Show)

data Recur = RNone | RDepth Int | RUnlimited deriving (Eq, Show)

data PullItem = PWild | PAttr PullAttr deriving (Eq, Show)

data PullAttr = PullAttr
  { paName    :: Keyword          -- ^ as written (the key in the result, unless renamed)
  , paKey     :: Keyword          -- ^ the attribute it reads
  , paReverse :: Bool
  , paAs      :: Maybe Text
  , paDefault :: Maybe Value
  , paLimit   :: Maybe Int
  , paSub     :: Maybe Pattern
  , paRecur   :: Recur
  } deriving (Eq, Show)

type Pattern = [PullItem]

-- ---------------------------------------------------------------------------
-- Parsing

parsePullText :: Text -> Either String Pattern
parsePullText t = readEdn t >>= parsePull

parsePull :: Edn -> Either String Pattern
parsePull e = case e of
  EVec items -> concat <$> mapM item items
  _          -> Left "a pull pattern must be a vector"

plain :: Keyword -> PullAttr
plain k = PullAttr k k False Nothing Nothing Nothing Nothing RNone

-- | @:ns/_name@ reads the reverse of @:ns/name@.
attrOf :: Text -> PullAttr
attrOf txt =
  let (ns, name) = T.breakOnEnd "/" txt
  in if "_" `T.isPrefixOf` name && T.length name > 1
       then (plain (Keyword txt)) { paKey = Keyword (ns <> T.drop 1 name), paReverse = True }
       else plain (Keyword txt)

item :: Edn -> Either String [PullItem]
item e = case e of
  ESym "*"  -> Right [PWild]
  EKw k     -> Right [PAttr (attrOf k)]
  EList xs  -> (\a -> [PAttr a]) <$> expr xs
  EMap ps   -> forM ps $ \(k, v) -> fmap PAttr $ do
    base <- case k of
      EKw n    -> Right (attrOf n)
      EList xs -> expr xs
      _        -> Left ("pull: a map key must be an attribute, got " ++ T.unpack (renderEdn k))
    case v of
      EVec _     -> (\sub -> base { paSub = Just sub }) <$> parsePull v
      EInt n | n >= 0 -> Right base { paRecur = if n == 0 then RNone else RDepth (fromInteger n) }
      ESym "..." -> Right base { paRecur = RUnlimited }
      _          -> Left ("pull: expected a nested pattern, a depth or ... after " ++ T.unpack (renderEdn k))
  _ -> Left ("cannot parse pull item: " ++ T.unpack (renderEdn e))

-- | @(limit :attr n)@ or @(:attr :as "x" :default v :limit n)@.
expr :: [Edn] -> Either String PullAttr
expr xs = case xs of
  [ESym "limit", EKw k, EInt n] -> Right (attrOf k) { paLimit = Just (fromInteger n) }
  [ESym "limit", EKw k, ENil]   -> Right (attrOf k)
  (EKw k : opts)                -> options (attrOf k) opts
  _ -> Left ("cannot parse pull expression: " ++ T.unpack (renderEdn (EList xs)))
  where
    options it [] = Right it
    options it (EKw "as" : v : r) = case v of
      EStr s -> options it { paAs = Just s } r
      EKw s  -> options it { paAs = Just s } r
      ESym s -> options it { paAs = Just s } r
      _      -> Left ":as expects a name"
    options it (EKw "default" : v : r) = do
      d <- ednToValue v
      options it { paDefault = Just d } r
    options it (EKw "limit" : EInt n : r) = options it { paLimit = Just (fromInteger n) } r
    options _ (o : _) = Left ("unknown pull option: " ++ T.unpack (renderEdn o))

-- ---------------------------------------------------------------------------
-- Running

dbId :: Keyword
dbId = Keyword "db/id"

pull :: Db -> Pattern -> EntityId -> IO Pulled
pull db pat = pullE db pat S.empty

pullMany :: Db -> Pattern -> [EntityId] -> IO [Pulled]
pullMany db pat = mapM (pull db pat)

-- | Pull the entity that owns a unique (attribute, value), if there is one.
pullLookup :: Db -> Pattern -> Keyword -> Value -> IO (Maybe Pulled)
pullLookup db pat a v = lookupRef db a v >>= traverse (pull db pat)

pullE :: Db -> Pattern -> S.Set EntityId -> EntityId -> IO Pulled
pullE db pat seen e = do
  attrs <- entity db e
  let seen'    = S.insert e seen
      sch      = dbSchema db
      explicit = S.fromList [ paKey a | PAttr a <- pat, not (paReverse a) ]
      explicitId = dbId `S.member` explicit
      isRef k  = fmap asType (M.lookup k sch) == Just TRef
      isMany k = fmap asCard (M.lookup k sch) == Just Many

      -- a referenced entity: expand it, or just give its id
      child it n = case (paSub it, paRecur it) of
        (Just sub, _)
          | n `S.member` seen' -> bare n
          | otherwise          -> pullE db sub seen' n
        (Nothing, RUnlimited)
          | n `S.member` seen' -> bare n
          | otherwise          -> pullE db pat seen' n
        (Nothing, RDepth d)
          | n `S.member` seen' -> bare n
          | otherwise          -> pullE db (map (descend it d) pat) seen' n
        (Nothing, RNone)       -> bare n
      bare n = pure (PEntity [(dbId, PScalar (VInt n))])
      -- one level deeper: this item's depth counts down (at 0 it stops following)
      descend it d x = case x of
        PAttr a | paName a == paName it && paReverse a == paReverse it ->
                    PAttr a { paRecur = if d <= 1 then RNone else RDepth (d - 1) }
        _ -> x

      outKey it = maybe (paName it) Keyword (paAs it)

      forward it = case M.lookup (paKey it) attrs of
        Nothing -> pure [ (outKey it, PScalar d) | Just d <- [paDefault it] ]
        Just vs -> do
          let vs' = case paLimit it of Just n | isMany (paKey it) -> take n vs; _ -> vs
          out <- forM vs' $ \v -> case v of
                   VInt n | isRef (paKey it) -> child it n
                   _                         -> pure (PScalar v)
          pure [ (outKey it, if isMany (paKey it) then PMany out else head out) | not (null out) ]

      backward it = do
        ds <- datoms db Nothing (Just (paKey it)) (Just (VInt e))
        let srcs = maybe id take (paLimit it) (map dE ds)
        out <- mapM (child it) srcs
        pure [ (outKey it, PMany out) | not (null out) ]

  parts <- forM pat $ \it -> case it of
    PWild -> do
      -- :db/id, then every attribute that no explicit item mentions, read like any other
      rest <- fmap concat . forM [ k | k <- M.keys attrs, not (k `S.member` explicit) ] $ \k -> forward (plain k)
      pure ([ (dbId, PScalar (VInt e)) | not explicitId ] ++ rest)
    PAttr a
      | paKey a == dbId, not (paReverse a) -> pure [(outKey a, PScalar (VInt e))]
      | paReverse a -> backward a
      | otherwise   -> forward a
  pure (PEntity (concat parts))

-- ---------------------------------------------------------------------------
-- Rendering

renderPulled :: Pulled -> Text
renderPulled p = case p of
  PScalar v  -> renderValue v
  PMany xs   -> "[" <> T.unwords (map renderPulled xs) <> "]"
  PEntity kv -> "{" <> T.intercalate ", " [ T.pack (show k) <> " " <> renderPulled v | (k, v) <- kv ] <> "}"

pulledToJSON :: Pulled -> A.Value
pulledToJSON p = case p of
  PScalar v -> case v of
    VInt n    -> A.toJSON n
    VDouble d -> A.toJSON d
    VStr s    -> A.String s
    VBool b   -> A.Bool b
    VKw k     -> A.String (T.pack (show k))
  PMany xs  -> A.toJSON (map pulledToJSON xs)
  PEntity kv -> A.Object (KM.fromList [ (K.fromText (kwText k), pulledToJSON v) | (k, v) <- kv ])
