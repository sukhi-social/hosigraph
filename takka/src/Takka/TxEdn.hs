{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka.TxEdn
-- Description : Read Datomic/Datalevin-style tx-data written as EDN.
--
-- > [ {:db/id -1, :person/name "Frege", :person/alias ["Gottlob" "G."]}
-- > , [:db/add 42 :person/age 30]
-- > , [:db/retract 42 :person/age 30]
-- > , [:db/retractEntity [:person/name "Peirce"]] ]
--
-- Rules of thumb:
--
-- * a negative integer in an entity position is a temp id (resolved per
--   transaction); in value position it is a temp id only for @ref@ attributes;
-- * @[:attr value]@ (exactly two elements, first a keyword, second a
--   non-keyword scalar) is a lookup ref; any other vector is a set of values;
-- * a nested map in value position creates/updates a referenced entity.
module Takka.TxEdn
  ( parseTxData
  , parseTxText
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Takka.Edn
import           Takka.Store
import           Takka.Types

parseTxText :: Text -> Either String [TxOp]
parseTxText t = readEdn t >>= parseTxData

parseTxData :: Edn -> Either String [TxOp]
parseTxData e = case e of
  EVec items -> mapM item items
  EList items -> mapM item items
  _ -> Left "tx-data must be a vector"

item :: Edn -> Either String TxOp
item e = case e of
  EMap ps -> do
    (er, rest) <- splitId ps
    kvs <- mapM pair rest
    Right (EntityMap er [ kv | Just kv <- kvs ])
  EVec [EKw "db/add", en, EKw a, v] -> Add <$> entRef en <*> pure (kw a) <*> txVal v
  EVec [EKw "db/retract", en, EKw a, v] -> Retract <$> entRef en <*> pure (kw a) <*> ednToValue v
  EVec [EKw k, en] | k `elem` ["db/retractEntity", "db.fn/retractEntity"] -> RetractEntity <$> entRef en
  _ -> Left ("cannot parse tx item: " ++ T.unpack (renderEdn e))

splitId :: [(Edn, Edn)] -> Either String (EntRef, [(Edn, Edn)])
splitId ps = case [ v | (EKw "db/id", v) <- ps ] of
  []  -> Right (ENew, ps)
  [v] -> do er <- entRef v
            Right (er, [ p | p@(k, _) <- ps, k /= EKw "db/id" ])
  _   -> Left "several :db/id keys in one map"

pair :: (Edn, Edn) -> Either String (Maybe (Keyword, TxVal))
pair (k, v) = case (k, v) of
  (EKw _, ENil) -> Right Nothing   -- nil means "no value": skip
  (EKw a, _)    -> (\tv -> Just (kw a, tv)) <$> txVal v
  _ -> Left ("attribute names must be keywords, got " ++ T.unpack (renderEdn k))

entRef :: Edn -> Either String EntRef
entRef e = case e of
  EInt n | n < 0  -> Right (ETemp (fromInteger n))
         | n == 0 -> Left "entity id 0 is not valid"
         | otherwise -> Right (EId (fromInteger n))
  EVec [EKw a, v] -> ELookup (kw a) <$> ednToValue v
  _ -> Left ("cannot use as an entity reference: " ++ T.unpack (renderEdn e))

txVal :: Edn -> Either String TxVal
txVal e = case e of
  EMap ps -> do
    (er, rest) <- splitId ps
    case er of
      ENew -> do kvs <- mapM pair rest
                 Right (TNest [ kv | Just kv <- kvs ])
      _    -> Left "nested maps with :db/id are not supported; use a lookup ref or temp id"
  EVec [EKw a, v] | isPlainScalar v -> TLookup (kw a) <$> ednToValue v
  EVec xs -> TMany <$> mapM txVal xs
  ESet xs -> TMany <$> mapM txVal xs
  _ -> TVal <$> ednToValue e
  where
    isPlainScalar v = case v of EKw _ -> False; EVec _ -> False; EList _ -> False
                                EMap _ -> False; ESet _ -> False; ENil -> False; _ -> True
