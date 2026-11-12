-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka.Types
-- Description : The small vocabulary shared by every layer.
module Takka.Types
  ( -- * Names and values
    Keyword(..), kw, EntityId, Value(..), renderValue, valueTypeName
    -- * Datoms
  , Datom(..)
    -- * Schema
  , ValueType(..), Cardinality(..), Unique(..), AttrSchema(..), defaultAttrSchema, Schema
  , attr, ref, many, identity, uniqueValue
    -- * Errors
  , TakkaError(..)
  ) where

import           Control.Exception (Exception)
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T

-- | An EDN-style keyword such as @:person/name@.  The leading colon is not stored.
-- Any text is allowed, so full IRIs can be attribute names (handy for JSON-LD).
newtype Keyword = Keyword { kwText :: Text } deriving (Eq, Ord)

instance Show Keyword where show (Keyword t) = ':' : T.unpack t

-- | @"person/name"@ and @":person/name"@ both work with OverloadedStrings.
instance IsString Keyword where fromString = kw . T.pack

kw :: Text -> Keyword
kw t = Keyword (maybe t id (T.stripPrefix ":" t))

-- | Entity ids are plain 64-bit integers (as in Datomic/Datalevin).
type EntityId = Int64

-- | The values a datom can hold.  Entity ids are 'VInt' — a reference is just an
-- integer, which is what lets @[?a :friend ?b] [?b :name ?n]@ join naturally.
data Value
  = VInt    !Int64
  | VDouble !Double
  | VStr    !Text
  | VBool   !Bool
  | VKw     !Keyword
  deriving (Eq, Ord, Show)

renderValue :: Value -> Text
renderValue v = case v of
  VInt n    -> T.pack (show n)
  VDouble d -> T.pack (show d)
  VStr s    -> T.pack (show (T.unpack s))
  VBool b   -> if b then "true" else "false"
  VKw k     -> T.pack (show k)

valueTypeName :: Value -> String
valueTypeName v = case v of
  VInt _ -> "long"; VDouble _ -> "double"; VStr _ -> "string"
  VBool _ -> "boolean"; VKw _ -> "keyword"

-- | One fact: entity, attribute, value.
data Datom = Datom
  { dE :: !EntityId
  , dA :: !Keyword
  , dV :: !Value
  } deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Schema (schema-on-write: attributes you never declare get 'defaultAttrSchema')

data ValueType = TAny | TString | TLong | TDouble | TBool | TKeyword | TRef
  deriving (Eq, Show, Enum, Bounded)

data Cardinality = One | Many deriving (Eq, Show)

data Unique = NotUnique | UniqueIdentity | UniqueValue deriving (Eq, Show)

data AttrSchema = AttrSchema
  { asType   :: !ValueType
  , asCard   :: !Cardinality
  , asUnique :: !Unique
  } deriving (Eq, Show)

defaultAttrSchema :: AttrSchema
defaultAttrSchema = AttrSchema TAny One NotUnique

type Schema = Map Keyword AttrSchema

-- Tiny combinators so schemas read nicely:
--
-- > schema = Map.fromList
-- >   [ ("name",   identity (attr TString))
-- >   , ("friend", many ref) ]
attr :: ValueType -> AttrSchema
attr t = defaultAttrSchema { asType = t }

ref :: AttrSchema
ref = attr TRef

many :: AttrSchema -> AttrSchema
many s = s { asCard = Many }

identity :: AttrSchema -> AttrSchema
identity s = s { asUnique = UniqueIdentity }

uniqueValue :: AttrSchema -> AttrSchema
uniqueValue s = s { asUnique = UniqueValue }

-- ---------------------------------------------------------------------------

data TakkaError
  = TxError     String
  | QueryError  String
  | SchemaError String
  | ParseError  String
  deriving Show

instance Exception TakkaError
