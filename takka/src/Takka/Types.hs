-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka.Types
-- Description : The small vocabulary shared by every layer.
module Takka.Types
  ( -- * Names and values
    Keyword(..), kw, EntityId, Value(..), renderValue, valueTypeName
  , uuidText, parseUuid
    -- * Datoms
  , Datom(..)
    -- * Schema
  , ValueType(..), Cardinality(..), Unique(..), AttrSchema(..), defaultAttrSchema, Schema
  , attr, ref, many, identity, uniqueValue, tuple, fulltext
    -- * Errors
  , TakkaError(..)
  ) where

import           Control.Exception (Exception)
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Char (digitToInt, intToDigit, isHexDigit)
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)

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
  | VTuple  ![Maybe Value]
    -- ^ What a composite tuple attribute ('tuple') holds: its parts, in order, @Nothing@ for a part the entity does
    --   not have yet.  The khana makes it; it is read and it can be an input of a query, but it is never written
    --   (a write of one is refused), and a tuple does not hold a tuple.
  | VUuid   !Word64 !Word64
    -- ^ A UUID: its 128 bits, the high half first, so that the order of the values is the order of the bytes (and, for
    --   UUIDv7, the order of the times they were made).  Written as text by 'uuidText' and read by 'parseUuid'.
  deriving (Eq, Ord, Show)

-- | The usual text of a UUID, in lower case: @01a21aec-c46b-7226-b63c-5ab791b7584a@.
uuidText :: Word64 -> Word64 -> Text
uuidText hi lo =
  let hex n w = [ intToDigit (fromIntegral ((w `shiftR` (4 * i)) .&. 15)) | i <- [n - 1, n - 2 .. 0] ]
      s = hex 16 hi ++ hex 16 lo
      (a, r1) = splitAt 8 s
      (b, r2) = splitAt 4 r1
      (c, r3) = splitAt 4 r2
      (d, e)  = splitAt 4 r3
  in T.pack (a ++ "-" ++ b ++ "-" ++ c ++ "-" ++ d ++ "-" ++ e)

-- | Read the usual text of a UUID (8-4-4-4-12 hexadecimal digits, in either case), and nothing else: no braces, no
-- @urn:uuid:@, no missing hyphens.
parseUuid :: Text -> Maybe (Word64, Word64)
parseUuid t = case T.splitOn "-" t of
  [a, b, c, d, e]
    | map T.length [a, b, c, d, e] == [8, 4, 4, 4, 12], all (T.all isHexDigit) [a, b, c, d, e]
    -> Just (word (a <> b <> c), word (d <> e))
  _ -> Nothing
  where word = T.foldl' (\n ch -> (n `shiftL` 4) .|. fromIntegral (digitToInt ch)) (0 :: Word64)

renderValue :: Value -> Text
renderValue v = case v of
  VInt n    -> T.pack (show n)
  VDouble d -> T.pack (show d)
  VStr s    -> T.pack (show (T.unpack s))
  VBool b   -> if b then "true" else "false"
  VKw k     -> T.pack (show k)
  VTuple xs -> "[" <> T.unwords (map (maybe "nil" renderValue) xs) <> "]"
  VUuid a b -> "#uuid \"" <> uuidText a b <> "\""

valueTypeName :: Value -> String
valueTypeName v = case v of
  VInt _ -> "long"; VDouble _ -> "double"; VStr _ -> "string"
  VBool _ -> "boolean"; VKw _ -> "keyword"; VTuple _ -> "tuple"; VUuid _ _ -> "uuid"

-- | One fact: entity, attribute, value.
data Datom = Datom
  { dE :: !EntityId
  , dA :: !Keyword
  , dV :: !Value
  } deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Schema (schema-on-write: attributes you never declare get 'defaultAttrSchema')

data ValueType = TAny | TString | TLong | TDouble | TBool | TKeyword | TRef | TUuid
  deriving (Eq, Show, Enum, Bounded)

data Cardinality = One | Many deriving (Eq, Show)

data Unique = NotUnique | UniqueIdentity | UniqueValue deriving (Eq, Show)

data AttrSchema = AttrSchema
  { asType   :: !ValueType
  , asCard   :: !Cardinality
  , asUnique :: !Unique
  , asTuple  :: ![Keyword]
    -- ^ Not empty: this attribute is the tuple of these (two or more) attributes of the same entity, kept by the khana
    --   and never written; see 'tuple'.
  , asFulltext :: !Bool
    -- ^ A string attribute that can be searched by its words; see 'fulltext'.
  } deriving (Eq, Show)

defaultAttrSchema :: AttrSchema
defaultAttrSchema = AttrSchema TAny One NotUnique [] False

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

-- | An attribute that is the tuple of two or more others of the same entity (a /composite tuple/ of Datalevin):
-- the khana keeps @[a b]@ up to date as @a@ and @b@ are written, so that it can be looked up, and, with 'identity',
-- so that no two entities have the same pair.  It is made, not written: a write of it is refused.
--
-- > ("follow/pair", identity (tuple ["follow/from", "follow/to"]))
--
-- A part that is not there yet is @nil@ in the tuple, and two entities with the same half-made tuple are the same
-- as far as 'identity' goes: write the parts of one entity in one batch.
tuple :: [Keyword] -> AttrSchema
tuple ks = defaultAttrSchema { asTuple = ks }

-- | A string attribute that is searched by its words: the khana keeps an index of them, and a query asks it with
-- Datalevin's @fulltext@ function,
--
-- > [:find ?e ?v :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]]]
--
-- Words are the runs of letters and digits, lower-cased; a run of Han, kana or hangul is cut into overlapping pairs
-- of characters, so that it is found by any two of its characters in a row (a search for one such character on its
-- own does not find it).  Only the resident JVM khana ('Hosigraph.KhanaDatalevinJvm') has it; with the
-- native @dtlv@ the schema is refused ('Hosigraph.SchemaRefused').  And 'Hosigraph.clean' makes the khana again from
-- the events when there is one, because the index keeps the words of a value that was erased in its file.
fulltext :: AttrSchema -> AttrSchema
fulltext s = s { asFulltext = True }

-- ---------------------------------------------------------------------------

data TakkaError
  = TxError     String
  | QueryError  String
  | SchemaError String
  | ParseError  String
  deriving Show

instance Exception TakkaError
