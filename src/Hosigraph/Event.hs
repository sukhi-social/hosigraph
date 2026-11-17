-- SPDX-License-Identifier: BlueOak-1.0.0
-- |
-- Module      : Hosigraph.Event
-- Description : The vocabulary of hosigraph: ids, events (Create / Delete), and their bytes.
module Hosigraph.Event
  ( HosiId, Kind(..), kindName, kindFromName
  , Event(..), Ref(..), Val(..), Op(..), Cond(..)
  , putValue, getValue, getValueBody, putEvent, getEvent
  , HosiError(..)
  ) where

import           Control.Exception (Exception)
import           Data.Int (Int64)
import           Data.Word
import           GHC.Float (castDoubleToWord64, castWord64ToDouble)

import           Hosigraph.Bin
import           Takka.Types (Keyword(..), Value(..))

-- | A time-sortable internal id (see "Hosigraph.Id").  Same width as the khana's 'Takka.Types.EntityId',
-- so a hosigraph id /is/ the entity id inside the khana.
type HosiId = Int64

-- | The two kinds of event.  There is no Update: a newer Create of a cardinality-one
-- attribute wins (the design's first proposal, still open).
data Kind = KCreate | KDelete deriving (Eq, Ord, Show, Enum, Bounded)

kindName :: Kind -> String
kindName KCreate = "create"
kindName KDelete = "delete"

kindFromName :: String -> Maybe Kind
kindFromName "create" = Just KCreate
kindFromName "delete" = Just KDelete
kindFromName _        = Nothing

-- | One frozen fact about the world.  @evSeq@ is the order in which it happened;
-- the design's tail log has this number, and we keep it inside chunks too, because
-- folding a Create and a later Delete (or a newer Create) needs to know which came first.
data Event = Event
  { evSeq  :: !Word64
  , evKind :: !Kind
  , evId   :: !HosiId
  , evAttr :: !Keyword
  , evVal  :: !Value
  } deriving (Eq, Show)

-- | How a caller names an entity: an existing id, or a placeholder (@Tmp 1@) that
-- hosigraph replaces with a fresh id when the batch is written.
data Ref = Id !HosiId | Tmp !Int deriving (Eq, Show)

-- | A value: a literal, or a reference to another entity.
data Val = Lit !Value | RefTo !Ref deriving (Eq, Show)

-- | What a caller asks for.  Each becomes one 'Event'.
data Op
  = Create Ref Keyword Val
  | Delete Ref Keyword Val
  deriving (Eq, Show)

-- | A fact about "now" that a write can ask to still be true when it goes in ('Hosigraph.writeIf').
-- There is no @Tmp@ here: a fresh entity has no state to ask about.
data Cond
  = Holds !HosiId Keyword Value   -- ^ this entity has this value for this attribute
  | Lacks !HosiId Keyword Value   -- ^ ...does not have this value
  | Unset !HosiId Keyword         -- ^ ...has no value at all for this attribute
  deriving (Eq, Show)

-- | What went wrong, in the kinds a caller would answer differently.  The 'String' is for a person;
-- match on the constructor, not on the text.
data HosiError
  = WriteRejected String      -- ^ the batch was refused; nothing was kept
  | Corrupt String            -- ^ a file does not match what its hash / format promises
  | BadArgument String        -- ^ the call itself is wrong (an empty batch, an id that is not an entity, a range that ends before it starts): change the call
  | WorldBusy String          -- ^ the world is open already, in this process or in another: close the other one, or try again later.  Nothing was touched
  | BackendMissing String     -- ^ the program the khana needs (@dtlv@, or @java@ and the Datalevin jar) is not where the config says: install it, or fix the path
  | SchemaRefused String      -- ^ the khana cannot be made under this schema, and nothing was changed: open again with the schema the world was made with
  deriving Show

instance Exception HosiError

-- ---------------------------------------------------------------------------
-- Values

putValue :: Value -> Builder'
putValue v = case v of
  VInt n          -> putU8 0 <> putU64 (fromIntegral n)
  VDouble d       -> putU8 1 <> putU64 (castDoubleToWord64 d)
  VStr t          -> putU8 2 <> putText t
  VBool b         -> putU8 3 <> putU8 (if b then 1 else 0)
  VKw (Keyword t) -> putU8 4 <> putText t
  VTuple _        -> error "putValue: a tuple is made by the khana and never written (Hosigraph.Events.validate refuses it)"

getValue :: Get Value
getValue = getU8 >>= getValueBody

-- | The rest of a value, once its tag byte has been read.  (A chunk adds one tag of its own,
-- a reference into its dictionary; it reads the tag itself and hands the others to this.)
getValueBody :: Word8 -> Get Value
getValueBody tag =
  case tag of
    0 -> VInt . fromIntegral <$> getU64
    1 -> VDouble . castWord64ToDouble <$> getU64
    2 -> VStr <$> getText
    3 -> VBool . (/= 0) <$> getU8
    4 -> VKw . Keyword <$> getText
    _ -> failGet ("unknown value tag " ++ show tag)

-- ---------------------------------------------------------------------------
-- Whole events (the tail log stores them this way; chunks use a dictionary instead)

putEvent :: Event -> Builder'
putEvent (Event s k i (Keyword a) v) =
  putU64 s <> putU8 (fromIntegral (fromEnum k)) <> putU64 (fromIntegral i) <> putText a <> putValue v

getEvent :: Get Event
getEvent = do
  s <- getU64
  k <- getU8
  i <- getU64
  a <- getText
  v <- getValue
  kind <- if k <= 1 then pure (toEnum (fromIntegral k)) else failGet "unknown event kind"
  pure (Event s kind (fromIntegral i) (Keyword a) v)
