-- |
-- Module      : Hosigraph.Event
-- Description : The vocabulary of hosigraph: ids, events (Create / Delete), and their bytes.
module Hosigraph.Event
  ( HosiId, Kind(..), kindName, kindFromName
  , Event(..), Ref(..), Val(..), Op(..)
  , putValue, getValue, putEvent, getEvent
  , HosiError(..)
  ) where

import           Control.Exception (Exception)
import           Data.Int (Int64)
import           Data.Word
import           GHC.Float (castDoubleToWord64, castWord64ToDouble)

import           Hosigraph.Bin
import           Native.Types (Keyword(..), Value(..))

-- | A time-sortable internal id (see "Hosigraph.Id").  Same width as the khana's 'Native.Types.EntityId',
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

data HosiError
  = WriteRejected String      -- ^ the batch was refused; nothing was kept
  | Corrupt String            -- ^ a file does not match what its hash / format promises
  | BadArgument String
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

getValue :: Get Value
getValue = do
  tag <- getU8
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
