-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module      : Hosigraph.Abi.Codec
-- Description : Between EDN and hosigraph's own types.  Pure; the shapes are those of "Hosigraph.Abi.Api".
--
-- The types are hosigraph's and takka's, and they do not know about EDN: the instances of
-- 'ToEdn' and 'FromEdn' are written here (which is why the module is full of orphans).  Where a
-- type is a record or a sum they come from "Setu.Codec" in one line; what is read by the shape of
-- the EDN (a value, a reference) is written by hand.
--
-- The decoders run after the request has been checked against its 'Setu.Spec.Ty', so what they
-- refuse is a disagreement between this module and the 'Setu.Spec.Ty' (a mistake of ours, which
-- the dispatcher reports as the library's, not the caller's).  The encoders are checked in turn
-- by the dispatcher against the result's 'Setu.Spec.Ty'.
module Hosigraph.Abi.Codec
  ( -- * The arguments of each operation
    decWrite, decBatches, decIds, decId, decQueries
  , decErasures, decCleanOptions, decFreezeOptions
    -- * The answers that are not one instance
  , encWriteResult, encTrace, errorKind, conflictMessage
  , encEraseReport, encCleanReport, encFreezeReport
  ) where

import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           GHC.Generics (Generic)

import           Hosigraph
import           Hosigraph.Event (kindName)
import           Hosigraph.Manifest (ChunkEntry(..), FileEntry(..))
import           Setu.Codec
import           Takka.Edn (Edn(..), renderEdn)
import           Takka.Syntax (Pulled(..))

deriving instance Generic Op
deriving instance Generic Event
deriving instance Generic Stats
deriving instance Generic WriteReport
deriving instance Generic SealReport
deriving instance Generic LookupTrace
deriving instance Generic ChunkEntry
deriving instance Generic FileEntry

-- ---------------------------------------------------------------------------
-- In

-- | @[:holds id attr value]@, @[:lacks id attr value]@ or @[:unset id attr]@.
instance FromEdn Cond where
  fromEdn e = case e of
    EVec [EKw "holds", i, a, v] -> Holds <$> fromEdn i <*> fromEdn a <*> fromEdn v
    EVec [EKw "lacks", i, a, v] -> Lacks <$> fromEdn i <*> fromEdn a <*> fromEdn v
    EVec [EKw "unset", i, a]    -> Unset <$> fromEdn i <*> fromEdn a
    _                           -> Left "not a condition"

instance ToEdn Cond where
  toEdn c = case c of
    Holds i a v -> EVec [EKw "holds", toEdn i, toEdn a, toEdn v]
    Lacks i a v -> EVec [EKw "lacks", toEdn i, toEdn a, toEdn v]
    Unset i a   -> EVec [EKw "unset", toEdn i, toEdn a]

-- | By its shape: an integer, a float, a string, a boolean or a keyword.
instance FromEdn Value where
  fromEdn e = case e of
    EInt _   -> VInt <$> fromEdn e
    EFloat d -> Right (VDouble d)
    EStr t   -> Right (VStr t)
    EBool b  -> Right (VBool b)
    EKw k    -> Right (VKw (Keyword k))
    _        -> Left "not a value"

-- | An integer is an existing entity; @[:tmp n]@ is a placeholder.
instance FromEdn Ref where
  fromEdn e = case e of
    EInt _              -> Id <$> fromEdn e
    EVec [EKw "tmp", n] -> Tmp <$> fromEdn n
    _                   -> Left "not a ref"

-- | A value is a literal; @[:ref r]@ is a reference.
instance FromEdn Val where
  fromEdn e = case e of
    EVec [EKw "ref", r] -> RefTo <$> fromEdn r
    _                   -> Lit <$> fromEdn e

-- | @[:create ref attr val]@ or @[:delete ref attr val]@.
instance FromEdn Op where
  fromEdn = genericFromEdn defaultStyle

-- | By hand, not by Generic: takka's 'Input' has a constructor (@InRules@) that is not offered.
instance FromEdn Input where
  fromEdn e = case e of
    EVec [EKw "scalar", v] -> InScalar <$> fromEdn v
    EVec [EKw "coll", vs]  -> InColl <$> fromEdn vs
    EVec [EKw "rel", rows] -> InRel <$> fromEdn rows
    _                      -> Left "not an input"

-- | The value under a key of the arguments (a missing key is nil).
field :: Text -> Edn -> Edn
field k e = case e of
  EMap ps -> maybe ENil id (lookup (EKw k) ps)
  _       -> ENil

-- | The batch, and the conditions it waits for (@:when@ may be left out, or nil: none).
decWrite :: Edn -> Either Text ([Cond], [Op])
decWrite e = do
  ops <- fromEdn (field "ops" e)
  conds <- case field "when" e of
    ENil -> Right []
    c    -> fromEdn c
  Right (conds, ops)

-- | What a @:conflict@ says: the conditions that did not hold, as EDN text.
conflictMessage :: [Cond] -> Text
conflictMessage = renderEdn . toEdn

decBatches :: Edn -> Either Text [[Op]]
decBatches = fromEdn . field "batches"

decIds :: Edn -> Either Text [HosiId]
decIds = fromEdn . field "ids"

decId :: Edn -> Either Text HosiId
decId = fromEdn . field "id"

-- | By hand: the tags are not the names of the constructors, and a time is milliseconds here, a
-- 'UTCTime' there.
instance FromEdn Erasure where
  fromEdn e = case e of
    EVec [EKw "ids", is]       -> EraseIds <$> fromEdn is
    EVec [EKw "range", lo, hi] -> EraseRange <$> fromEdn lo <*> fromEdn hi
    EVec [EKw "between", a, b] -> EraseBetween <$> time a <*> time b
    EVec [EKw "owned-by", attrs, owners] ->
      (\as os -> EraseOwnedBy as os Nothing) <$> fromEdn attrs <*> fromEdn owners
    EVec [EKw "owned-by-between", attrs, owners, a, b] ->
      (\as os from to -> EraseOwnedBy as os (Just (from, to))) <$> fromEdn attrs <*> fromEdn owners <*> time a <*> time b
    _ -> Left "not an erasure"
    where
      time :: Edn -> Either Text UTCTime
      time x = (\ms -> posixSecondsToUTCTime (fromInteger ms / 1000)) <$> fromEdn x

decErasures :: Edn -> Either Text [Erasure]
decErasures = fromEdn . field "erasures"

-- | The arguments of @clean@ may be left out altogether; so may @:max-chunks@.
decCleanOptions :: Edn -> Either Text CleanOptions
decCleanOptions e = do
  n <- fromEdn (field "max-chunks" e)
  pure defaultCleanOptions { coMaxChunks = maybe (coMaxChunks defaultCleanOptions) id n }

decFreezeOptions :: Edn -> Either Text FreezeOptions
decFreezeOptions e = do
  n <- fromEdn (field "max-units" e)
  pure defaultFreezeOptions { foMaxUnits = maybe (foMaxUnits defaultFreezeOptions) id n }

decQueries :: Edn -> Either Text [(Text, [Input])]
decQueries e = do
  qs <- fromEdn (field "queries" e)
  traverse query qs
  where
    query :: Edn -> Either Text (Text, [Input])
    query q = do
      txt <- case field "q" q of
        EStr t -> Right t
        _      -> Left "no query text"
      ins <- fromEdn (field "inputs" q)
      pure (txt, maybe [] id ins)

-- ---------------------------------------------------------------------------
-- Out

instance ToEdn Value where
  toEdn v = case v of
    VInt n    -> toEdn n
    VDouble d -> EFloat d
    VStr t    -> EStr t
    VBool b   -> EBool b
    VKw k     -> toEdn k

instance ToEdn Kind where
  toEdn = EKw . T.pack . kindName

-- | A 'Month' (it is a pair of 'Int's) is written as its text, @"2026-11"@.
instance ToEdn Month where
  toEdn = EStr . T.pack . showMonth

-- | The temporary numbers a write was given, with the ids they became: @[[1 1001] [2 1002]]@.
instance ToEdn (M.Map Int HosiId) where
  toEdn m = EVec [EVec [toEdn n, toEdn i] | (n, i) <- M.toList m]

-- | What one chunk answered to a look-up: the chunk's name, what it said, and what it found.
instance ToEdn (String, Probe Int) where
  toEdn (name, p) = EMap [(EKw "chunk", toEdn name), (EKw "says", EKw says), (EKw "found", found)]
    where
      says = case p of
        NotStar   -> "not-star"
        LookAlike -> "look-alike"
        Star _    -> "star"
        EmptySky  -> "empty-sky"
      found = case p of
        Star n -> toEdn n
        _      -> ENil

instance ToEdn WriteReport where toEdn = genericToEdn defaultStyle
instance ToEdn Stats where toEdn = genericToEdn defaultStyle
instance ToEdn Event where toEdn = genericToEdn defaultStyle
instance ToEdn SealReport where toEdn = genericToEdn defaultStyle
instance ToEdn LookupTrace where toEdn = genericToEdn defaultStyle

-- | What the chunk is, not the files it is made of (they are not offered), and not its erase record
-- (a file of the world's, which the doors do not show yet).
instance ToEdn ChunkEntry where
  toEdn = genericToEdn defaultStyle { styleSkip = (`elem` ["ceFiles", "ceErase"]) }

-- | Never written (see 'ChunkEntry'); here because the instance of 'ChunkEntry' needs the
-- instances of all its fields.
instance ToEdn FileEntry where
  toEdn = genericToEdn defaultStyle

-- | A pulled tree: a scalar is a value, many is a vector, an entity is a map in the order it came.
instance ToEdn Pulled where
  toEdn p = case p of
    PScalar v  -> toEdn v
    PMany ps   -> toEdn ps
    PEntity as -> EMap [(toEdn k, toEdn x) | (k, x) <- as]

instance ToEdn Cell where
  toEdn c = case c of
    CVal v  -> toEdn v
    CPull p -> EVec [EKw "pull", toEdn p]

-- | One answer of @write-many@: @[:ok report]@, or @[:error kind message]@.
encWriteResult :: Either HosiError WriteReport -> Edn
encWriteResult r = case r of
  Right w -> EVec [EKw "ok", toEdn w]
  Left e  -> let (k, m) = errorKind e in EVec [EKw "error", EKw k, EStr (T.pack m)]

-- | The kind and message that a 'HosiError' is told as (also used for the status of a failed call).
errorKind :: HosiError -> (Text, String)
errorKind e = case e of
  WriteRejected m -> ("write-rejected", m)
  Corrupt m       -> ("corrupt", m)
  BadArgument m   -> ("bad-argument", m)

-- | @{:events [...] :trace {...}}@
encTrace :: ([Event], LookupTrace) -> Edn
encTrace (evs, t) = EMap [(EKw "events", toEdn evs), (EKw "trace", toEdn t)]

encEraseReport :: EraseReport -> Edn
encEraseReport r = EMap
  [ (EKw "generation", toEdn (erGeneration r)), (EKw "dropped", names (erDropped r))
  , (EKw "recorded", names (erRecorded r)), (EKw "retracted", toEdn (erRetracted r))
  , (EKw "owned", toEdn (erOwned r)) ]

-- | A chunk that was rewritten, and the one written in its place (nil: nothing of it was left).
encCleanReport :: CleanReport -> Edn
encCleanReport r = EMap
  [ (EKw "rewritten", EVec [ EMap [(EKw "chunk", name old), (EKw "into", maybe ENil name new)] | (old, new) <- crRewritten r ])
  , (EKw "skipped", names (crSkipped r)), (EKw "records-left", toEdn (crRecordsLeft r))
  , (EKw "khana-cleaned", EBool (crKhanaCleaned r)), (EKw "khana-rebuilt", EBool (crKhanaRebuilt r)) ]

-- | A unit (kind, month) that went cold, and the cold chunk it became (nil: nothing of it was left).
encFreezeReport :: FreezeReport -> Edn
encFreezeReport r = EMap
  [ (EKw "frozen", EVec [ EMap [(EKw "kind", toEdn k), (EKw "month", toEdn m), (EKw "into", maybe ENil name new)] | ((k, m), new) <- frFrozen r ])
  , (EKw "skipped", EVec [ EMap [(EKw "kind", toEdn k), (EKw "month", toEdn m)] | (k, m) <- frSkipped r ]) ]

name :: String -> Edn
name = EStr . T.pack

names :: [String] -> Edn
names = EVec . map name
