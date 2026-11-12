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
    decOps, decBatches, decIds, decId, decQueries
    -- * The answers that are not one instance
  , encWriteResult, encTrace, errorKind
  ) where

import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Hosigraph
import           Hosigraph.Event (kindName)
import           Hosigraph.Manifest (ChunkEntry(..), FileEntry(..))
import           Setu.Codec
import           Takka.Edn (Edn(..))
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

decOps :: Edn -> Either Text [Op]
decOps = fromEdn . field "ops"

decBatches :: Edn -> Either Text [[Op]]
decBatches = fromEdn . field "batches"

decIds :: Edn -> Either Text [HosiId]
decIds = fromEdn . field "ids"

decId :: Edn -> Either Text HosiId
decId = fromEdn . field "id"

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

-- | What the chunk is, not the files it is made of (they are not offered).
instance ToEdn ChunkEntry where
  toEdn = genericToEdn defaultStyle { styleSkip = (== "ceFiles") }

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
