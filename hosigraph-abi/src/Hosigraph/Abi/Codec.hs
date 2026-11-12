{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Abi.Codec
-- Description : Between EDN and hosigraph's own types.  Pure; the shapes are those of "Hosigraph.Abi.Api".
--
-- The decoders run after the request has been checked against its 'Setu.Spec.Ty', so what they
-- refuse is a disagreement between this module and the 'Setu.Spec.Ty' (a mistake of ours, which
-- the dispatcher reports as the library's, not the caller's).  The encoders are checked in turn
-- by the dispatcher against the result's 'Setu.Spec.Ty'.
module Hosigraph.Abi.Codec
  ( -- * Decoding what comes in
    decOps, decBatches, decIds, decId, decQueries
    -- * Encoding what goes out
  , encWriteReport, encWriteResult, encSealReport, encEntities
  , encRows, encCellRows, encEvents, encTrace, encStats, encStrings
  , errorKind
    -- * One value at a time (for the tests)
  , decValue, encValue, decRef, decVal, decOp, decInput, encCell, encPulled, encEvent
  ) where

import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import           Hosigraph
import           Hosigraph.Event (kindName)
import           Hosigraph.Manifest (ChunkEntry(..))
import           Takka.Edn (Edn(..))
import           Takka.Syntax (Pulled(..))

type Dec a = Edn -> Either Text a

-- ---------------------------------------------------------------------------
-- In

decValue :: Dec Value
decValue e = case e of
  EInt n   -> Right (VInt (fromInteger n))
  EFloat d -> Right (VDouble d)
  EStr t   -> Right (VStr t)
  EBool b  -> Right (VBool b)
  EKw k    -> Right (VKw (Keyword k))
  _        -> Left "not a value"

decRef :: Dec Ref
decRef e = case e of
  EInt n                   -> Right (Id (fromInteger n))
  EVec [EKw "tmp", EInt n] -> Right (Tmp (fromInteger n))
  _                        -> Left "not a ref"

decVal :: Dec Val
decVal e = case e of
  EVec [EKw "ref", r] -> RefTo <$> decRef r
  _                   -> Lit <$> decValue e

decOp :: Dec Op
decOp e = case e of
  EVec [EKw "create", r, EKw a, v] -> Create <$> decRef r <*> pure (Keyword a) <*> decVal v
  EVec [EKw "delete", r, EKw a, v] -> Delete <$> decRef r <*> pure (Keyword a) <*> decVal v
  _                                -> Left "not an op"

decInput :: Dec Input
decInput e = case e of
  EVec [EKw "scalar", v] -> InScalar <$> decValue v
  EVec [EKw "coll", EVec vs] -> InColl <$> mapM decValue vs
  EVec [EKw "rel", EVec rows] -> InRel <$> mapM (\r -> case r of EVec vs -> mapM decValue vs; _ -> Left "not a row") rows
  _ -> Left "not an input"

-- | The value under a key of the arguments (a missing key is nil).
field :: Text -> Edn -> Edn
field k e = case e of
  EMap ps -> maybe ENil id (lookup (EKw k) ps)
  _       -> ENil

vec :: Dec a -> Dec [a]
vec d e = case e of
  EVec xs -> mapM d xs
  _       -> Left "not a vector"

decOps :: Dec [Op]
decOps = vec decOp . field "ops"

decBatches :: Dec [[Op]]
decBatches = vec (vec decOp) . field "batches"

decIds :: Dec [HosiId]
decIds = vec decInt . field "ids"

decId :: Dec HosiId
decId = decInt . field "id"

decInt :: Dec HosiId
decInt e = case e of
  EInt n -> Right (fromInteger n)
  _      -> Left "not an integer"

decQueries :: Dec [(Text, [Input])]
decQueries = vec query . field "queries"
  where
    query q = do
      txt <- case field "q" q of
        EStr t -> Right t
        _      -> Left "no query text"
      ins <- case field "inputs" q of
        ENil -> Right []
        v    -> vec decInput v
      pure (txt, ins)

-- ---------------------------------------------------------------------------
-- Out

int :: Integral a => a -> Edn
int = EInt . toInteger

kw :: Keyword -> Edn
kw (Keyword t) = EKw t

encValue :: Value -> Edn
encValue v = case v of
  VInt n    -> int n
  VDouble d -> EFloat d
  VStr t    -> EStr t
  VBool b   -> EBool b
  VKw k     -> kw k

obj :: [(Text, Edn)] -> Edn
obj kvs = EMap [(EKw k, v) | (k, v) <- kvs]

encWriteReport :: WriteReport -> Edn
encWriteReport r = obj
  [ ("temps", EVec [EVec [int n, int i] | (n, i) <- M.toList (wrTemps r)])
  , ("first-seq", int (wrFirstSeq r)), ("last-seq", int (wrLastSeq r)), ("sealed", EBool (wrSealed r)) ]

-- | One answer of @write-many@: @[:ok report]@, or @[:error kind message]@.
encWriteResult :: Either HosiError WriteReport -> Edn
encWriteResult r = case r of
  Right w -> EVec [EKw "ok", encWriteReport w]
  Left e  -> let (k, m) = errorKind e in EVec [EKw "error", EKw k, EStr (T.pack m)]

-- | The kind and message that a 'HosiError' is told as (also used for the status of a failed call).
errorKind :: HosiError -> (Text, String)
errorKind e = case e of
  WriteRejected m -> ("write-rejected", m)
  Corrupt m       -> ("corrupt", m)
  BadArgument m   -> ("bad-argument", m)

encSealReport :: Maybe SealReport -> Edn
encSealReport Nothing  = ENil
encSealReport (Just r) = obj
  [ ("events", int (srEvents r)), ("generation", int (srGeneration r))
  , ("chunks", EVec (map chunk (srChunks r))) ]
  where
    chunk c = obj
      [ ("name", EStr (T.pack (ceName c))), ("tier", EStr (T.pack (ceTier c)))
      , ("kind", EKw (T.pack (kindName (ceKind c))))
      , ("month", EStr (T.pack (showMonth (ceMonth c))))
      , ("events", int (ceEvents c)), ("ids", int (ceIds c))
      , ("min-id", int (ceMinId c)), ("max-id", int (ceMaxId c))
      , ("min-seq", int (ceMinSeq c)), ("max-seq", int (ceMaxSeq c))
      , ("hash", EStr (T.pack (ceHash c))) ]

encEntities :: [M.Map Keyword [Value]] -> Edn
encEntities = EVec . map one
  where one m = EMap [(kw k, EVec (map encValue vs)) | (k, vs) <- M.toList m]

encRows :: [[[Value]]] -> Edn
encRows = EVec . map (EVec . map (EVec . map encValue))

encCell :: Cell -> Edn
encCell c = case c of
  CVal v  -> encValue v
  CPull p -> EVec [EKw "pull", encPulled p]

-- | A tree: a scalar is a value, many is a vector, an entity is a map in the order it came.
encPulled :: Pulled -> Edn
encPulled p = case p of
  PScalar v  -> encValue v
  PMany ps   -> EVec (map encPulled ps)
  PEntity as -> EMap [(kw k, encPulled x) | (k, x) <- as]

encCellRows :: [[[Cell]]] -> Edn
encCellRows = EVec . map (EVec . map (EVec . map encCell))

encEvent :: Event -> Edn
encEvent ev = obj
  [ ("seq", int (evSeq ev)), ("kind", EKw (T.pack (kindName (evKind ev)))), ("id", int (evId ev))
  , ("attr", kw (evAttr ev)), ("val", encValue (evVal ev)) ]

encEvents :: [Event] -> Edn
encEvents = EVec . map encEvent

-- | @{:events [...] :trace {...}}@
encTrace :: ([Event], LookupTrace) -> Edn
encTrace (evs, t) = obj
  [ ("events", encEvents evs)
  , ("trace", obj
      [ ("month", EStr (T.pack (showMonth (ltMonth t)))), ("chunks", int (ltChunks t))
      , ("skipped-by-month", int (ltSkippedByMonth t)), ("skipped-by-range", int (ltSkippedByRange t))
      , ("skipped-by-filter", int (ltSkippedByFilter t))
      , ("probed", EVec [ obj [("chunk", EStr (T.pack n)), ("says", says p), ("found", found p)] | (n, p) <- ltProbed t ])
      , ("tail-events", int (ltTailEvents t)) ]) ]
  where
    says p = EKw $ case p of
      NotStar   -> "not-star"
      LookAlike -> "look-alike"
      Star _    -> "star"
      EmptySky  -> "empty-sky"
    found p = case p of
      Star n -> int n
      _      -> ENil

encStats :: Stats -> Edn
encStats s = obj
  [ ("generation", int (stGeneration s)), ("chunks", int (stChunks s))
  , ("sealed-seq", int (stSealedSeq s)), ("next-seq", int (stNextSeq s))
  , ("tail-events", int (stTailEvents s)), ("tail-bytes", int (stTailBytes s))
  , ("folded-seq", int (stFoldedSeq s)), ("khana-datoms", int (stKhanaDatoms s)) ]

encStrings :: [String] -> Edn
encStrings = EVec . map (EStr . T.pack)
