{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Setu.Validate
-- Description : Does this EDN have the shape a 'Ty' asks for, and if not, where is it wrong?
--
-- The first mistake found is reported, with the path to it.  The path is an EDN vector
-- (@[:args :ops 3 :value]@), so it can go into an error body as it is.
module Setu.Validate
  ( PathSeg(..), TyError(..)
  , validate, validateAt, accepts
  , renderTyError, renderPath
  ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S

import           Takka.Edn (Edn(..))
import           Setu.Spec

-- | One step down into a value: a map key (without the colon) or a position in a vector.
data PathSeg = PKey Text | PIdx Int deriving (Eq, Show)

data TyError = TyError
  { tyErrPath    :: [PathSeg]   -- ^ from the top; empty when the top value itself is wrong
  , tyErrMessage :: Text
  } deriving (Eq, Show)

validate :: Ty -> Edn -> Either TyError ()
validate = validateAt []

-- | 'validate' for a value that sits at the given path in a bigger one (say, @[PKey "args"]@).
validateAt :: [PathSeg] -> Ty -> Edn -> Either TyError ()
validateAt base ty0 e0 = go (reverse base) ty0 e0
  where
    -- the path is kept reversed while we go down
    go :: [PathSeg] -> Ty -> Edn -> Either TyError ()
    go rp ty e = case (ty, e) of
      (TAny, _)           -> ok
      (TNil, ENil)        -> ok
      (TBool, EBool _)    -> ok
      (TInt, EInt n)
        | n >= toInteger (minBound :: Int64) && n <= toInteger (maxBound :: Int64) -> ok
        | otherwise       -> bad rp ("integer out of 64-bit range: " <> T.pack (show n))
      (TDouble, EFloat d)
        | isNaN d || isInfinite d -> bad rp "float is not finite"
        | otherwise       -> ok
      (TStr, EStr _)      -> ok
      (TKw, EKw _)        -> ok
      (TOpt _, ENil)      -> ok
      (TOpt t, _)         -> go rp t e
      (TVec t, EVec xs)   -> mapM_ (\(i, x) -> go (PIdx i : rp) t x) (zip [0 ..] xs)
      (TEnum ns, EKw k)
        | k `elem` ns     -> ok
        | otherwise       -> bad rp ("expected one of " <> kws ns <> ", got :" <> k)
      (TMap fs, EMap ps)  -> goMap rp fs ps
      (TTag vs, EVec (EKw tag : args)) -> case [v | v <- vs, variantTag v == tag] of
        []      -> bad rp ("unknown tag :" <> tag <> ", expected one of " <> kws (map variantTag vs))
        (v : _)
          | length args /= length (variantArgs v) ->
              bad rp ("[:" <> tag <> " ...] takes " <> T.pack (show (length (variantArgs v)))
                      <> " argument(s), got " <> T.pack (show (length args)))
          | otherwise -> mapM_ (\(i, t, a) -> go (PIdx i : rp) t a) (zip3 [1 ..] (variantArgs v) args)
      _                   -> bad rp ("expected " <> tyName ty <> ", got " <> describe e)

    goMap rp fs ps = do
      keys <- mapM (\(k, _) -> case k of
                       EKw n -> Right n
                       _     -> bad rp ("map keys must be keywords, got " <> describe k)) ps
      case firstDup keys of
        Just k  -> bad (PKey k : rp) ("key :" <> k <> " is given twice")
        Nothing -> Right ()
      case [k | k <- keys, k `notElem` map fieldName fs] of
        (k : _) -> bad (PKey k : rp) ("unknown key :" <> k <> ", expected " <> kws (map fieldName fs))
        []      -> Right ()
      mapM_ (\f -> case lookup (fieldName f) (zip keys (map snd ps)) of
                Just v  -> go (PKey (fieldName f) : rp) (fieldTy f) v
                Nothing
                  | accepts (fieldTy f) ENil -> ok
                  | otherwise                -> bad rp ("missing key :" <> fieldName f)) fs

    ok = Right ()
    bad rp msg = Left (TyError (reverse rp) msg)

-- | Is this value of this shape?
accepts :: Ty -> Edn -> Bool
accepts ty e = either (const False) (const True) (validate ty e)

firstDup :: [Text] -> Maybe Text
firstDup = go S.empty
  where go _ [] = Nothing
        go seen (x : xs) | x `S.member` seen = Just x
                         | otherwise         = go (S.insert x seen) xs

kws :: [Text] -> Text
kws = T.unwords . map (T.cons ':')

tyName :: Ty -> Text
tyName ty = case ty of
  TNil    -> "nil"
  TBool   -> "boolean"
  TInt    -> "integer"
  TDouble -> "float"
  TStr    -> "string"
  TKw     -> "keyword"
  TAny    -> "anything"
  TVec _  -> "vector"
  TMap _  -> "map"
  TOpt t  -> tyName t <> " or nil"
  TEnum ns -> "one of " <> kws ns
  TTag _  -> "a tagged vector like [:tag ...]"

describe :: Edn -> Text
describe e = case e of
  ENil     -> "nil"
  EBool _  -> "boolean"
  EInt _   -> "integer"
  EFloat _ -> "float"
  EStr _   -> "string"
  EKw _    -> "keyword"
  ESym _   -> "symbol"
  EVec _   -> "vector"
  EList _  -> "list"
  EMap _   -> "map"
  ESet _   -> "set"

-- | @at [:args :ops 3]: expected integer, got string@; with an empty path, the message alone.
renderTyError :: TyError -> Text
renderTyError (TyError p m)
  | null p    = m
  | otherwise = "at " <> renderPath p <> ": " <> m

renderPath :: [PathSeg] -> Text
renderPath p = "[" <> T.unwords (map seg p) <> "]"
  where seg (PKey k) = T.cons ':' k
        seg (PIdx i) = T.pack (show i)
