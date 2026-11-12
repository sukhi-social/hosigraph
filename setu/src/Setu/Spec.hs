{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Setu.Spec
-- Description : Say once what an API offers; everything else is written from this.
--
-- An 'Api' lists the operations a library offers and the shape of what goes in and out of each.
-- It says nothing about C, Go or Elixir: the generators read it and write those.
--
-- Bodies on the wire are EDN (T-12: the ABI never looks inside), so 'Ty' describes EDN shapes,
-- not types of any host language.  Operations travel inside the body (@{:op :write :args ...}@),
-- which is why adding an operation does not change the C header.
module Setu.Spec
  ( -- * The description
    Api(..), Op(..)
    -- * Shapes of EDN
  , Ty(..), Field(..), Variant(..)
    -- * Operations every API has
  , reservedOps, allOps
    -- * Checking a description
  , checkApi
  ) where

import           Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

-- | What a library offers.
data Api = Api
  { apiName      :: Text   -- ^ the C prefix, lower case: @"dtlv"@ gives @dtlv_open@, @DTLV_OK@, ...
  , apiVersion   :: Int    -- ^ the contract version (FR-12): bump it when the header or an operation changes incompatibly
  , apiAppErr    :: Text   -- ^ the one status the API names itself, upper case: @"SERVER"@ gives @DTLV_ERR_SERVER@
  , apiAppErrDoc :: Text   -- ^ what that status means, for the comment in the header
  , apiOps       :: [Op]
  } deriving (Eq, Show)

-- | One operation: @{:op :name :args <opArgs>}@ in, @{:ok <opResult>}@ out.
data Op = Op
  { opName   :: Text   -- ^ a keyword name without the colon, kebab case: @"list-databases"@
  , opDoc    :: Text
  , opArgs   :: Ty
  , opResult :: Ty
  } deriving (Eq, Show)

-- | The shape of an EDN value.
--
-- A key that is missing from a map counts as @nil@, so a field is optional exactly when its type
-- accepts @nil@ ('TOpt', 'TNil', 'TAny').  Maps are closed: a key that no field names is refused
-- (a mistyped key should not pass in silence; loosening later breaks nobody, tightening would).
data Ty
  = TNil
  | TBool
  | TInt                    -- ^ an integer that fits in 64 bits
  | TDouble                 -- ^ a finite float (an integer is not a float here)
  | TStr
  | TKw                     -- ^ any keyword
  | TAny                    -- ^ any EDN; the library does not look inside
  | TVec Ty                 -- ^ a vector whose every element has this shape
  | TMap [Field]            -- ^ a map whose keys are keywords
  | TOpt Ty                 -- ^ @nil@, or this shape
  | TEnum [Text]            -- ^ one of these keywords
  | TTag [Variant]          -- ^ a tagged vector: @[:tag arg ...]@
  deriving (Eq, Show)

data Field = Field { fieldName :: Text, fieldTy :: Ty } deriving (Eq, Show)

data Variant = Variant { variantTag :: Text, variantArgs :: [Ty] } deriving (Eq, Show)

-- | The operations every API has, so that the plumbing can be tried without the library's own
-- state: @:ping@ answers with its arguments, @:abi@ names the API.
reservedOps :: [Op]
reservedOps =
  [ Op "ping" "Answers with its arguments, untouched. Needs nothing else to be working." TAny TAny
  , Op "abi" "Names the API, its contract version and its operations." TNil
      (TMap [Field "name" TStr, Field "version" TInt, Field "ops" (TVec TKw)])
  ]

-- | The reserved operations, then the API's own.
allOps :: Api -> [Op]
allOps api = reservedOps ++ apiOps api

-- | What is wrong with a description (empty when nothing is).  The generators write C identifiers
-- from these names, so a bad name must be refused here and not found by a C compiler.
checkApi :: Api -> [Text]
checkApi api = concat
  [ [ "api name " <> q (apiName api) <> ": use a lower case letter, then lower case letters, digits or _"
    | not (T.all isCName (apiName api) && startsLower (apiName api)) ]
  , [ "api version must be 1 or more" | apiVersion api < 1 ]
  , [ "app error name " <> q (apiAppErr api) <> ": use an upper case letter, then upper case letters, digits or _"
    | not (okUpper (apiAppErr api)) ]
  , [ "app error name " <> q (apiAppErr api) <> " is one of the fixed statuses"
    | apiAppErr api `elem` fixedStatuses ]
  , [ "operation " <> q n <> " is reserved" | n <- names, n `elem` map opName reservedOps ]
  , [ "operation " <> q n <> " is given twice" | n <- dups names ]
  , concat [ opProblems op | op <- apiOps api ]
  ]
  where
    names = map opName (apiOps api)
    q t = "\"" <> t <> "\""
    isCName c = isAsciiLower c || isDigit c || c == '_'
    startsLower t = maybe False (isAsciiLower . fst) (T.uncons t)
    okUpper t = case T.uncons t of
      Just (c, r) -> isAsciiUpper c && T.all (\x -> isAsciiUpper x || isDigit x || x == '_') r
      Nothing     -> False
    opProblems op =
      [ "operation " <> q (opName op) <> ": not a kebab-case keyword name" | not (okName (opName op)) ]
      ++ tyProblems ("operation " <> q (opName op) <> ", args") (opArgs op)
      ++ tyProblems ("operation " <> q (opName op) <> ", result") (opResult op)

-- | The statuses the ABI fixes (their order is part of the ABI); only the app error is the API's.
fixedStatuses :: [Text]
fixedStatuses = ["PROTOCOL", "IO", "TIMEOUT", "CANCELLED", "CLOSED", "INTERNAL"]

-- | A name used as a keyword in EDN and as part of an identifier elsewhere.
okName :: Text -> Bool
okName t = case T.uncons t of
  Just (c, r) -> isAsciiLower c && T.all (\x -> isAsciiLower x || isDigit x || x `elem` ("-_./" :: String)) r
  Nothing     -> False

tyProblems :: Text -> Ty -> [Text]
tyProblems where_ ty = case ty of
  TVec t     -> tyProblems (where_ <> ", element") t
  TOpt t     -> tyProblems (where_ <> ", optional") t
  TMap fs    ->
    [ where_ <> ": field name " <> shown n <> " is not a kebab-case keyword name" | n <- map fieldName fs, not (okName n) ]
    ++ [ where_ <> ": field " <> shown n <> " is given twice" | n <- dups (map fieldName fs) ]
    ++ concat [ tyProblems (where_ <> ", field " <> shown (fieldName f)) (fieldTy f) | f <- fs ]
  TEnum ns   ->
    [ where_ <> ": an enum needs at least one name" | null ns ]
    ++ [ where_ <> ": enum name " <> shown n <> " is not a kebab-case keyword name" | n <- ns, not (okName n) ]
    ++ [ where_ <> ": enum name " <> shown n <> " is given twice" | n <- dups ns ]
  TTag vs    ->
    [ where_ <> ": a tagged vector needs at least one tag" | null vs ]
    ++ [ where_ <> ": tag " <> shown n <> " is not a kebab-case keyword name" | n <- map variantTag vs, not (okName n) ]
    ++ [ where_ <> ": tag " <> shown n <> " is given twice" | n <- dups (map variantTag vs) ]
    ++ concat [ tyProblems (where_ <> ", tag " <> shown (variantTag v)) t | v <- vs, t <- variantArgs v ]
  _          -> []
  where shown t = "\"" <> t <> "\""

-- | The names that occur more than once, each once, in order of their second appearance.
dups :: Ord a => [a] -> [a]
dups = go S.empty S.empty
  where
    go _ _ [] = []
    go seen said (x : xs)
      | x `S.member` said = go seen said xs
      | x `S.member` seen = x : go seen (S.insert x said) xs
      | otherwise         = go (S.insert x seen) said xs
