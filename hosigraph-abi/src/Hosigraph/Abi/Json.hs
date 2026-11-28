-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Abi.Json
-- Description : The same requests and answers as the EDN of "Hosigraph.Abi.Api", written as JSON.
--
-- The contract is the EDN one; this module only writes it down in another hand, at the door.  Where
-- the 'Ty' of an operation says what is expected, that decides how the JSON is read, so a request
-- reads the way one would write it:
--
-- > {"op":"write","args":{"ops":[["create",["tmp",1],"person/name","Frege"]]}}
-- > {"ok":{"temps":[[1,58047583928451072]],"first-seq":1,"last-seq":1,"sealed":false}}
--
-- * An object is a map (a key is a keyword without its colon: @"person/name"@), an array is a vector,
--   and a tagged vector starts with its tag: @["create", ...]@.
-- * A keyword where the shape says keyword is a bare string (@"person/name"@, @"create"@).  Where a /value/
--   may be a string or a keyword, a string that starts with @:@ is the keyword (@":active"@), and a string
--   that starts with @:@ by itself is written with one more (@"::x"@ is the string @":x"@).
-- * 1 and 1.0: where the shape asks for a double, any number is one.  Where it asks for a value, the
--   attribute decides when the schema of @open@ names it (@:double@ makes @1@ a double; @:long@ makes @1.0@ an
--   error from hosigraph's own check), and otherwise a number written with a fraction or an exponent is a double.
--   An answer writes a double with a fraction, always (@1.0@).
--   To say it yourself, write @{"type":"double","value":1}@ (also @long@, @string@, @keyword@, @uuid@); the schema does not change that.
-- * A UUID is @{"type":"uuid","value":"01a21aec-c46b-7226-b63c-5ab791b7584a"}@, in a request and in an answer.  Where the
--   schema of @open@ says that an attribute is a @:uuid@, a request may write the value of that attribute as a plain
--   string; the text of a query and its inputs have no attribute to go by, and say it in full.
-- * An integer is a number, or a string of digits where the shape asks for an integer (and for a value
--   of an attribute the schema says is @:long@ or @:ref@).  A number past 2^53 is not safe in every
--   language (JavaScript rounds it), so with @idsAsStrings@ an answer writes those as strings of digits.
-- * An answer is @{"ok": ...}@ or @{"error": "kind", "message": "..."}@.
module Hosigraph.Abi.Json
  ( JsonOptions(..)
  , jsonRequest
  , jsonReply
  , jsonError
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isDigit)
import qualified Data.Map.Strict as M
import           Data.Scientific (base10Exponent, isInteger, toRealFloat)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import           Setu.Spec
import           Setu.Validate (accepts)
import           Setu.Wire (Reply(..), Request(..), decodeReply, encodeRequest)
import           Takka.Edn (Edn(..))
import qualified Takka.Types as K

data JsonOptions = JsonOptions
  { joIdsAsStrings :: Bool         -- ^ answers write integers past ±2^53 as strings of digits
  , joSchema       :: K.Schema     -- ^ the schema @open@ was given: it settles 1 against 1.0 for an attribute
  }

-- ---------------------------------------------------------------------------
-- A request

-- | A request in JSON, as the EDN request it stands for, and the name of the operation (to read the answer by).
jsonRequest :: Api -> JsonOptions -> ByteString -> Either Text (Text, ByteString)
jsonRequest api opts body = do
  v <- either (Left . ("the line is not JSON: " <>) . T.pack) Right (A.eitherDecodeStrict body)
  o <- case v of
    A.Object o -> Right o
    _          -> Left "request must be an object"
  mapM_ (\k -> if AK.toText k `elem` ["op", "args"] then Right () else Left ("request: unknown key " <> AK.toText k)) (KM.keys o)
  name <- case KM.lookup "op" o of
    Just (A.String n) -> Right (T.dropWhile (== ':') n)
    Just _            -> Left "request: \"op\" must be a string"
    Nothing           -> Left "request: \"op\" is missing"
  -- an operation nobody offers is the dispatcher's to refuse (unknown-op); its arguments are read as they come
  let ty = maybe TAny opArgs (lookup name [(opName x, x) | x <- allOps api])
  args <- case KM.lookup "args" o of
    Nothing -> Right ENil
    Just a  -> dec [PK "args"] ty (wrapByAttribute (joSchema opts) a)
  Right (name, encodeRequest (Request name args))

data Seg = PK Text | PI Int

where' :: [Seg] -> Text
where' = T.concat . map seg . reverse
  where seg (PK k) = "/" <> k
        seg (PI i) = "[" <> T.pack (show i) <> "]"

bad :: [Seg] -> Text -> Either Text a
bad rp msg = Left ("at " <> where' rp <> ": " <> msg)

dec :: [Seg] -> Ty -> A.Value -> Either Text Edn
dec rp ty v
  | Just (t, x) <- wrapper v, not (mapLike ty) = typed rp t x
  | otherwise = dec' rp ty v

dec' :: [Seg] -> Ty -> A.Value -> Either Text Edn
dec' rp ty v = case (ty, v) of
  (TAny, _)               -> generic rp v
  (TNil, A.Null)          -> Right ENil
  (TOpt _, A.Null)        -> Right ENil
  (TOpt t, _)             -> dec rp t v
  (TBool, A.Bool b)       -> Right (EBool b)
  (TInt, _)               -> integer rp v
  (TDouble, A.Number n)   -> Right (EFloat (toRealFloat n))
  (TStr, A.String s)      -> Right (EStr s)
  (TKw, A.String s)       -> Right (EKw (bare s))
  (TEnum _, A.String s)   -> Right (EKw (bare s))
  (TVec t, A.Array xs)    -> EVec <$> mapM (\(i, x) -> dec (PI i : rp) t x) (zip [0 ..] (V.toList xs))
  (TMap fs, A.Object o)   -> EMap <$> mapM (\(k, x) ->
                               let n = AK.toText k
                               in (,) (EKw n) <$> dec (PK n : rp) (maybe TAny fieldTy (lookupField n fs)) x) (KM.toList o)
  (TDict t, A.Object o)   -> EMap <$> mapM (\(k, x) -> (,) (EKw (bare (AK.toText k))) <$> dec (PK (AK.toText k) : rp) t x) (KM.toList o)
  (TTag vs, A.Array xs)
    | (A.String tag : rest) <- V.toList xs -> tagged rp vs (bare tag) rest
  (TUnion ts, _)          -> union rp ts v
  _                       -> bad rp ("expected " <> shape ty <> ", got " <> kind v)

-- the keyword of a name that may or may not carry its colon
bare :: Text -> Text
bare = T.dropWhile (== ':')

lookupField :: Text -> [Field] -> Maybe Field
lookupField n fs = case [f | f <- fs, fieldName f == n] of { (f : _) -> Just f; [] -> Nothing }

tagged :: [Seg] -> [Variant] -> Text -> [A.Value] -> Either Text Edn
tagged rp vs tag rest = case [x | x <- vs, variantTag x == tag] of
  (x : _) | length (variantArgs x) == length rest ->
    (\as -> EVec (EKw tag : as)) <$> mapM (\(i, t, a) -> dec (PI i : rp) t a) (zip3 [1 ..] (variantArgs x) rest)
  -- a tag nobody has, or the wrong number of arguments: read it plainly and let the shape check say what is wrong
  _ -> (\as -> EVec (EKw tag : as)) <$> mapM (generic rp) rest

union :: [Seg] -> [Ty] -> A.Value -> Either Text Edn
union rp ts v = case v of
  A.Null       -> Right ENil
  A.Bool b     -> Right (EBool b)
  A.Number n
    | has TDouble && (not (has TInt) || not (isInteger n) || base10Exponent n < 0) -> Right (EFloat (toRealFloat n))
    | otherwise -> integer rp v
  A.String s
    | Just k <- T.stripPrefix ":" s, not (":" `T.isPrefixOf` k), hasKw -> Right (EKw k)
    | hasStr    -> Right (EStr (unescape s))
    | hasKw     -> Right (EKw (bare s))
    | has TInt  -> integer rp v
  A.Array xs
    | (A.String tag : rest) <- V.toList xs, (TTag vs : _) <- [t | t@(TTag _) <- ts] -> tagged rp vs (bare tag) rest
    | (TVec t : _) <- [t | t@(TVec _) <- ts] -> dec rp t v
  A.Object _
    | (t : _) <- [t | t <- ts, isMap t] -> dec rp t v
  _ -> bad rp ("expected " <> T.intercalate " or " (map shape ts) <> ", got " <> kind v)
  where
    has t   = t `elem` ts
    hasStr  = has TStr
    hasKw   = has TKw
    isMap t = case t of { TMap _ -> True; TDict _ -> True; _ -> False }

-- "::x" is the string ":x"
unescape :: Text -> Text
unescape s = case T.stripPrefix "::" s of { Just r -> ":" <> r; Nothing -> s }

-- | An integer from a number, or from a string of digits.
integer :: [Seg] -> A.Value -> Either Text Edn
integer rp v = case v of
  A.Number n | isInteger n -> Right (EInt (truncate n))
             | otherwise   -> bad rp "expected integer, got a number with a fraction"
  A.String s | Just ds <- digits s -> Right (EInt (read (T.unpack ds)))
  _ -> bad rp ("expected integer, got " <> kind v)
  where digits s = let b = fromMaybeText (T.stripPrefix "-" s) s
                   in if not (T.null b) && T.all isDigit b then Just s else Nothing
        fromMaybeText m d = maybe d id m

-- | What the shape does not describe (@ping@, a pulled tree): read as it comes.  A string that starts
-- with @:@ is a keyword here too.
generic :: [Seg] -> A.Value -> Either Text Edn
generic rp v | Just (t, x) <- wrapper v = typed rp t x
generic rp v = case v of
  A.Null      -> Right ENil
  A.Bool b    -> Right (EBool b)
  A.Number n
    | isInteger n && base10Exponent n >= 0 -> Right (EInt (truncate n))
    | otherwise -> Right (EFloat (toRealFloat n))
  A.String s
    | Just k <- T.stripPrefix ":" s, not (":" `T.isPrefixOf` k) -> Right (EKw k)
    | otherwise -> Right (EStr (unescape s))
  A.Array xs  -> EVec <$> mapM (\(i, x) -> generic (PI i : rp) x) (zip [0 ..] (V.toList xs))
  A.Object o  -> EMap <$> mapM (\(k, x) -> (,) (EKw (bare (AK.toText k))) <$> generic (PK (AK.toText k) : rp) x) (KM.toList o)

-- | A number or a string of digits that the author means as a type of their own: @{"type":"double","value":1}@.
-- The types are the names the schema of @open@ uses: @double@, @long@, @string@, @keyword@, @uuid@.  It is read
-- wherever a value or a number may stand, and not where an object is a map of its own (a 'TMap', a 'TDict').
wrapper :: A.Value -> Maybe (Text, A.Value)
wrapper v = case v of
  A.Object o | KM.size o == 2, Just (A.String t) <- KM.lookup "type" o, Just x <- KM.lookup "value" o -> Just (t, x)
  _ -> Nothing

mapLike :: Ty -> Bool
mapLike ty = case ty of
  TMap _    -> True
  TDict _   -> True
  TOpt t    -> mapLike t
  TUnion ts -> any mapLike ts
  _         -> False

typed :: [Seg] -> Text -> A.Value -> Either Text Edn
typed rp t x = case (t, x) of
  ("double",  A.Number n) -> Right (EFloat (toRealFloat n))
  ("long",    _)          -> integer (PK "value" : rp) x
  ("string",  A.String s) -> Right (EStr s)
  ("keyword", A.String s) -> Right (EKw (bare s))
  ("uuid",    A.String s) -> case K.parseUuid s of
    Just (a, b) -> Right (EUuid a b)
    Nothing     -> bad (PK "value" : rp) ("not a UUID: " <> T.take 40 s)
  _ | t `elem` ["double", "long", "string", "keyword", "uuid"] -> bad (PK "value" : rp) ("expected the value of a " <> t <> ", got " <> kind x)
    | otherwise -> bad (PK "type" : rp) ("unknown type \"" <> t <> "\", expected one of double, long, string, keyword, uuid")

-- | Where an operation names an attribute and a value, the schema of the attribute has the say, for what the author did
-- not say themselves: @["create", r, "a", 3]@ with @a@ a @:double@ holds 1.0, and with @a@ a @:long@ a string of digits is the
-- integer.  It is written as the @{"type": ...}@ the author could have written, so that what they did write is left alone.
wrapByAttribute :: K.Schema -> A.Value -> A.Value
wrapByAttribute schema = go
  where
    go v = case v of
      A.Array xs -> case V.toList xs of
        [A.String t, r, A.String a, x]
          | bare t `elem` ["create", "delete", "holds", "lacks"], Just sch <- M.lookup (K.Keyword (bare a)) schema ->
              A.Array (V.fromList [A.String t, go r, A.String a, fixValue (K.asType sch) x])
        ys -> A.Array (V.fromList (map go ys))
      A.Object o -> A.Object (fmap go o)
      _ -> v
    fixValue ty x = case (ty, x) of
      (K.TDouble, A.Number _)                -> wrap "double" x
      (K.TLong, A.String s) | isDigits s     -> wrap "long" x
      (K.TUuid, A.String _)                  -> wrap "uuid" x
      _                                      -> go x
    wrap t x = A.object ["type" A..= (t :: Text), "value" A..= x]
    isDigits s = let b = maybe s id (T.stripPrefix "-" s) in not (T.null b) && T.all isDigit b

-- ---------------------------------------------------------------------------
-- An answer

-- | The EDN answer of an operation, as one line of JSON.
jsonReply :: Api -> JsonOptions -> Text -> ByteString -> ByteString
jsonReply api opts name body = case decodeReply body of
  Left e                -> jsonError "bad-answer" (T.pack (show e))
  Right (ReplyErr k m)  -> jsonError k m
  Right (ReplyOk v)     -> TE.encodeUtf8 ("{\"ok\":" <> enc opts ty v <> "}")
  where ty = maybe TAny opResult (lookup name [(opName x, x) | x <- allOps api])

jsonError :: Text -> Text -> ByteString
jsonError kind msg = TE.encodeUtf8 ("{\"error\":" <> string kind <> ",\"message\":" <> string msg <> "}")

string :: Text -> Text
string = TE.decodeUtf8 . BL.toStrict . A.encode . A.String

enc :: JsonOptions -> Ty -> Edn -> Text
enc opts = go
  where
    go ty e = case (ty, e) of
      -- where a string and a keyword can both stand, the keyword has its colon and a string that starts with one has two
      (TUnion ts, EKw k)  | TStr `elem` ts, TKw `elem` ts -> string (":" <> k)
      (TUnion ts, EStr s) | TKw `elem` ts, TStr `elem` ts -> string (if ":" `T.isPrefixOf` s then ":" <> s else s)
      (TUnion ts, _)         -> case [t | t <- ts, accepts t e] of { (t : _) -> go t e; [] -> generic' e }
      (TStr, EStr s)         -> string s
      (TOpt t, _) | e /= ENil -> go t e
      (TKw, EKw k)           -> string k
      (TEnum _, EKw k)       -> string k
      (TVec t, EVec xs)      -> arr (map (go t) xs)
      (TMap fs, EMap ps)     -> obj [(k, go (maybe TAny fieldTy (lookupField k fs)) x) | (EKw k, x) <- ps]
      (TDict t, EMap ps)     -> obj [(k, go t x) | (EKw k, x) <- ps]
      (TTag vs, EVec (EKw tag : as))
        | (x : _) <- [x | x <- vs, variantTag x == tag] -> arr (string tag : zipWith go (variantArgs x) as)
      _                      -> generic' e
    -- a value without a shape to guide it: a keyword is a string with its colon
    generic' e = case e of
      ENil      -> "null"
      EBool b   -> if b then "true" else "false"
      EInt n    -> int n
      EFloat d  -> float d
      EStr s    -> string (if ":" `T.isPrefixOf` s then ":" <> s else s)
      EKw k     -> string (":" <> k)
      EUuid a b -> obj [("type", string "uuid"), ("value", string (K.uuidText a b))]
      ESym s    -> string s
      EVec xs   -> arr (map generic' xs)
      EList xs  -> arr (map generic' xs)
      ESet xs   -> arr (map generic' xs)
      EMap ps   -> obj [(keyName k, generic' x) | (k, x) <- ps]
    keyName k = case k of { EKw n -> n; EStr s -> s; EUuid a b -> K.uuidText a b; _ -> T.pack (show k) }
    int n | joIdsAsStrings opts && abs n > 9007199254740991 = string (T.pack (show n))
          | otherwise = T.pack (show n)
    float d = let t = T.pack (show d)
              in if T.any (`elem` (".eE" :: String)) t then t else t <> ".0"
    arr xs = "[" <> T.intercalate "," xs <> "]"
    obj ps = "{" <> T.intercalate "," [string k <> ":" <> x | (k, x) <- ps] <> "}"

-- ---------------------------------------------------------------------------
-- Words for messages

shape :: Ty -> Text
shape ty = case ty of
  TNil -> "null"; TBool -> "boolean"; TInt -> "integer"; TDouble -> "number"; TStr -> "string"
  TKw -> "keyword string"; TUuid -> "uuid"; TAny -> "anything"; TVec _ -> "array"; TMap _ -> "object"; TDict _ -> "object"
  TOpt t -> shape t <> " or null"; TUnion ts -> T.intercalate " or " (map shape ts)
  TEnum ns -> "one of " <> T.intercalate ", " ns; TTag _ -> "an array like [\"tag\", ...]"

kind :: A.Value -> Text
kind v = case v of
  A.Null -> "null"; A.Bool _ -> "boolean"; A.Number _ -> "number"; A.String _ -> "string"
  A.Array _ -> "array"; A.Object _ -> "object"
