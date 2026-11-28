-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka.Edn
-- Description : A small EDN reader — enough for Datalog queries and tx-data.
--
-- Supported: nil, booleans, integers, floats, strings (with escapes), keywords,
-- symbols, lists, vectors, maps, sets, @#uuid "..."@, @;@ comments, @#_@ discard,
-- commas as whitespace.  Not supported: other tagged literals (@#inst@, say),
-- characters, ratios, namespaced-map syntax.
--
-- Keywords are read permissively (everything up to whitespace or a delimiter)
-- so IRIs such as @:http://schema.org/name@ are fine.
module Takka.Edn
  ( Edn(..)
  , readEdn, readEdnMany
  , renderEdn
  , ednToValue
  ) where

import           Data.Char (chr, isDigit, isHexDigit, isSpace, digitToInt, ord)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)
import           Numeric (showHex)
import           Text.Read (readMaybe)

import           Takka.Types (Keyword(..), Value(..), parseUuid, uuidText)

data Edn
  = ENil
  | EBool  Bool
  | EInt   Integer
  | EFloat Double
  | EStr   Text
  | EKw    Text            -- ^ without the leading colon
  | ESym   Text
  | EVec   [Edn]
  | EList  [Edn]
  | EMap   [(Edn, Edn)]
  | ESet   [Edn]
  | EUuid  !Word64 !Word64  -- ^ @#uuid "..."@, as the 128 bits of 'VUuid'
  deriving (Eq, Show)

-- | Read exactly one form (trailing whitespace/comments allowed).
--
-- The reader works on 'Text' directly (no 'String' in between): a string without escapes is a slice of the
-- input, a plain integer is added up digit by digit, and a token is cut with 'T.break'.  What it accepts and
-- the messages it gives are those of the 'String' reader it replaced.
readEdn :: Text -> Either String Edn
readEdn t = do
  (e, rest) <- form (skipWs t)
  let r = skipWs rest
  if T.null r then Right e else Left ("unexpected trailing input: " ++ T.unpack (T.take 20 r))

-- | Read zero or more forms.
readEdnMany :: Text -> Either String [Edn]
readEdnMany t = go (skipWs t)
  where go s | T.null s  = Right []
             | otherwise = do (e, rest) <- form s
                              (e :) <$> go (skipWs rest)

type Parser a = Text -> Either String (a, Text)

skipWs :: Text -> Text
skipWs s0 =
  let s = T.dropWhile (\c -> isSpace c || c == ',') s0
  in case T.uncons s of
       Just (';', cs)  -> skipWs (T.dropWhile (/= '\n') cs)
       Just ('#', cs)
         | Just ('_', cs') <- T.uncons cs ->
             case form (skipWs cs') of
               Right (_, rest) -> skipWs rest
               Left _          -> s        -- let the real parser report the error
       _ -> s

isDelim :: Char -> Bool
isDelim c = isSpace c || c == ',' || c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}' || c == '"' || c == ';'

form :: Parser Edn
form s = case T.uncons s of
  Nothing         -> Left "unexpected end of input"
  Just (c, cs)    -> case c of
    '(' -> EList <$$> seqUntil ')' cs
    '[' -> EVec  <$$> seqUntil ']' cs
    '{' -> do (xs, rest) <- seqUntil '}' cs
              ps <- pairUp xs
              Right (EMap ps, rest)
    '#' | Just ('{', cs') <- T.uncons cs -> ESet <$$> seqUntil '}' cs'
        | ("uuid", rest) <- T.break isDelim cs -> uuid (skipWs rest)
        | otherwise -> Left ("unsupported tagged literal: #" ++ T.unpack (T.takeWhile (not . isDelim) cs))
    '"' -> str cs
    ')' -> Left "unexpected ')'"
    ']' -> Left "unexpected ']'"
    '}' -> Left "unexpected '}'"
    ':' -> let (tok, rest) = T.break isDelim cs
           in if T.null tok then Left "empty keyword" else Right (EKw tok, rest)
    _   -> let (tok, rest) = T.break isDelim s in Right (atom tok, rest)
  where
    f <$$> r = fmap (\(x, rest) -> (f x, rest)) r
    pairUp (a : b : r) = ((a, b) :) <$> pairUp r
    pairUp []          = Right []
    pairUp _           = Left "map literal must have an even number of forms"

-- | What follows @#uuid@: a string that is a UUID.
uuid :: Parser Edn
uuid s = case T.uncons s of
  Just ('"', cs) -> do
    (e, rest) <- str cs
    case e of
      EStr t | Just (a, b) <- parseUuid t -> Right (EUuid a b, rest)
             | otherwise -> Left ("not a UUID: " ++ T.unpack (T.take 40 t))
      _ -> Left "#uuid wants a string"
  _ -> Left "#uuid wants a string"

seqUntil :: Char -> Parser [Edn]
seqUntil close = go []
  where
    go acc s0 = let s = skipWs s0 in case T.uncons s of
      Nothing                  -> Left ("missing closing " ++ [close])
      Just (c, cs) | c == close -> Right (reverse acc, cs)
      _                        -> do (e, rest) <- form s
                                     go (e : acc) rest

atom :: Text -> Edn
atom tok
  | tok == "nil"   = ENil
  | tok == "true"  = EBool True
  | tok == "false" = EBool False
  | looksNumeric tok = case plainInt tok of
      Just n -> EInt n
      Nothing -> maybe (ESym tok) id (number (T.unpack tok))
  | otherwise      = ESym tok

looksNumeric :: Text -> Bool
looksNumeric t = case T.uncons t of
  Just (c, _) | isDigit c -> True
  Just (s, r) | s == '+' || s == '-', Just (c, _) <- T.uncons r, isDigit c -> True
  _ -> False

-- | @[+-]?[0-9]+@ and nothing else: added up digit by digit (a 'readMaybe' for each integer costs microseconds).
-- Anything else (a float, an exponent, a stray letter) goes the old way, through 'number'.
plainInt :: Text -> Maybe Integer
plainInt t0 =
  let (neg, ds) = case T.uncons t0 of
        Just ('-', r) -> (True, r)
        Just ('+', r) -> (False, r)
        _             -> (False, t0)
  in if T.null ds || not (T.all isDigit ds) then Nothing
     else let v | T.length ds <= 18 = toInteger (T.foldl' (\a c -> a * 10 + (ord c - 48)) (0 :: Int) ds)
                | otherwise         = T.foldl' (\a c -> a * 10 + toInteger (ord c - 48)) 0 ds
          in Just (if neg then negate v else v)

number :: String -> Maybe Edn
number tok0
  | any (`elem` (".eE" :: String)) tok = EFloat <$> readMaybe (fixup tok)
  | otherwise                          = EInt   <$> readMaybe tok
  where
    tok = case tok0 of ('+' : r) -> r; _ -> tok0
    fixup t = if '.' `elem` t || last t == '.' then t else t

-- | A string.  The text up to the next @"@ or @\\@ is taken in one piece (the whole string, when it has no escape).
str :: Parser Edn
str = go []
  where
    go acc s =
      let (chunk, rest) = T.break (\c -> c == '"' || c == '\\') s
          acc' = if T.null chunk then acc else chunk : acc
      in case T.uncons rest of
           Nothing -> Left "unterminated string"
           Just ('"', cs) -> Right (EStr (finish acc'), cs)
           Just (_, cs) ->                                   -- a backslash
             case T.uncons cs of
               Nothing -> Left "unterminated string"
               Just (c, cs') -> case c of
                 'n'  -> go (T.singleton '\n' : acc') cs'
                 't'  -> go (T.singleton '\t' : acc') cs'
                 'r'  -> go (T.singleton '\r' : acc') cs'
                 '"'  -> go (T.singleton '"'  : acc') cs'
                 '\\' -> go (T.singleton '\\' : acc') cs'
                 'u' | (h, rest') <- T.splitAt 4 cs', T.length h == 4, T.all isHexDigit h
                     -> go (T.singleton (chr (T.foldl' (\a d -> a * 16 + digitToInt d) 0 h)) : acc') rest'
                 _   -> Left ("bad string escape: \\" ++ [c])
    finish [x] = x
    finish xs  = T.concat (reverse xs)

-- | Write EDN text that 'readEdn' reads back to the same value (for diagnostics, and for bodies
-- that are sent on).
--
-- A string is written as it is: its characters stay what they are (the text is UTF-8 on the wire),
-- and only @"@, @\\@ and control characters are escaped.  (Haskell's @show@ would write a
-- non-ASCII character as @\\26085@, which no EDN reader understands.)  A float that is not
-- finite is written the way Clojure writes it (@##NaN@, @##Inf@, @##-Inf@); 'readEdn' does not
-- read those.  A map is written with a comma between its pairs, as Clojure prints it.
renderEdn :: Edn -> Text
renderEdn e = case e of
  ENil      -> "nil"
  EBool b   -> if b then "true" else "false"
  EInt n    -> T.pack (show n)
  EFloat d
    | isNaN d      -> "##NaN"
    | isInfinite d -> if d > 0 then "##Inf" else "##-Inf"
    | otherwise    -> T.pack (show d)
  EStr s    -> "\"" <> T.concatMap escapeChar s <> "\""
  EKw k     -> T.cons ':' k
  ESym s    -> s
  EVec xs   -> wrap "[" "]" xs
  EList xs  -> wrap "(" ")" xs
  ESet xs   -> wrap "#{" "}" xs
  EUuid a b -> "#uuid \"" <> uuidText a b <> "\""
  EMap ps   -> "{" <> T.intercalate ", " [renderEdn k <> " " <> renderEdn v | (k, v) <- ps] <> "}"
  where wrap o c xs = o <> T.unwords (map renderEdn xs) <> c

escapeChar :: Char -> Text
escapeChar c = case c of
  '"'  -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | c < ' ' || c == '\DEL' -> T.pack ("\\u" ++ pad (showHex (ord c) ""))
    | otherwise              -> T.singleton c
  where pad s = replicate (4 - length s) '0' ++ s

-- | Scalars that can be stored in a datom, and a vector of them (@nil@ for a part that is not there) as a tuple
-- ('VTuple').  @nil@ by itself and the other collections are rejected.
ednToValue :: Edn -> Either String Value
ednToValue e = case e of
  EInt n
    | n >= toInteger (minBound :: Int) && n <= toInteger (maxBound :: Int) -> Right (VInt (fromInteger n))
    | otherwise -> Left ("integer out of 64-bit range: " ++ show n)
  EFloat d -> Right (VDouble d)
  EStr s   -> Right (VStr s)
  EBool b  -> Right (VBool b)
  EKw k    -> Right (VKw (Keyword k))
  EUuid a b -> Right (VUuid a b)
  EVec xs  -> VTuple <$> mapM part xs
  ENil     -> Left "nil cannot be stored (omit the attribute instead)"
  _        -> Left ("not a scalar value: " ++ T.unpack (renderEdn e))
  where
    part ENil       = Right Nothing
    part (EVec _)   = Left "a tuple does not hold a tuple"
    part x          = Just <$> ednToValue x
