{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka.Edn
-- Description : A small EDN reader — enough for Datalog queries and tx-data.
--
-- Supported: nil, booleans, integers, floats, strings (with escapes), keywords,
-- symbols, lists, vectors, maps, sets, @;@ comments, @#_@ discard, commas as
-- whitespace.  Not supported: tagged literals (@#inst@, @#uuid@), characters,
-- ratios, namespaced-map syntax.
--
-- Keywords are read permissively (everything up to whitespace or a delimiter)
-- so IRIs such as @:http://schema.org/name@ are fine.
module Takka.Edn
  ( Edn(..)
  , readEdn, readEdnMany
  , renderEdn
  , ednToValue
  ) where

import           Data.Char (chr, isDigit, isHexDigit, isSpace, digitToInt)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Takka.Types (Keyword(..), Value(..))

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
  deriving (Eq, Show)

-- | Read exactly one form (trailing whitespace/comments allowed).
readEdn :: Text -> Either String Edn
readEdn t = do
  (e, rest) <- form (skipWs (T.unpack t))
  case skipWs rest of
    [] -> Right e
    r  -> Left ("unexpected trailing input: " ++ take 20 r)

-- | Read zero or more forms.
readEdnMany :: Text -> Either String [Edn]
readEdnMany t = go (skipWs (T.unpack t))
  where go [] = Right []
        go s  = do (e, rest) <- form s
                   (e :) <$> go (skipWs rest)

type Parser a = String -> Either String (a, String)

skipWs :: String -> String
skipWs s = case s of
  (c : cs) | isSpace c || c == ',' -> skipWs cs
  (';' : cs)                       -> skipWs (dropWhile (/= '\n') cs)
  ('#' : '_' : cs) ->
    case form (skipWs cs) of
      Right (_, rest) -> skipWs rest
      Left _          -> s        -- let the real parser report the error
  _ -> s

isDelim :: Char -> Bool
isDelim c = isSpace c || c `elem` (",()[]{}\";" :: String)

form :: Parser Edn
form s = case s of
  []          -> Left "unexpected end of input"
  ('(' : cs)  -> EList <$$> seqUntil ')' cs
  ('[' : cs)  -> EVec  <$$> seqUntil ']' cs
  ('{' : cs)  -> do (xs, rest) <- seqUntil '}' cs
                    ps <- pairUp xs
                    Right (EMap ps, rest)
  ('#' : '{' : cs) -> ESet <$$> seqUntil '}' cs
  ('#' : cs)  -> Left ("unsupported tagged literal: #" ++ takeWhile (not . isDelim) cs)
  ('"' : cs)  -> str cs
  (')' : _)   -> Left "unexpected ')'"
  (']' : _)   -> Left "unexpected ']'"
  ('}' : _)   -> Left "unexpected '}'"
  (':' : cs)  -> let (tok, rest) = break isDelim cs
                 in if null tok then Left "empty keyword" else Right (EKw (T.pack tok), rest)
  _           -> let (tok, rest) = break isDelim s in Right (atom tok, rest)
  where
    f <$$> r = fmap (\(x, rest) -> (f x, rest)) r
    pairUp (a : b : r) = ((a, b) :) <$> pairUp r
    pairUp []          = Right []
    pairUp _           = Left "map literal must have an even number of forms"

seqUntil :: Char -> Parser [Edn]
seqUntil close = go []
  where
    go acc s = case skipWs s of
      []                       -> Left ("missing closing " ++ [close])
      (c : cs) | c == close    -> Right (reverse acc, cs)
      s'                       -> do (e, rest) <- form s'
                                     go (e : acc) rest

atom :: String -> Edn
atom tok = case tok of
  "nil"   -> ENil
  "true"  -> EBool True
  "false" -> EBool False
  _ | looksNumeric tok -> maybe (ESym (T.pack tok)) id (number tok)
    | otherwise        -> ESym (T.pack tok)

looksNumeric :: String -> Bool
looksNumeric (c : _)        | isDigit c = True
looksNumeric (s : c : _)    | s `elem` ("+-" :: String), isDigit c = True
looksNumeric _              = False

number :: String -> Maybe Edn
number tok0
  | any (`elem` (".eE" :: String)) tok = EFloat <$> readMaybe (fixup tok)
  | otherwise                          = EInt   <$> readMaybe tok
  where
    tok = case tok0 of ('+' : r) -> r; _ -> tok0
    fixup t = if '.' `elem` t || last t == '.' then t else t

str :: Parser Edn
str = go []
  where
    go acc s = case s of
      []            -> Left "unterminated string"
      ('"' : cs)    -> Right (EStr (T.pack (reverse acc)), cs)
      ('\\' : c : cs) -> case c of
        'n' -> go ('\n' : acc) cs
        't' -> go ('\t' : acc) cs
        'r' -> go ('\r' : acc) cs
        '"' -> go ('"' : acc) cs
        '\\' -> go ('\\' : acc) cs
        'u' | (h, rest) <- splitAt 4 cs, length h == 4, all isHexDigit h
            -> go (chr (foldl (\a d -> a * 16 + digitToInt d) 0 h) : acc) rest
        _   -> Left ("bad string escape: \\" ++ [c])
      (c : cs)      -> go (c : acc) cs

-- | Render back to EDN text (used for diagnostics).
renderEdn :: Edn -> Text
renderEdn e = case e of
  ENil      -> "nil"
  EBool b   -> if b then "true" else "false"
  EInt n    -> T.pack (show n)
  EFloat d  -> T.pack (show d)
  EStr s    -> T.pack (show (T.unpack s))
  EKw k     -> T.cons ':' k
  ESym s    -> s
  EVec xs   -> wrap "[" "]" xs
  EList xs  -> wrap "(" ")" xs
  ESet xs   -> wrap "#{" "}" xs
  EMap ps   -> "{" <> T.intercalate ", " [renderEdn k <> " " <> renderEdn v | (k, v) <- ps] <> "}"
  where wrap o c xs = o <> T.unwords (map renderEdn xs) <> c

-- | Scalars that can be stored in a datom.  @nil@ and collections are rejected.
ednToValue :: Edn -> Either String Value
ednToValue e = case e of
  EInt n
    | n >= toInteger (minBound :: Int) && n <= toInteger (maxBound :: Int) -> Right (VInt (fromInteger n))
    | otherwise -> Left ("integer out of 64-bit range: " ++ show n)
  EFloat d -> Right (VDouble d)
  EStr s   -> Right (VStr s)
  EBool b  -> Right (VBool b)
  EKw k    -> Right (VKw (Keyword k))
  ENil     -> Left "nil cannot be stored (omit the attribute instead)"
  _        -> Left ("not a scalar value: " ++ T.unpack (renderEdn e))
