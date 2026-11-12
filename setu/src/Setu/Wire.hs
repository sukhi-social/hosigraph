{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Setu.Wire
-- Description : The envelope around every request and every answer (UTF-8 EDN text, T-12).
--
-- > request: {:op :write :args <edn>}          (:args may be left out when it is nil)
-- > answer:  {:ok <edn>}  or  {:error :kind :message "..."}
--
-- The envelope is closed: a key other than the ones above is refused.  The status of the C ABI
-- says how a call ended; the envelope says it too, so every driver (C, a pipe, ...) shares it.
module Setu.Wire
  ( Request(..), Reply(..)
  , WireError(..), renderWireError
  , encodeRequest, decodeRequest
  , encodeReply, decodeReply
  , writeEdn
  ) where

import           Data.ByteString (ByteString)
import           Data.Char (ord)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Numeric (showHex)

import           Takka.Edn (Edn(..), readEdn)

data Request = Request
  { reqOp   :: Text   -- ^ the operation's name, without the colon
  , reqArgs :: Edn    -- ^ 'ENil' when the request has none
  } deriving (Eq, Show)

data Reply
  = ReplyOk Edn
  | ReplyErr Text Text   -- ^ the kind (a keyword name, without the colon), and a message for people
  deriving (Eq, Show)

-- | Why some bytes are not an envelope.
data WireError
  = NotUtf8
  | NotEdn String
  | BadEnvelope Text
  deriving (Eq, Show)

renderWireError :: WireError -> Text
renderWireError e = case e of
  NotUtf8       -> "the body is not valid UTF-8"
  NotEdn why    -> "the body is not EDN: " <> T.pack why
  BadEnvelope m -> m

encodeRequest :: Request -> ByteString
encodeRequest (Request op args) = TE.encodeUtf8 . writeEdn . EMap $
  (EKw "op", EKw op) : [(EKw "args", args) | args /= ENil]

decodeRequest :: ByteString -> Either WireError Request
decodeRequest bs = do
  e <- decodeBody bs
  ps <- asMap "request" e
  checkKeys "request" ["op", "args"] ps
  op <- case lookupKw "op" ps of
    Just (EKw n) -> Right n
    Just _       -> Left (BadEnvelope "request: :op must be a keyword")
    Nothing      -> Left (BadEnvelope "request: :op is missing")
  Right (Request op (maybe ENil id (lookupKw "args" ps)))

encodeReply :: Reply -> ByteString
encodeReply r = TE.encodeUtf8 . writeEdn . EMap $ case r of
  ReplyOk v        -> [(EKw "ok", v)]
  ReplyErr kind m  -> [(EKw "error", EKw kind), (EKw "message", EStr m)]

decodeReply :: ByteString -> Either WireError Reply
decodeReply bs = do
  e <- decodeBody bs
  ps <- asMap "answer" e
  case (lookupKw "ok" ps, lookupKw "error" ps) of
    (Just v, Nothing) -> do
      checkKeys "answer" ["ok"] ps
      Right (ReplyOk v)
    (Nothing, Just k) -> do
      checkKeys "answer" ["error", "message"] ps
      kind <- case k of
        EKw n -> Right n
        _     -> Left (BadEnvelope "answer: :error must be a keyword")
      msg <- case lookupKw "message" ps of
        Just (EStr m) -> Right m
        _             -> Left (BadEnvelope "answer: :message must be a string")
      Right (ReplyErr kind msg)
    (Just _, Just _)  -> Left (BadEnvelope "answer: both :ok and :error are present")
    (Nothing, Nothing) -> Left (BadEnvelope "answer: neither :ok nor :error is present")

decodeBody :: ByteString -> Either WireError Edn
decodeBody bs = case TE.decodeUtf8' bs of
  Left _  -> Left NotUtf8
  Right t -> either (Left . NotEdn) Right (readEdn t)

asMap :: Text -> Edn -> Either WireError [(Edn, Edn)]
asMap what e = case e of
  EMap ps -> Right ps
  _       -> Left (BadEnvelope (what <> " must be a map"))

lookupKw :: Text -> [(Edn, Edn)] -> Maybe Edn
lookupKw k ps = lookup (EKw k) ps

-- | Only these keys, each at most once.
checkKeys :: Text -> [Text] -> [(Edn, Edn)] -> Either WireError ()
checkKeys what allowed ps = mapM_ one (zip [0 :: Int ..] (map fst ps))
  where
    one (i, k) = case k of
      EKw n
        | n `notElem` allowed -> Left (BadEnvelope (what <> ": unknown key :" <> n))
        | n `elem` [m | EKw m <- take i (map fst ps)] -> Left (BadEnvelope (what <> ": key :" <> n <> " is given twice"))
        | otherwise -> Right ()
      _ -> Left (BadEnvelope (what <> ": keys must be keywords"))

-- | EDN as text that 'Takka.Edn.readEdn' reads back to the same value.
--
-- (Not 'Takka.Edn.renderEdn': that one is for diagnostics and writes strings the way Haskell's
-- 'show' does, so a non-ASCII character comes out as @\\26085@, which no EDN reader understands.)
-- Characters are written as they are, UTF-8 being the encoding of the body; only @"@, @\\@ and
-- control characters are escaped.  A float that is not finite is written the way Clojure writes
-- it (@##NaN@, @##Inf@, @##-Inf@); takka's reader does not read those, which is why
-- "Setu.Validate" refuses them for 'Setu.Spec.TDouble'.
writeEdn :: Edn -> Text
writeEdn e = case e of
  ENil      -> "nil"
  EBool b   -> if b then "true" else "false"
  EInt n    -> T.pack (show n)
  EFloat d
    | isNaN d      -> "##NaN"
    | isInfinite d -> if d > 0 then "##Inf" else "##-Inf"
    | otherwise    -> T.pack (show d)
  EStr s    -> "\"" <> T.concatMap escape s <> "\""
  EKw k     -> T.cons ':' k
  ESym s    -> s
  EVec xs   -> wrap "[" "]" xs
  EList xs  -> wrap "(" ")" xs
  ESet xs   -> wrap "#{" "}" xs
  EMap ps   -> wrap "{" "}" (concat [[k, v] | (k, v) <- ps])
  where
    wrap o c xs = o <> T.unwords (map writeEdn xs) <> c

escape :: Char -> Text
escape c = case c of
  '"'  -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | c < ' ' || c == '\DEL' -> T.pack ("\\u" ++ pad (showHex (ord c) ""))
    | otherwise              -> T.singleton c
  where pad s = replicate (4 - length s) '0' ++ s
