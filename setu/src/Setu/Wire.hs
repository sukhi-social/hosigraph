-- SPDX-License-Identifier: BlueOak-1.0.0
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
  ) where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Takka.Edn (Edn(..), readEdn, renderEdn)

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
encodeRequest (Request op args) = TE.encodeUtf8 . renderEdn . EMap $
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
encodeReply r = TE.encodeUtf8 . renderEdn . EMap $ case r of
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
