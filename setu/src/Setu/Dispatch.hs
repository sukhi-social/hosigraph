{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Setu.Dispatch
-- Description : From the bytes of a request to the bytes of an answer: the envelope, the shape of
--               the arguments, the handler of the operation, the shape of the result.
--
-- This is the middle of the library, and every door shares it: the C ABI hands it the body of a
-- 'Setu.Ticket.submit', and a pipe would hand it a frame.  It reads the envelope
-- ("Setu.Wire"), finds the operation in the 'Api', checks the arguments against the 'Ty' the
-- 'Api' gives, runs the handler, checks what the handler gave back, and writes the answer.
--
-- Who is blamed is told by the status:
--
--  * a request that is not the contract ('StProtocol'): not UTF-8, not EDN, not an envelope, an
--    operation nobody offers, arguments of the wrong shape;
--  * the library's own refusal ('StApp'), or an I/O failure ('StIo'): the handler says so;
--  * the library's own mistake ('StInternal'): an exception, or a result that is not the shape the
--    'Api' promised.  (This is why the result is checked too: a promise the generated code makes
--    to Go or Elixir should not be broken by a handler that forgot a key.)
module Setu.Dispatch
  ( Failure(..), Handler
  , Dispatch, newDispatch, dispatchApi
  , runRequest
  ) where

import           Control.Exception hiding (Handler)
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import           Setu.Spec
import           Setu.Ticket (Status(..), isKill)
import           Setu.Validate
import           Setu.Wire
import           Takka.Edn (Edn(..))

-- | What a handler says when it cannot give a result.  The status must not be 'StOk'.
data Failure = Failure
  { failStatus  :: Status
  , failKind    :: Text     -- ^ a keyword name, kebab case: @"write-rejected"@
  , failMessage :: Text
  } deriving (Eq, Show)

-- | The arguments have already been checked against the operation's 'Ty'.
type Handler = Edn -> IO (Either Failure Edn)

data Dispatch = Dispatch
  { dispatchApi :: Api
  , dHandlers   :: M.Map Text Handler
  }

-- | Pair an 'Api' with the handlers of its operations.  Every operation the 'Api' lists needs
-- one (the reserved ones, @ping@ and @abi@, are done here), and a handler for something the
-- 'Api' does not list is a mistake worth knowing at start.
newDispatch :: Api -> [(Text, Handler)] -> Either [Text] Dispatch
newDispatch api hs
  | not (null problems) = Left problems
  | otherwise           = Right (Dispatch api (M.fromList (reserved ++ hs)))
  where
    own      = map opName (apiOps api)
    given    = map fst hs
    problems = checkApi api
      ++ [ "no handler for operation \"" <> n <> "\"" | n <- own, n `notElem` given ]
      ++ [ "a handler for \"" <> n <> "\", which the API does not list" | n <- given, n `notElem` own ]
      ++ [ "two handlers for operation \"" <> n <> "\"" | (n, c) <- M.toList (M.fromListWith (+) [(n, 1 :: Int) | n <- given]), c > 1 ]
    reserved =
      [ ("ping", pure . Right)
      , ("abi", \_ -> pure . Right . EMap $
          [ (EKw "name",    EStr (apiName api))
          , (EKw "version", EInt (toInteger (apiVersion api)))
          , (EKw "ops",     EVec [EKw (opName o) | o <- allOps api])
          ])
      ]

-- | One request, one answer.  Never throws (an exception thrown /to/ the thread excepted).
runRequest :: Dispatch -> ByteString -> IO (Status, ByteString)
runRequest d body = case decodeRequest body of
  Left e -> pure (refuse "bad-request" (renderWireError e))
  Right (Request name args) -> case lookupOp name of
    Nothing -> pure (refuse "unknown-op" ("no operation named :" <> name))
    Just (op, h) -> case validateAt [PKey "args"] (opArgs op) args of
      Left e  -> pure (refuse "bad-args" (renderTyError e))
      Right _ -> do
        r <- try (h args)
        case r of
          Right (Right v) -> pure $ case validateAt [PKey "ok"] (opResult op) v of
            Right _ -> (StOk, encodeReply (ReplyOk v))
            Left e  -> broke "bad-result" ("the handler of :" <> name <> " gave a result of the wrong shape: " <> renderTyError e)
          Right (Left (Failure st kind msg))
            | st == StOk -> pure (broke "bad-failure" ("the handler of :" <> name <> " failed with the status ok"))
            | otherwise  -> pure (st, encodeReply (ReplyErr kind msg))
          Left (e :: SomeException)
            | isKill e -> throwIO e
            | Just (ie :: IOException) <- fromException e ->
                pure (StIo, encodeReply (ReplyErr "io" (T.pack (show ie))))
            | otherwise -> pure (broke "exception" ("an exception in :" <> name <> ": " <> T.pack (show e)))
  where
    lookupOp n = do
      op <- lookup n [(opName o, o) | o <- allOps (dispatchApi d)]
      h  <- M.lookup n (dHandlers d)
      pure (op, h)
    refuse k m = (StProtocol, encodeReply (ReplyErr k m))
    broke  k m = (StInternal, encodeReply (ReplyErr k m))
