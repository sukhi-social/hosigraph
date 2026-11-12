-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Tests of "Setu.Dispatch": bytes in, bytes out, and who is blamed.
module DispatchTests (dispatchTests) where

import           Control.Concurrent
import           Control.Exception hiding (Handler)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Harness
import           Setu.Dispatch
import           Setu.Spec
import           Setu.Ticket
import           Setu.Wire
import           Takka.Edn (Edn(..), readEdn)

-- | A small API with one operation of each kind of behaviour.
api :: Api
api = Api
  { apiName = "demo", apiVersion = 3, apiAppErr = "APP", apiAppErrDoc = "the library said no"
  , apiOps =
      [ Op "add" "Adds." (TMap [Field "a" TInt, Field "b" TInt]) TInt
      , Op "refuse" "Always says no." TNil TNil
      , Op "disk" "Fails the way I/O fails." TNil TNil
      , Op "crash" "Throws." TNil TNil
      , Op "liar" "Gives a result of the wrong shape." TNil TInt
      , Op "bad-failure" "Fails with the status ok." TNil TNil
      , Op "echo-str" "Echoes a string." TStr TStr
      ]
  }

handlers :: [(Text, Handler)]
handlers =
  [ ("add", \a -> pure $ case a of
        EMap ps | Just (EInt x) <- lookup (EKw "a") ps, Just (EInt y) <- lookup (EKw "b") ps -> Right (EInt (x + y))
        _ -> Left (Failure StInternal "handler-bug" "the arguments passed the check but are not a and b"))
  , ("refuse",      \_ -> pure (Left (Failure StApp "write-rejected" "no, thank you")))
  , ("disk",        \_ -> throwIO (userError "disk is gone"))
  , ("crash",       \_ -> throwIO (ErrorCall "boom"))
  , ("liar",        \_ -> pure (Right (EStr "not an integer")))
  , ("bad-failure", \_ -> pure (Left (Failure StOk "x" "y")))
  , ("echo-str",    pure . Right)
  ]

dispatch :: Dispatch
dispatch = either (error . show) id (newDispatch api handlers)

-- | Ask in EDN text, hear the status and the answer read back.
ask :: Text -> IO (Status, Either WireError Reply)
ask req = askBytes (TE.encodeUtf8 req)

askBytes :: ByteString -> IO (Status, Either WireError Reply)
askBytes bs = do
  (st, body) <- runRequest dispatch bs
  pure (st, decodeReply body)

-- | The status and the kind of the error (the message is for people).
shape :: (Status, Either WireError Reply) -> (Status, Either WireError Text)
shape (st, r) = (st, fmap f r)
  where f (ReplyOk v) = "ok " <> T.pack (show v)
        f (ReplyErr k _) = k

dispatchTests :: [Test]
dispatchTests =
  [ ("dispatch: ping gives back its arguments, whatever they are", do
      r <- ask "{:op :ping :args {:x [1 2.5 \"日本語\" :k]}}"
      checkEq "ping" (StOk, Right (ReplyOk (EMap [(EKw "x", EVec [EInt 1, EFloat 2.5, EStr "日本語", EKw "k"])]))) r
      r2 <- ask "{:op :ping}"
      checkEq "no arguments is nil" (StOk, Right (ReplyOk ENil)) r2)

  , ("dispatch: abi names the API, its version and every operation, the reserved ones first", do
      r <- ask "{:op :abi}"
      checkEq "abi" (StOk, Right (ReplyOk (EMap
        [ (EKw "name", EStr "demo"), (EKw "version", EInt 3)
        , (EKw "ops", EVec (map EKw ["ping", "abi", "add", "refuse", "disk", "crash", "liar", "bad-failure", "echo-str"])) ]))) r)

  , ("dispatch: an operation of the API", do
      r <- ask "{:op :add :args {:a 2 :b 40}}"
      checkEq "add" (StOk, Right (ReplyOk (EInt 42))) r)

  , ("dispatch: non-ASCII text goes through the whole way, bytes in and bytes out", do
      let s = "日本語 \"quoted\" \\ \n tab\t ✓ 𝒳"
      (st, body) <- runRequest dispatch (encodeRequest (Request "echo-str" (EStr s)))
      checkEq "status" StOk st
      checkEq "answer" (Right (ReplyOk (EStr s))) (decodeReply body))

  , ("dispatch: what is not the contract is a protocol error, and says which part", do
      let proto k = (StProtocol, Right k)
      -- not UTF-8
      r1 <- askBytes (B.pack [0x7b, 0xff, 0xfe, 0x7d])
      checkEq "not utf-8" (proto "bad-request") (shape r1)
      -- not EDN
      r2 <- ask "{:op :ping"
      checkEq "not edn" (proto "bad-request") (shape r2)
      -- not an envelope
      r3 <- ask "[1 2 3]"
      checkEq "not a map" (proto "bad-request") (shape r3)
      r4 <- ask "{:args 1}"
      checkEq "no op" (proto "bad-request") (shape r4)
      r5 <- ask "{:op :ping :args 1 :extra 2}"
      checkEq "unknown envelope key" (proto "bad-request") (shape r5)
      -- an operation nobody offers
      r6 <- ask "{:op :nope}"
      checkEq "unknown op" (proto "unknown-op") (shape r6)
      -- arguments of the wrong shape, with the path to the mistake
      r7 <- ask "{:op :add :args {:a 1 :b \"two\"}}"
      checkEq "wrong shape" (proto "bad-args") (shape r7)
      case snd r7 of
        Right (ReplyErr _ msg) -> check ("the message tells where: " ++ T.unpack msg) ("[:args :b]" `T.isInfixOf` msg)
        other -> throwIO (userError ("unexpected " ++ show other))
      r8 <- ask "{:op :add :args {:a 1 :b 2 :c 3}}"
      checkEq "unknown argument key" (proto "bad-args") (shape r8)
      r9 <- ask "{:op :add}"
      checkEq "missing arguments" (proto "bad-args") (shape r9))

  , ("dispatch: the library's own refusal keeps its kind and message", do
      r <- ask "{:op :refuse}"
      checkEq "refuse" (StApp, Right (ReplyErr "write-rejected" "no, thank you")) r)

  , ("dispatch: an I/O exception is an I/O error; any other exception is the library's own mistake (T-11)", do
      r1 <- ask "{:op :disk}"
      checkEq "io" (StIo, Right "io") (shape r1)
      r2 <- ask "{:op :crash}"
      checkEq "exception" (StInternal, Right "exception") (shape r2))

  , ("dispatch: a result of the wrong shape is the library's mistake, not the caller's", do
      r <- ask "{:op :liar}"
      checkEq "bad result" (StInternal, Right "bad-result") (shape r)
      r2 <- ask "{:op :bad-failure}"
      checkEq "a failure that is ok" (StInternal, Right "bad-failure") (shape r2))

  , ("dispatch: newDispatch says what is missing, what is extra, and what is wrong with the API", do
      let h = pure . Right
          names = map fst handlers
      checkEq "complete" True (either (const False) (const True) (newDispatch api handlers))
      case newDispatch api (drop 1 handlers) of
        Left ps -> checkEq "missing" ["no handler for operation \"add\""] ps
        Right _ -> throwIO (userError "a missing handler should be refused")
      case newDispatch api (("surplus", h) : handlers) of
        Left ps -> checkEq "extra" ["a handler for \"surplus\", which the API does not list"] ps
        Right _ -> throwIO (userError "an extra handler should be refused")
      case newDispatch api (("ping", h) : handlers) of
        Left ps -> checkEq "reserved" ["a handler for \"ping\", which the API does not list"] ps
        Right _ -> throwIO (userError "a handler for a reserved operation should be refused")
      case newDispatch api ((head names, h) : handlers) of
        Left ps -> checkEq "twice" ["two handlers for operation \"add\""] ps
        Right _ -> throwIO (userError "two handlers should be refused")
      case newDispatch api { apiName = "Bad Name" } handlers of
        Left ps -> check "the API itself is checked" (not (null ps))
        Right _ -> throwIO (userError "a bad API should be refused"))

  , ("dispatch: through a ticket table, bytes in a submit come out of the callback as the answer", do
      t <- newTable defaultConfig
      got <- newEmptyMVar
      r <- submit t 1 (runRequest dispatch (TE.encodeUtf8 "{:op :add :args {:a 1 :b 2}}")) (\i st b -> putMVar got (i, st, b))
      checkEq "accepted" (Right ()) r
      (i, st, b) <- patiently "the answer" (takeMVar got)
      close t
      checkEq "id" 1 i
      checkEq "status" StOk st
      checkEq "answer" (Right (ReplyOk (EInt 3))) (decodeReply b)
      -- and the reader of the whole thing agrees with what is written
      checkEq "readable" (Right (EMap [(EKw "ok", EInt 3)])) (readEdn (TE.decodeUtf8 b)))
  ]
