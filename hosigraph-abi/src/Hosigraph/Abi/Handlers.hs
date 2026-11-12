{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Abi.Handlers
-- Description : One handler per operation of "Hosigraph.Abi.Api": decode the arguments, call hosigraph, encode the answer.
--
-- The handlers speak to a 'Backend', a record of the functions of "Hosigraph" that they use.  The
-- real one is 'realBackend'; a test can put its own in its place, which is how the operations are
-- tried here without a Datalevin to run them against.
module Hosigraph.Abi.Handlers
  ( Backend(..), realBackend, unattached
  , handlers
  , guarded
  ) where

import qualified Control.Exception as E
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import           Hosigraph
import           Hosigraph.Abi.Codec
import           Setu.Codec (ToEdn(..))
import           Setu.Dispatch (Failure(..), Handler)
import           Setu.Ticket (Status(..))
import           Takka.Edn (Edn(..))
import           Takka.Types (TakkaError(..))

-- | The functions of "Hosigraph" that the operations use, without the 'Hosi' they work on.
data Backend = Backend
  { bWrite          :: [Op] -> IO WriteReport
  , bWriteMany      :: [[Op]] -> IO [Either HosiError WriteReport]
  , bSeal           :: IO (Maybe SealReport)
  , bGetEntities    :: [HosiId] -> IO [M.Map Keyword [Value]]
  , bQueryMany      :: [(Text, [Input])] -> IO [[[Value]]]
  , bQueryCellsMany :: [(Text, [Input])] -> IO [[[Cell]]]
  , bHistory        :: HosiId -> IO [Event]
  , bHistoryTrace   :: HosiId -> IO ([Event], LookupTrace)
  , bVerify         :: IO [String]
  , bStats          :: IO Stats
  , bRebuildKhana   :: IO ()
  }

realBackend :: Hosi -> Backend
realBackend h = Backend
  { bWrite = write h, bWriteMany = writeMany h, bSeal = seal h
  , bGetEntities = getEntities h, bQueryMany = queryMany h, bQueryCellsMany = queryCellsMany h
  , bHistory = history h, bHistoryTrace = historyTrace h
  , bVerify = verify h, bStats = stats h, bRebuildKhana = rebuildKhana h }

-- | The backend of a connection that was opened without a database: every operation is refused
-- (the reserved @ping@ and @abi@ are the dispatcher's, and still answer).
unattached :: Backend
unattached = Backend
  { bWrite = no1, bWriteMany = no1, bSeal = no0, bGetEntities = no1, bQueryMany = no1
  , bQueryCellsMany = no1, bHistory = no1, bHistoryTrace = no1
  , bVerify = no0, bStats = no0, bRebuildKhana = no0 }
  where
    no0 :: IO b
    no0 = E.throwIO (BadArgument "this connection has no database: open was not given :dir")
    no1 :: a -> IO b
    no1 _ = no0

-- | The handlers, by the names of the operations in the 'Hosigraph.Abi.Api.api'.
handlers :: Backend -> [(Text, Handler)]
handlers b =
  [ ("write",            run decOps      (bWrite b)          toEdn)
  , ("write-many",       run decBatches  (bWriteMany b)      (EVec . map encWriteResult))
  , ("seal",             run none        (const (bSeal b))   toEdn)
  , ("get-entities",     run decIds      (bGetEntities b)    toEdn)
  , ("query-many",       run decQueries  (bQueryMany b)      toEdn)
  , ("query-cells-many", run decQueries  (bQueryCellsMany b) toEdn)
  , ("history",          run decId       (bHistory b)        toEdn)
  , ("history-trace",    run decId       (bHistoryTrace b)   encTrace)
  , ("verify",           run none        (const (bVerify b)) toEdn)
  , ("stats",            run none        (const (bStats b))  toEdn)
  , ("rebuild-khana",    run none        (const (bRebuildKhana b)) (const ENil))
  ]
  where none _ = Right ()

-- | A handler from its three parts. A decoder that refuses what the 'Setu.Spec.Ty' let through is
-- our mistake, and is told as one.
run :: (Edn -> Either Text a) -> (a -> IO r) -> (r -> Edn) -> Handler
run dec act enc args = case dec args of
  Left why -> pure (Left (Failure StInternal "decode" ("the arguments passed their shape but could not be read: " <> why)))
  Right a  -> fmap enc <$> guarded (act a)

-- | What hosigraph and takka throw on purpose is the library's own refusal (the status
-- @HOSI_ERR_APP@, with the kind); anything else is left to the dispatcher (an I/O error, or the
-- library's own mistake).
guarded :: IO a -> IO (Either Failure a)
guarded act = (Right <$> act) `E.catches`
  [ E.Handler (\(e :: HosiError) -> let (k, m) = errorKind e in pure (Left (Failure StApp k (T.pack m))))
  , E.Handler (\(e :: TakkaError) -> pure (Left (takka e)))
  ]
  where
    takka e = case e of
      TxError m     -> Failure StApp "tx-error" (T.pack m)
      QueryError m  -> Failure StApp "query-error" (T.pack m)
      SchemaError m -> Failure StApp "schema-error" (T.pack m)
      ParseError m  -> Failure StApp "parse-error" (T.pack m)
