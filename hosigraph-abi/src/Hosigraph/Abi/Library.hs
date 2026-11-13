-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Hosigraph.Abi.Library
-- Description : hosigraph as a 'Setu.Abi.Library': how @open@ starts it, and what it releases.
module Hosigraph.Abi.Library
  ( library
  , parseOpen, OpenArgs(..)
  ) where

import qualified Control.Exception as E
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import           Hosigraph
import           Hosigraph.Abi.Api
import           Hosigraph.Abi.Handlers
import           Setu.Abi (Library(..))
import           Setu.Dispatch (Dispatch, Failure(..), newDispatch)
import           Setu.Ticket (Status(..))
import           Setu.Validate (renderTyError, validate)
import           Takka.Edn (Edn(..), readEdn)
import           Takka.Types (AttrSchema(..), Cardinality(..), Unique(..))

-- | What @open@ asked for, once read.
data OpenArgs = OpenArgs
  { oaDir    :: Maybe FilePath   -- ^ 'Nothing': open no database
  , oaSchema :: Schema
  , oaConfig :: HosiConfig
  }

library :: Library
library = Library
  { libApi     = api
  , libLimit   = 1024     -- requests waiting on one connection
  , libWorkers = 4        -- requests running at once; writes take turns inside hosigraph in any case
  , libConnect = connect
  }

connect :: Text -> IO (Either Failure (Dispatch, IO ()))
connect txt = case parseOpen txt of
  Left why -> pure (Left (Failure StProtocol "bad-open" why))
  Right (OpenArgs Nothing _ _) -> pure (dispatchOf unattached (pure ()))
  Right (OpenArgs (Just dir) schema cfg) -> do
    r <- guarded (openHosiWith cfg dir schema) `E.catch` \(e :: E.IOException) ->
           pure (Left (Failure StIo "io" (T.pack (show e))))
    case r of
      Left f  -> pure (Left f)
      Right h -> case dispatchOf (realBackend h) (closeHosi h) of
        Left f  -> closeHosi h >> pure (Left f)
        Right x -> pure (Right x)
  where
    dispatchOf :: Backend -> IO () -> Either Failure (Dispatch, IO ())
    dispatchOf b release = case newDispatch api (handlers b) of
      Left ps -> Left (Failure StInternal "bad-api" (T.unwords ps))
      Right d -> Right (d, release)

-- | Read the text @open@ was given: EDN of the shape 'openTy'.
parseOpen :: Text -> Either Text OpenArgs
parseOpen txt = do
  e <- either (Left . ("the open argument is not EDN: " <>) . T.pack) Right (readEdn txt)
  either (Left . renderTyError) Right (validate openTy e)
  let dir = case field "dir" e of
        EStr d -> Just (T.unpack d)
        _      -> Nothing
  schema <- case field "schema" e of
    ENil    -> Right M.empty
    EMap ps -> M.fromList <$> mapM attrSchema ps
    _       -> Left "bad :schema"
  cfg <- config (field "config" e)
  pure (OpenArgs dir schema cfg)

attrSchema :: (Edn, Edn) -> Either Text (Keyword, AttrSchema)
attrSchema (EKw k, s) = do
  t <- case field "type" s of
    ENil          -> Right TAny
    EKw "any"     -> Right TAny
    EKw "string"  -> Right TString
    EKw "long"    -> Right TLong
    EKw "double"  -> Right TDouble
    EKw "boolean" -> Right TBool
    EKw "keyword" -> Right TKeyword
    EKw "ref"     -> Right TRef
    _             -> Left "bad :type"
  c <- case field "card" s of
    ENil        -> Right One
    EKw "one"   -> Right One
    EKw "many"  -> Right Many
    _           -> Left "bad :card"
  u <- case field "unique" s of
    ENil          -> Right NotUnique
    EKw "identity" -> Right UniqueIdentity
    EKw "value"   -> Right UniqueValue
    _             -> Left "bad :unique"
  pure (Keyword k, AttrSchema t c u)
attrSchema _ = Left "bad :schema key"

config :: Edn -> Either Text HosiConfig
config c = do
  khana <- case field "khana" c of
    ENil                                   -> Right (hcKhanaBackend defaultConfig)
    EVec [EKw "dtlv", EStr exe]            -> Right (KhanaDtlv (T.unpack exe))
    EVec [EKw "jvm", EStr java, EStr jar]  -> Right (KhanaDatalevinJvm (T.unpack java) (T.unpack jar))
    _                                      -> Left "bad :khana"
  pure defaultConfig
    { hcSealEvents  = int "seal-events" (hcSealEvents defaultConfig)
    , hcSealAgeMs   = case field "seal-age-ms" c of { EInt n -> Just n; _ -> hcSealAgeMs defaultConfig }
    , hcBlockEvents = int "block-events" (hcBlockEvents defaultConfig)
    , hcColdBlockEvents = int "cold-block-events" (hcColdBlockEvents defaultConfig)
    , hcColdAfterMs = case field "cold-after-ms" c of { EInt n -> Just n; _ -> hcColdAfterMs defaultConfig }
    , hcSync        = case field "sync" c of { EBool b -> b; _ -> hcSync defaultConfig }
    , hcKhanaBackend = khana
    , hcReaderCacheBytes = int "reader-cache-bytes" (hcReaderCacheBytes defaultConfig)
    }
  where
    int k d = case field k c of { EInt n -> fromInteger n; _ -> d }

field :: Text -> Edn -> Edn
field k e = case e of
  EMap ps -> maybe ENil id (lookup (EKw k) ps)
  _       -> ENil
