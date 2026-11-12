{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Takka
-- Description : A Datalog database for Haskell, built on dlmdb (Datalevin's LMDB fork).
--
-- > import Takka
-- > import qualified Data.Map as M
-- >
-- > main = withConn "/tmp/mydb" (M.fromList [("person/name", identity (attr TString))]) $ \conn -> do
-- >   _ <- transactEdn conn "[{:person/name \"Frege\" :person/age 70}]"
-- >   q conn "[:find ?n :where [?e :person/name ?n]]" [] >>= print
module Takka
  ( -- * Connection
    Conn, openConn, openConnWith, closeConn, withConn, withConnWith
  , ConnConfig(..), defaultConnConfig
  , EnvConfig(..), defaultEnvConfig
    -- * Values, datoms and schema
  , module Takka.Types
  , updateSchema, getSchema
    -- * Writing
  , EntRef(..), TxVal(..), TxOp(..), TxReport(..)
  , transact, transactEdn
    -- * Reading
  , Db, withDb, datoms, entity, lookupRef, allDatoms
  , countDatoms, datomCount, distinctEstimate
  , Bound(..), datomsInRange, countInRange
    -- * Datalog
  , Input(..), Rule(..), Query, Cell(..)
  , q, qCells, qEdn, qCellsEdn, explainEdn, runQuery, runQueryCells, parseQuery, rulesEdn
    -- * Pull
  , Pulled(..), PullItem(..), PullAttr(..), Recur(..), Pattern
  , parsePull, parsePullText, pull, pullMany, pullLookup, pullEdn
  , renderPulled, pulledToJSON
    -- * JSON-LD
  , module Takka.JsonLD
  ) where

import           Control.Exception (throwIO)
import           Data.Text (Text)

import           Takka.JsonLD
import           Takka.Pull
import           Takka.Query
import           Takka.Store
import           Takka.TxEdn
import           Takka.Types

-- | Transact tx-data written as EDN.
transactEdn :: Conn -> Text -> IO TxReport
transactEdn conn txt = either (throwIO . ParseError) (transact conn) (parseTxText txt)

-- | Run an EDN query against a fresh snapshot of the connection.
q :: Conn -> Text -> [Input] -> IO [[Value]]
q conn txt inputs = withDb conn $ \db -> qEdn db txt inputs

-- | Like 'q', but @(pull ?e [pattern])@ may appear in @:find@.
qCells :: Conn -> Text -> [Input] -> IO [[Cell]]
qCells conn txt inputs = withDb conn $ \db -> qCellsEdn db txt inputs

-- | Pull one entity with a pattern written as EDN.
pullEdn :: Db -> Text -> EntityId -> IO Pulled
pullEdn db txt e = either (throwIO . ParseError) (\pat -> pull db pat e) (parsePullText txt)

-- | Parse rules written as EDN (pass them as @InRules@ with @%@ in @:in@).
rulesEdn :: Text -> IO [Rule]
rulesEdn txt = either (throwIO . ParseError) pure (parseRulesText txt)
