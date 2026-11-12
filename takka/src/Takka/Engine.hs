{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Takka.Engine
-- Description : A database kept in an engine that runs inside this process.
--
-- This is the "Datalevin Native" slot of hosigraph: a Datalog database with Datalevin's shape
-- (EAV facts, EDN Datalog, pull), reached by function calls instead of through @dtlv@ or a JVM.
-- It is a thin face over the rest of takka (it is not Datalevin's code).  When Datalevin has a
-- native core of its own, this is the module that a replacement has to match.
--
-- Everything a khana needs from the engine is here.
module Takka.Engine
  ( Native, Db, NativeOp(..)
  , openNative, closeNative
  , nativeValues, nativeTransact, nativeDatoms, nativeDatomCount
  , nativeEntity, nativeEntities
  , nativeQuery, nativeQueryCells, nativeQueryMany, nativeQueryCellsMany
  , nativeWithDb
  ) where

import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Word (Word64)

import qualified Takka as H
import           Takka.Store (Conn, openConnWith, closeConn, defaultConnConfig, ccEnv, defaultEnvConfig, cfgNoSync)
import           Takka.Query (Cell, Input)
import           Takka.Types

-- | An open database.
newtype Native = Native Conn

-- | A consistent snapshot of the database (see 'nativeWithDb').
type Db = H.Db

-- | One fact to put in or take out.
data NativeOp
  = NAdd     !EntityId !Keyword !Value
  | NRetract !EntityId !Keyword !Value

-- | Open (creating if needed).  @noSync@: skip fsync on commit.
openNative :: FilePath -> Schema -> Bool -> IO Native
openNative dir schema noSync =
  Native <$> openConnWith defaultConnConfig { ccEnv = defaultEnvConfig { cfgNoSync = noSync } } dir schema

closeNative :: Native -> IO ()
closeNative (Native c) = closeConn c

-- | The values of one attribute of one entity.
nativeValues :: Native -> EntityId -> Keyword -> IO [Value]
nativeValues (Native c) e a = H.withDb c $ \db -> do
  ds <- H.datoms db (Just e) (Just a) Nothing
  pure [ v | Datom _ _ v <- ds ]

-- | Apply facts in one atomic transaction, in the order given.
nativeTransact :: Native -> [NativeOp] -> IO ()
nativeTransact (Native c) ops = void (H.transact c (map op ops))
  where
    op o = case o of
      NAdd e a v     -> H.Add     (H.EId e) a (H.TVal v)
      NRetract e a v -> H.Retract (H.EId e) a v

nativeDatoms :: Native -> IO [Datom]
nativeDatoms (Native c) = H.withDb c H.allDatoms

nativeDatomCount :: Native -> IO Word64
nativeDatomCount (Native c) = H.withDb c H.datomCount

nativeEntity :: Native -> EntityId -> IO (M.Map Keyword [Value])
nativeEntity (Native c) i = H.withDb c $ \db -> H.entity db i

-- | Many entities from one snapshot.
nativeEntities :: Native -> [EntityId] -> IO [M.Map Keyword [Value]]
nativeEntities (Native c) is = H.withDb c $ \db -> mapM (H.entity db) is

nativeQuery :: Native -> Text -> [Input] -> IO [[Value]]
nativeQuery (Native c) = H.q c

nativeQueryCells :: Native -> Text -> [Input] -> IO [[Cell]]
nativeQueryCells (Native c) = H.qCells c

-- | Many queries against one snapshot.
nativeQueryMany :: Native -> [(Text, [Input])] -> IO [[[Value]]]
nativeQueryMany (Native c) qs = H.withDb c $ \db -> forM qs (uncurry (H.qEdn db))

nativeQueryCellsMany :: Native -> [(Text, [Input])] -> IO [[[Cell]]]
nativeQueryCellsMany (Native c) qs = H.withDb c $ \db -> forM qs (uncurry (H.qCellsEdn db))

-- | Run something against one consistent snapshot, with the whole engine available.
nativeWithDb :: Native -> (Db -> IO a) -> IO a
nativeWithDb (Native c) = H.withDb c
