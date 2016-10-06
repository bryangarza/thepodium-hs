{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Podium.DB.Query where

import Podium.DB.Util
import Podium.DB.Connect
import Podium.DB.Table.Account as A
import Podium.DB.Table.Post    as P

import qualified Data.UUID as U
import Data.UUID
import Control.Arrow                   (returnA)
import Control.Lens                    (view)
import Control.Lens.Getter             (Getter, Getting)
import Control.Monad.Trans.Reader      (ask)
import Control.Monad.Trans.Class       (lift)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple      (Connection)
import Opaleye.PGTypes                 (pgUUID)
import Opaleye
  ( Column
  , Query
  , restrict
  , (.==)
  , PGUuid
  , queryTable
  , runQuery
  , showSqlForPostgres
  , Unpackspec
  , Table(Table) )
import Opaleye.Internal.TableMaker (ColumnMaker)
import Opaleye.Internal.RunQuery (QueryRunner)

import Database.PostgreSQL.Simple (Connection)
import Data.Pool

-- postQuery :: Query ColumnR
-- postQuery = queryTable table

byId :: Default ColumnMaker s s => Table a s -> Getter s (Column PGUuid) -> UUID -> Query s
byId qTable field idToMatch = proc () -> do
  row <- (queryTable qTable) -< ()
  restrict -< (view field row) .== pgUUID idToMatch
  returnA -< row

q :: Default QueryRunner columns haskells => Query columns -> Database [haskells]
q query = lift . flip withResource (\conn -> runQuery conn query) =<< ask

accountByAccountId :: AccountId -> Query A.ColumnR
accountByAccountId = byId A.table A.accountId

postByAccountId :: AccountId -> Query P.ColumnR
postByAccountId = byId P.table P.accountId

runPostByAccountId :: AccountId -> Database [Post]
runPostByAccountId = q . postByAccountId

runAccountByAccountId :: AccountId -> Database [Account]
runAccountByAccountId = q . accountByAccountId

-- printSql :: Default Unpackspec a a => Query a -> IO ()
-- printSql = putStrLn . showSqlForPostgres
