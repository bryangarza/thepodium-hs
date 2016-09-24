{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Podium.DB.Query where

import Podium.DB.Connect
import Podium.DB.Table.Account (AccountId)
import Podium.DB.Table.Post

import qualified Data.UUID as U
import Data.UUID
import Control.Arrow                   (returnA)
import Control.Lens                    (view)
import Control.Monad.Trans             (liftIO)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple      (Connection)
import Opaleye.PGTypes                 (pgUUID)
import Opaleye
  ( Query
  , restrict
  , (.==)
  , queryTable
  , runQuery
  , showSqlForPostgres
  , Unpackspec )

import Database.PostgreSQL.Simple (Connection)
import Data.Pool

postQuery :: Query ColumnR
postQuery = queryTable table

postByAccountId :: AccountId -> Query ColumnR
postByAccountId idToMatch = proc () -> do
  row <- postQuery -< ()
  restrict -< (view accountId row) .== pgUUID idToMatch
  returnA -< row

runPostByAccountId :: Pool Connection -> IO [Post]
runPostByAccountId =
  liftIO . flip withResource (\c -> runQuery c $ postByAccountId U.nil)

-- printSql :: Default Unpackspec a a => Query a -> IO ()
-- printSql = putStrLn . showSqlForPostgres
