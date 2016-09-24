module Podium.DB.Connect (mkPooledConnection) where

import Database.PostgreSQL.Simple
import Data.Pool

myConnectInfo :: ConnectInfo
myConnectInfo = ConnectInfo
  { connectHost     = "127.0.0.1"
  , connectPort     = 5432
  , connectUser     = "bryangarza"
  , connectPassword = ""
  , connectDatabase = "qwu"
  }

mkPooledConnection :: IO (Pool Connection)
mkPooledConnection =
  createPool (connect myConnectInfo) close 1 0.5 1
