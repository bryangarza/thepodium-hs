module Podium.DB.Util where

import Podium.DB.Connect

import Crypto.PasswordStore (makePassword)
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Connection, close)

import Control.Monad.Trans.Reader
import Data.Pool

-- Performs a database action by using a pooled connection in the environment
type Database a = ReaderT (Pool Connection) IO a

genUuid :: IO UUID
genUuid = nextRandom

genPassword :: ByteString -> IO ByteString
genPassword x = makePassword x 18
