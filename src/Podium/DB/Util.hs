module Podium.DB.Util where

import Podium.DB.Connect

import Crypto.PasswordStore (makePassword)
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Connection, close)

--import Control.Monad.Managed
import Control.Monad.Trans.Reader
import Data.Pool

-- Performs a database action by using a pooled connection in the environment
type Database a = ReaderT (Pool Connection) IO a

genUuid :: IO UUID
genUuid = nextRandom

genPassword :: ByteString -> IO ByteString
genPassword x = makePassword x 18

-- data Connected a where
--   Function :: (a -> Connection -> IO a


-- withResource pool (\conn -> runUpdate conn xp1 p2 p3)     :: IO Int64
-- --withConn :: (Connection -> a) -> a
-- withConn f = do
--   conn <- pConnect
--   f conn
--   close conn

-- connToLast :: (Connection -> a -> b -> c -> IO d) -> (a -> b -> c -> Connection -> IO d)
-- connToLast f = \a b c conn -> f conn a b c

-- \ a b c -> withConnection >>= ((connToLast runUpdate) a b c)


-- runWithConn :: (t22 -> t3 -> Connection -> IO Int64) -> t2 -> t3 -> IO ()
-- runWithConn action x y =
--   do
--     conn <- pConnect
--     runManaged $ p $managed (action x y)
--     close conn

-- runWithConn3 :: (Connection -> t2 -> t3 -> t4 -> IO Int64) -> t2 -> t3 -> t4 -> IO ()
-- runWithConn3 action x y z =
--   do
--     conn <- pConnect
--     action conn x y z
--     close conn

-- runConnected (Connected Int64)
--   a <*> b <*> c
