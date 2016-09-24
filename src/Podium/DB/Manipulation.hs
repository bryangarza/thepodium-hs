{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Podium.DB.Manipulation (
    createPost
  , createAccount
  , deletePost
  , deleteAccount
  , updatePostBody
  , updateAccountUsername
  , updateAccountEmail
  , updateAccountPassword
  ) where

import           Podium.DB.Table.Post
import qualified Podium.DB.Table.Post    as Post
import           Podium.DB.Table.Account
import qualified Podium.DB.Table.Account as Account
import           Podium.DB.Util

import           Control.Lens       (set, view)
import           Data.ByteString    (ByteString)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Time.Clock    (getCurrentTime)

import           Opaleye.Constant (Constant,constant)
import           Opaleye.Internal.Column (Column(Column),unColumn)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import           Opaleye ( (.==)
                         , runDelete
                         , runInsert
                         , runUpdate
                         )

import           Opaleye.PGTypes ( pgStrictText
                                 , pgInt4
                                 , pgUUID
                                 , pgUTCTime
                                 , pgArray )

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Int (Int64)
import Data.Pool

createPost :: Post -> AccountId -> Database Int64
createPost p accountId =
  do
    timestamp <- liftIO getCurrentTime
    pool <- ask
    lift $ withResource pool (\conn -> runInsert conn Post.table (columns timestamp))
  where
    body'      = pgStrictText (view body p)
    accountId' = pgUUID accountId
    columns t  = Post Nothing body' (pgUTCTime t) accountId'

updatePostField :: (Post.ColumnW -> Post.ColumnW) -> PostId -> Database Int64
updatePostField update idToMatch = do
  pool <- ask
  lift $ withResource pool (\conn -> runUpdate conn Post.table update' match)
  where
    idToMatch' = pgInt4 idToMatch
    update' = update . set postId (Just idToMatch')
    match = (.== idToMatch') . view postId

updatePostBody :: Body -> PostId -> Database Int64
updatePostBody = updatePostField . set body . pgStrictText

deletePost :: PostId -> Database Int64
deletePost idToMatch = ask >>= lift . flip withResource (\conn -> runDelete conn Post.table match)
  where
    match = (.== pgInt4 idToMatch) . view postId

-- pgArray :: forall a b. Default Constant a (Column b) => [a] -> Column (PGArray b)
-- pgArray = Column . HPQ.ArrayExpr . fmap ( unColumn . (constant :: a -> Column b))

createAccount :: Account -> Database Int64
createAccount Account {..} =
  do
    accountId <- liftIO genUuid
    hash      <- liftIO $ genPassword (encodeUtf8 _password)
    pool      <- ask
    lift $ withResource pool (\conn -> runInsert conn Account.table (columns hash))
  where
    accountId'   = pgUUID       _accountId
    username'    = pgStrictText _username
    email'       = pgStrictText _email
    bio'         = pgStrictText _bio
    joinDate'    = pgUTCTime    _joinDate
    location'    = pgStrictText _location
    profession'  = pgStrictText _profession
    yearsOfExp'  = pgInt4       _yearsOfExp
    interests'   = pgArray pgStrictText _interests
    columns hash = Account accountId' username' email' hash' bio' joinDate' location' profession' yearsOfExp' interests'
      where hash' = pgStrictText (decodeUtf8 hash)

updateAccountField :: (Account.ColumnR -> Account.ColumnW) -> AccountId -> Database Int64
updateAccountField update idToMatch = do
  pool <- ask
  lift $ withResource pool (\conn -> runUpdate conn Account.table update match)
  where
    match = (.== pgUUID idToMatch) . view Account.accountId

updateAccountUsername :: Username -> AccountId -> Database Int64
updateAccountUsername = updateAccountField . set username . pgStrictText

updateAccountEmail :: Email -> AccountId -> Database Int64
updateAccountEmail = updateAccountField . set email . pgStrictText

updateAccountPassword :: Password -> AccountId -> Database Int64
updateAccountPassword newPassword accountId =
  do
    hash <- liftIO $ genPassword (encodeUtf8 newPassword)
    updateAccountField (update hash) accountId
  where
    update :: ByteString -> Account.ColumnR -> Account.ColumnW
    update = set password . pgStrictText . decodeUtf8

deleteAccount :: AccountId -> Database Int64
deleteAccount idToMatch = ask >>= lift . flip withResource (\conn -> runDelete conn Account.table match)
  where
    match = (.== pgUUID idToMatch) . view Account.accountId
