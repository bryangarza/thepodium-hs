{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Podium.Api.Server where

import           Podium.DB.Manipulation
import           Podium.DB.Table.Account (Account, AccountId)
import           Podium.DB.Connect
import           Podium.DB.Query
import qualified Podium.DB.Table.Post as P
import           Podium.Html.Post

import Control.Lens                         (set)
import Control.Monad.Trans                  (liftIO)
import Control.Monad.Trans.Reader           (runReaderT)
import Control.Monad.Trans.Either           (EitherT)
import Data.Default
import Data.UUID as U
import Data.Text (Text)
import Network.Wai                          (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.Common.Text
import Servant.HTML.Lucid                   (HTML)

import Database.PostgreSQL.Simple
import Data.Pool

type MyApi = "account" :> Capture "accountId" AccountId :> Get '[JSON] [Account]
        :<|> "posts" :> Capture "accountId" AccountId :> Get '[JSON, HTML] [P.Post]
        :<|> "new_post" :> Capture "accountId" AccountId :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[JSON, HTML] [P.Post]

myApi :: Proxy MyApi
myApi = Proxy

server :: Pool Connection -> Server MyApi
server conn = account
    :<|> posts
    :<|> newPost
  where
    account :: AccountId -> EitherT ServantErr IO [Account]
    account accountId = liftIO $ runReaderT (runAccountByAccountId accountId) conn
    posts :: AccountId -> EitherT ServantErr IO [P.Post]
    posts accountId = liftIO $ runReaderT (runPostByAccountId accountId) conn
    newPost :: AccountId -> [(Text, Text)] -> EitherT ServantErr IO [P.Post]
    newPost accountId [(_, fieldData)] = do
      liftIO $ runReaderT (createPost (set P.body fieldData (def :: P.Post)) accountId) conn
      liftIO $ runReaderT (runPostByAccountId accountId) conn

app :: Pool Connection -> Application
app = logStdoutDev . serve myApi . server
