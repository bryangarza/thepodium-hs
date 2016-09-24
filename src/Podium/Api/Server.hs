{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Podium.Api.Server where

import           Podium.DB.Manipulation
import           Podium.DB.Table.Account (AccountId)
import           Podium.DB.Connect
import           Podium.DB.Query (runPostByAccountId)
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

type MyApi = "posts" :> Capture "accountId" AccountId :> Get '[JSON, HTML] [P.Post]
        -- Accept POST [(Text, Text)] (FormUrlEncoded), returns [Post] as JSON or HTML.
        :<|> "newpost" :> Capture "accountId" AccountId :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[JSON, HTML] [P.Post]

myApi :: Proxy MyApi
myApi = Proxy

server :: Pool Connection -> Server MyApi
server conn = posts
    :<|> newpost
  where
    posts :: AccountId -> EitherT ServantErr IO [P.Post]
    posts accountId = liftIO $ runReaderT (runPostByAccountId accountId) conn
    newpost :: AccountId -> [(Text, Text)] -> EitherT ServantErr IO [P.Post]
    newpost accountId [(_, fieldData)] = do
      liftIO $ runReaderT (createPost (set P.body fieldData (def :: P.Post)) accountId) conn
      liftIO $ runReaderT (runPostByAccountId accountId) conn

app :: Pool Connection -> Application
app = logStdoutDev . serve myApi . server
