{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Podium.Api.Server where

import           Podium.DB.Manipulation
import           Podium.DB.Query (runPostByAccountId)
import qualified Podium.DB.Table.Post as P
import           Podium.Html.Post

import Control.Lens                         (set)
import Control.Monad.Trans                  (liftIO)
import Control.Monad.Trans.Either           (EitherT)
import Data.Default
import Data.UUID as U
import Data.Text (Text)
import Network.Wai                          (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.HTML.Lucid                   (HTML)

type MyApi = "posts" :> Get '[JSON, HTML] [P.Post]
        -- Accept POST [(Text, Text)] (FormUrlEncoded), returns [Post] as JSON or HTML.
        :<|> "newpost" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[JSON, HTML] [P.Post]

myApi :: Proxy MyApi
myApi = Proxy

server :: Server MyApi
server = posts
    :<|> newpost
  where
    posts :: EitherT ServantErr IO [P.Post]
    posts = liftIO runPostByAccountId
    newpost :: [(Text, Text)] -> EitherT ServantErr IO [P.Post]
    newpost [(_, fieldData)] = do
      liftIO (createPost (set P.body fieldData (def :: P.Post)) U.nil)
      liftIO runPostByAccountId

app :: Application
app = logStdoutDev (serve myApi server)
