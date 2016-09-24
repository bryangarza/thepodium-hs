module Main where

import Podium.Api.Server
import Podium.DB.Connect

import Network.Wai
import Network.Wai.Handler.Warp

import Control.Monad.Trans.Class

main :: IO ()
main = do
  conn <- mkPooledConnection
  run 8081 (app conn)

-- main :: IO ()
-- main = putStrLn "hasdfd"
