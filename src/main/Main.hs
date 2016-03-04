module Main where

import Podium.Api.Server

import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 app

-- main :: IO ()
-- main = putStrLn "hasdfd"
