{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Lib (WeatherData, weatherHandler)

type API = "weather" :> Get '[JSON] Lib.WeatherData
      :<|> "static" :> Raw
      :<|> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = Lib.weatherHandler
    :<|> serveDirectoryFileServer "../frontend/static"
    :<|> serveDirectoryFileServer "../frontend"

app :: Application
app = simpleCors $ serve api server

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Running on port " ++ show port
  run port app
