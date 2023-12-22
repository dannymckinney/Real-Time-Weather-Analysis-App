{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Network.Wai.Middleware.Cors (simpleCors)
import Lib
--import System.Log.Logger (rootLoggerName, setLevel, Priority(DEBUG))

-- Define the API type
type API = "weather" :> Get '[JSON] Lib.WeatherData
      :<|> "static" :> Raw
      :<|> Raw

-- Proxy for the API
api :: Proxy API
api = Proxy

-- Server implementation
mserver :: Server API
mserver = Lib.weatherHandler
    :<|> serveDirectoryFileServer "../frontend/static"
    :<|> serveDirectoryFileServer "../frontend"

-- Application
mapp :: Application
mapp = simpleCors $ serve api mserver

-- Main function
main :: IO ()
main = do

  -- Define API key and city ID
  let apiKey = "43227aead2fa7bd3f2af255f98a5a53a"
  let cityId = 4843786 -- City ID for Storrs, CT

  -- Fetch and process weather data
  putStrLn "Fetching weather data..."
  weatherResult <- Lib.fetchWeatherData apiKey cityId

  case weatherResult of
    Right weatherData -> do
      putStrLn $ "Successfully fetched weather data: " ++ show weatherData
    Left err -> do
      putStrLn $ "Error fetching weather data: " ++ show err

  -- Start the server
  putStrLn "Running on port 9000"
  run 9000 mapp
