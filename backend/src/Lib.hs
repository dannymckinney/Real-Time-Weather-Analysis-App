{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Servant
import Network.HTTP.Simple hiding (Proxy)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(..))

type WeatherAPI = "weather" :> Get '[JSON] WeatherData

data WeatherData = WeatherData {
  main :: MainWeather
} deriving (Show, Generic, ToJSON, FromJSON)

data MainWeather = MainWeather {
  temp :: Float,
  humidity :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

fetchWeatherData :: String -> Float -> Float -> IO (Response L8.ByteString)
fetchWeatherData apiKey lat lon = do
  let apiEndpoint = "http://api.openweathermap.org/data/2.5/weather?lat=" ++ show lat ++ "&lon=" ++ show lon ++ "&appid=" ++ apiKey
  request <- parseRequest apiEndpoint
  httpLBS request

weatherHandler :: Handler WeatherData
weatherHandler = do
  let apiKey = "43227aead2fa7bd3f2af255f98a5a53a"
  let lat = 41.8077
  let lon = 72.2540
  response <- liftIO $ fetchWeatherData apiKey lat lon
  let weatherData = getResponseBody response
  case decode weatherData of
    Just wd -> return wd
    Nothing -> throwError err500 { errBody = "Failed to parse weather data" }

server :: Server WeatherAPI
server = weatherHandler

app :: Application
app = serve (Proxy :: Proxy WeatherAPI) server
