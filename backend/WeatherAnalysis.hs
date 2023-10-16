{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WeatherAnalysis where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson
import GHC.Generics

-- Define a data type for weather data
data WeatherData = WeatherData {
    main :: MainWeather
} deriving (Show, Generic)

data MainWeather = MainWeather {
    temp :: Float,
    humidity :: Int
} deriving (Show, Generic)

-- Aeson instances for JSON parsing
instance FromJSON WeatherData
instance FromJSON MainWeather

-- Fetch weather data from OpenWeatherMap
fetchWeatherData :: String -> Float -> Float -> IO (Response L8.ByteString)
fetchWeatherData apiKey lat lon = do
    let apiEndpoint = "http://api.openweathermap.org/data/2.5/weather?lat=" ++ show lat ++ "&lon=" ++ show lon ++ "&appid=" ++ apiKey
    request <- parseRequest apiEndpoint
    httpLBS request

-- Store weather data in a file
storeWeatherData :: L8.ByteString -> FilePath -> IO ()
storeWeatherData weatherData filename = L8.writeFile filename weatherData

-- Example usage
main :: IO ()
main = do
    -- Define API key, latitude, and longitude
    let apiKey = "43227aead2fa7bd3f2af255f98a5a53a"
    let lat = 41.8077  -- Latitude for UConn
    let lon = 72.2540  -- Longitude for UConn
    
    -- Fetch weather data from OpenWeatherMap
    response <- fetchWeatherData apiKey lat lon
    
    -- Extract and print the response body
    let weatherData = getResponseBody response
    L8.putStrLn weatherData
    
    -- Store the weather data in a file
    storeWeatherData weatherData "weatherdata.json"
