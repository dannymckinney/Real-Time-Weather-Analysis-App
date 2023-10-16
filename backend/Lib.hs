module Lib where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson
import GHC.Generics
import Data.List (sortBy)
import Data.Ord (comparing)

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

-- Convert Kelvin to Fahrenheit
kelvinToFahrenheit :: Float -> Float
kelvinToFahrenheit k = (k - 273.15) * 9/5 + 32

-- Data Sorting
sortWeatherDataByTemperature :: [WeatherData] -> [WeatherData]
sortWeatherDataByTemperature = sortBy (comparing (temp . main))

-- Data Filtering
filterWeatherDataByTemperature :: Float -> [WeatherData] -> [WeatherData]
filterWeatherDataByTemperature threshold = filter (\wd -> temp (main wd) > threshold)
