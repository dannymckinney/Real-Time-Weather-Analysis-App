{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
  ( WeatherData(..)
  , Coordinates(..)
  , WeatherDescription(..)
  , Wind(..)
  , Clouds(..)
  , fetchWeatherData
  , weatherHandler
  , server
  , app
  ) where

import Control.Monad.IO.Class (liftIO)  -- Import liftIO
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, wait, mapConcurrently)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, withObject, (.:), (.=), object, eitherDecode, decode, parseJSON, toJSON)
import Data.ByteString.Lazy.Char8 as L8 (ByteString, pack, putStrLn)
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody, Response)
import Control.Exception (SomeException, try, throwIO, toException)
import Servant
import System.IO (hPutStrLn, stderr, stdout)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (Priority(..), updateGlobalLogger, rootLoggerName, setLevel, infoM, errorM, addHandler)
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy as B


-- Defines a type alias for fetching weather data using JSON.
type WeatherAPI = "weather" :> Get '[JSON] WeatherData

-- Coordinates data
data Coordinates = Coordinates {
  lon :: Float,
  lat :: Float
} deriving (Show, Generic, ToJSON, FromJSON)

-- Weather description data
data WeatherDescription = WeatherDescription {
  id :: Int,
  main :: String,
  description :: String,
  icon :: String
} deriving (Show, Generic, ToJSON, FromJSON)

-- Main weather data
data Main = Main {
  temp :: Float,
  feels_like :: Float,
  temp_min :: Float,
  temp_max :: Float,
  pressure :: Int,
  humidity :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

-- Wind data
data Wind = Wind {
  speed :: Float,
  deg :: Int,
  gust :: Maybe Float  -- Optional field
} deriving (Show, Generic, ToJSON, FromJSON)

-- Rain data
data Rain = Rain {
  _1h :: Maybe Float  -- Optional field
} deriving (Show, Generic, ToJSON, FromJSON)

-- Clouds data
data Clouds = Clouds {
  all :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

-- System information
data Sys = Sys {
  sysType :: Maybe Int,  
  sysId :: Maybe Int, 
  country :: String,
  sunrise :: Int,
  sunset :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

data ProcessedData = ProcessedData {
  avgTemperature :: Float,
  trend :: String,
  windReport :: String
} deriving (Show, Generic, ToJSON, FromJSON)

data WeatherData = WeatherData {
  coord :: Coordinates,
  weather :: [WeatherDescription],
  base :: String,
  main :: Main,
  visibility :: Int,
  wind :: Wind,
  rain :: Maybe Rain,  
  clouds :: Clouds,
  dt :: Int,
  sys :: Sys,
  timezone :: Int,
  id :: Int,
  name :: String,
  cod :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

-- Converts temperature from Kelvin to Fahrenheit in a Main object.
convertTempToFahrenheit :: Main -> Main
convertTempToFahrenheit mainWeather = mainWeather { temp = convertTemp (temp mainWeather) }
  where
    convertTemp t = (t - 273.15) * 9 / 5 + 32

-- Detects significant temperature changes between consecutive weather data points.
detectTempChange :: [Main] -> [(Float, Float)]
detectTempChange [] = []
detectTempChange [_] = []
detectTempChange (x:y:xs) = (temp x, abs (temp x - temp y)) : detectTempChange (y:xs)

-- Detects significant humidity changes between consecutive weather data points.
detectHumidityChange :: [Main] -> [(Int, Int)]
detectHumidityChange [] = []
detectHumidityChange [_] = []
detectHumidityChange (x:y:xs) = (humidity x, abs (humidity x - humidity y)) : detectHumidityChange (y:xs)

setupLogging :: IO ()
-- Initializes logging for the application.
-- I creates log handler that writes to 'weatherApp.log'.
setupLogging = do
 -- Creates file handler for logging, sets to DEBUG level, and formats the log messages.
 handler <- fileHandler "weatherApp.log" DEBUG >>= \h -> return $
   setFormatter h (simpleLogFormatter "$time - $prio - $msg")
 updateGlobalLogger rootLoggerName (addHandler handler)
 updateGlobalLogger rootLoggerName (setLevel DEBUG)


--fetchWeatherData :: String -> Float -> Float -> IO (Response L8.ByteString)
--fetchWeatherData apiKey lat lon = do
 --let apiEndpoint = "http://api.openweathermap.org/data/2.5/weather?lat=" ++ show lat ++ "&lon=" ++ show lon ++ "&appid=" ++ apiKey
 --request <- parseRequest apiEndpoint
 --httpLBS request

-- Fetches weather data for a given city using its ID.
-- Returns either weather data or an exception in case of an error.


fetchWeatherData :: String -> Int -> IO (Either SomeException WeatherData)
fetchWeatherData apiKey cityId = do
  let apiEndpoint = "http://api.openweathermap.org/data/2.5/weather?id=" ++ show cityId ++ "&appid=" ++ apiKey
  -- Logs API request URL.
  infoM "Lib.fetchWeatherData" $ "Requesting data from: " ++ apiEndpoint
  -- Sends an HTTP GET request to API endpoint.
  request <- parseRequest apiEndpoint
  response <- httpLBS request
  let weatherData = getResponseBody response
  -- Logs the received response.
  infoM "Lib.fetchWeatherData" $ "Received response: " ++ show weatherData
  -- Attempts to decode the JSON response into `WeatherData`; logs success or failure.
  case eitherDecode weatherData :: Either String WeatherData of
    Right wd -> do
      infoM "Lib.fetchWeatherData" "Data parsed successfully."
      return $ Right wd
    Left err -> do
      L8.putStrLn $ L8.pack ("Parsing error: " ++ err) -- print detailed error
      errorM "Lib.fetchWeatherData" "Failed to parse weather data."
      return $ Left $ toException $ userError "Failed to parse weather data"

fetchWeatherDataConcurrently :: String -> [Int] -> IO [Either SomeException WeatherData]
-- Fetches weather data for multiple cities concurrently.
-- Takes API key and list of city IDs, returning list of Either results.
fetchWeatherDataConcurrently apiKey cityIds = do
  -- Wraps each `fetchWeatherData` call in an async action for concurrency.
  responses <- mapM (async . (\cityId -> fetchWeatherData apiKey cityId)) cityIds
  -- Waits for all asynchronous actions to complete and collects the results.
  mapM wait responses

processWeatherDataInParallel :: [WeatherData] -> IO [ProcessedData]
-- Applies `processSingleWeatherData` to each element of the list concurrently.
processWeatherDataInParallel weatherDataList =
  mapConcurrently processSingleWeatherData weatherDataList

processSingleWeatherData :: WeatherData -> IO ProcessedData
-- Processes single WeatherData record.
-- Computes average temperature, weather trend, and wind report.
processSingleWeatherData (WeatherData { main = mainField, weather = weatherList, wind = windData }) = do
  let avgTemperature = calculateAverageTemp mainField
      -- Analyzes the weather trend based on descriptions.
      trend = analyzeWeatherTrend weatherList
      -- Analyzes wind data to provide a wind report.
      windReport = analyzeWind windData
  return $ ProcessedData avgTemperature trend windReport

calculateAverageTemp :: Main -> Float
-- Takes the temperature and 'feels like' temperature, returning their average.
calculateAverageTemp mainWeather =
  (temp mainWeather + feels_like mainWeather) / 2

analyzeWeatherTrend :: [WeatherDescription] -> String
-- Returns "Rainy", "Clear", or "Mixed" based on the presence of respective weather conditions.
analyzeWeatherTrend descriptions
 | Prelude.any (\WeatherDescription { main = mainDesc } -> mainDesc == "Rain") descriptions = "Rainy"
 | Prelude.any (\WeatherDescription { main = mainDesc } -> mainDesc == "Clear") descriptions = "Clear"
 | otherwise = "Mixed"

analyzeWind :: Wind -> String
-- Takes WindData object and categorizes wind speed into more descriptive terms.
analyzeWind windData =
 let spd = speed windData
 in if spd < 5 then "Calm"
    else if spd < 15 then "Moderate"
         else "Windy"

fetchWeatherDataWrapped :: String -> Int -> IO (Either SomeException WeatherData)
-- Wraps the fetchWeatherData function in a try-catch block and returns an Either with the result.
fetchWeatherDataWrapped apiKey cityId = do
  result <- try $ fetchWeatherData apiKey cityId
  case result of
    Left err -> return $ Left err
    Right response -> return response

fetchAndProcessWeatherData :: String -> [Int] -> IO (Either SomeException [ProcessedData])
fetchAndProcessWeatherData apiKey cityIds = do
  -- Fetch weather data for each city and collect the results.
  fetchedData <- fetchWeatherDataConcurrently apiKey cityIds
  -- Return an Either with exceptions (Left) or processed data (Right).
  case sequence fetchedData of
    Left err -> return $ Left err
    Right dataPoints -> Right <$> processWeatherDataInParallel dataPoints

calculateAverage :: Fractional b => [Float] -> b
-- Calculates the average of a list of Float values.
calculateAverage xs = realToFrac (sum xs) / fromIntegral (Prelude.length xs)

analyzeForecastTrend :: [WeatherData] -> String
-- Analyzes temperature and humidity trends in weather data.
analyzeForecastTrend weatherDataList =
-- Extract temperature and humidity data from the main weather data for each WeatherData object.
 let tempTrends = Prelude.map (\(WeatherData { main = m }) -> temp m) weatherDataList
     humidityTrends = Prelude.map (\(WeatherData { main = m }) -> humidity m) weatherDataList
     -- Generate a summary string with temperature and humidity trend analysis.
 in "Temperature Trend: " ++ trendAnalysis tempTrends ++
    ", Humidity Trend: " ++ trendAnalysis humidityTrends

trendAnalysis :: (Num a, Ord a) => [a] -> String
-- Analyzes trends in a list of numeric values.
trendAnalysis [] = "Stable"
trendAnalysis xs = if increasing xs then "Increasing" else if decreasing xs then "Decreasing" else "Fluctuating"

increasing :: (Ord a) => [a] -> Bool
-- Checks if a list of values is strictly increasing.
increasing (x:y:xs) = x < y && increasing (y:xs)
increasing _ = True

decreasing :: (Ord a) => [a] -> Bool
-- Checks if a list of values is strictly decreasing.
decreasing (x:y:xs) = x > y && decreasing (y:xs)
decreasing _ = True

generateSevereWeatherAlerts :: WeatherData -> [String]
-- Generates alerts based on high wind and heavy rain conditions.
generateSevereWeatherAlerts weatherData =
 Prelude.concat [checkForHighWinds (wind weatherData), checkForHeavyRain (rain weatherData)]

checkForHighWinds :: Wind -> [String]
-- Checks for high wind conditions.
checkForHighWinds windData
 | speed windData > 20 = ["High wind alert!"]
 | otherwise = []

checkForHeavyRain :: Maybe Rain -> [String]
-- Checks for heavy rain conditions
checkForHeavyRain (Just rainData)
  | Just amount <- _1h rainData, amount > 10 = ["Heavy rain warning!"]
  | otherwise = []
checkForHeavyRain Nothing = []

--analyzeAirQuality :: WeatherData -> String
--analyzeAirQuality weatherData =
 --let visibilityScore = visibility weatherData
   --  cloudsRecord = clouds weatherData
    -- cloudsScore = all cloudsRecord  -- Accessing 'all' field directly from the clouds record
 --in "Air Quality Index: " ++ show (calculateAirQualityIndex visibilityScore cloudsScore)

calculateAirQualityIndex :: Int -> Int -> Int
-- Calculates an air quality index based on visibility and cloud cover scores.
calculateAirQualityIndex visibility clouds =
 -- Hypothetical calculation for AQI.
 -- It subtracts a fraction of cloud cover and adds a fraction of visibility to compute the AQI.
 100 - (clouds `div` 10) + (visibility `div` 100)

testWithJsonFile :: FilePath -> IO ()
testWithJsonFile filePath = do
  Prelude.putStrLn $ "Reading JSON file: " ++ filePath

  jsonData <- B.readFile filePath
  Prelude.putStrLn "JSON file read successfully"

  case eitherDecode jsonData :: Either String WeatherData of
    Right weatherData -> do
      Prelude.putStrLn "JSON decoded successfully"
      Prelude.putStrLn "Processing weather data..."
      processedData <- processSingleWeatherData weatherData
      Prelude.putStrLn "Processed data:"
      print processedData
    Left err -> Prelude.putStrLn $ "Error parsing JSON: " ++ err




weatherHandler :: Handler WeatherData
-- Handler function for retrieving weather data.
weatherHandler = do
  -- Define the API key and city ID for Storrs, CT.
  let apiKey = "43227aead2fa7bd3f2af255f98a5a53a"
  let cityId = 4843786 -- Storrs, CT
  -- Fetch weather data using the specified API key and city ID.
  result <- liftIO $ fetchWeatherData apiKey cityId
  -- Check the result of the weather data retrieval.
  case result of
    Right wd -> return wd
    Left err -> throwError err500 { errBody = L8.pack $ show err }

-- Defines the WeatherAPI server using the weatherHandler.
server :: Server WeatherAPI
server = weatherHandler

-- app is the main application that serves the WeatherAPI.
app :: Application
app = serve (Proxy :: Proxy WeatherAPI) server

main2 :: IO ()
main2 = do
  -- Call testWithJsonFile to process data from the JSON file
  let testJsonFilePath = "testWeatherData.json"
  testWithJsonFile testJsonFilePath

  let apiKey = "43227aead2fa7bd3f2af255f98a5a53a"
  let cityId = 4843786 -- City ID for Storrs, CT

  -- Log the start of the operation
  infoM "Main" "Starting the Weather Data Fetch operation"

  -- Fetch weather data for the specified city ID
  weatherResult <- fetchWeatherData apiKey cityId

  -- Handle the result of fetchWeatherData
  case weatherResult of
    Right weatherData -> do
      -- Log successful fetch and display the weather data
      infoM "Main" "Successfully fetched weather data"
      L8.putStrLn $ L8.pack $ "Fetched Weather Data: " ++ show weatherData

    Left err -> do
      -- Log any errors that occurred during fetch
      errorM "Main" $ "Error fetching weather data: " ++ show err

  -- Log the starting of the server
  infoM "Main" "Starting server on port 9000"
  L8.putStrLn "Starting server on port 9000"

  -- Start the server
  run 9000 app
