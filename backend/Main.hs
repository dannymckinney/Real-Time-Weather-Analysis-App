-- app/Main.hs

module Main where

import qualified Lib
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple (getResponseBody)

main :: IO ()
main = hspec $ do
    -- Define API key, latitude, and longitude
    let apiKey = "placeholder"
    let lat = 41.8077  -- Latitude for UConn
    let lon = 72.2540  -- Longitude for UConn
    
    -- Fetch weather data from OpenWeatherMap
    response <- Lib.fetchWeatherData apiKey lat lon
    
    -- Extract and print the response body
    let weatherData = getResponseBody response
    L8.putStrLn weatherData
    
    -- Store the weather data in a file
    Lib.storeWeatherData weatherData "weatherdata.json"

    describe "Weather Data Analysis" $ do
        it "sorts weather data by temperature" $ do
            let weatherDataList = [ ... ] -- Sample list of WeatherData
            let sortedData = sortWeatherDataByTemperature weatherDataList
            -- Add assertions to check if the data is sorted correctly

        it "filters weather data by temperature threshold" $ do
            let weatherDataList = [ ... ] -- Sample list of WeatherData
            let filteredData = filterWeatherDataByTemperature 20.0 weatherDataList
            -- Add assertions to check if the data is filtered correctly