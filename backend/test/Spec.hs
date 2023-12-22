{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Lib
import GHC.Generics (Generic)
import Control.Exception (SomeException)


main :: IO ()
main = hspec $ do
  describe "Lib.MainWeather Monoid" $ do
    it "obeys the Monoid laws" $ do
      let weather1 = Lib.MainWeather 10 70
      let weather2 = Lib.MainWeather 20 80
      let weather3 = Lib.MainWeather 30 90
      -- Ensure the last statement is an expression
      (weather1 <> weather2) <> weather3 `shouldBe` weather1 <> (weather2 <> weather3)
      -- Add more assertions as needed

  describe "Temperature Conversion" $ do
    it "converts temperature from Kelvin to Fahrenheit correctly" $ do
      let kelvinTemp = Lib.MainWeather 300 80
      let fahrenheitTemp = Lib.convertTempToFahrenheit kelvinTemp
      Lib.temp fahrenheitTemp `shouldBe` 80.33

  describe "Temperature Change Detection" $ do
    it "detects significant temperature changes" $ do
      let weatherData = [Lib.MainWeather 280 50, Lib.MainWeather 285 55, Lib.MainWeather 290 60]
      let changes = Lib.detectTempChange weatherData
      -- Add an assertion or test expression here
      changes `shouldBe` [(280, 5), (285, 5)]  -- Replace with your expected result

  describe "Concurrency in Data Fetching" $ do
    it "fetches data concurrently without errors" $ do
      let apiKey = "testApiKey"
      let locations = [(41.8077, 72.2540), (34.0522, -118.2437)]
      results <- Lib.fetchWeatherDataConcurrently apiKey locations
      length (filter isRight results) `shouldBe` length locations

  describe "Real-Time Data Analysis" $ do
    it "analyzes data in real-time" $ do
      let apiKey = "testApiKey"
      let locations = [(41.8077, 72.2540)]
      -- Directly call the function. Note: This will run indefinitely.
      Lib.periodicFetchAndAnalyze apiKey locations
      -- Since this function doesn't return a value, you can't make assertions here.

-- Helper function to check if Either is Right
isRight :: Either SomeException a -> Bool
isRight (Right _) = True
isRight _ = False
