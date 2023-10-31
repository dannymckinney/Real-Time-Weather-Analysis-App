module TestLib where

import Lib (fetchWeatherData)
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
  let apiKey = "43227aead2fa7bd3f2af255f98a5a53a"
  let lat = 41.8077
  let lon = 72.2540
  response <- fetchWeatherData apiKey lat lon
  L8.putStrLn $ L8.pack (show response)
