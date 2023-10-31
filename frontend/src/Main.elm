module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Platform.Cmd as Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import Json.Decode as Decode exposing (Decoder)


type alias WeatherData =
    { lon : Float
    , lat : Float
    , main : String
    , description : String
    , temp : Float
    , humidity : Int
    , pressure : Int
    , windSpeed : Float
    , country : String
    , city : String
    }

type alias Model =
    { weather : Maybe WeatherData
    }

type Msg
    = GotWeather (Result Http.Error WeatherData)
    | FetchWeather

init : Model
init =
    { weather = Nothing }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        -- other cases
        FetchWeather ->
            (model, getWeather)

view : Model -> Html Msg
view model =
    div [ class "weather-container" ]
        [ div [ class "location" ] [ text "Weather Analysis" ]
        , case model.weather of
            Just weather ->
                div []
                    [ div [ class "temperature" ] [ text ("Temperature: " ++ String.fromFloat weather.temp ++ "°C") ]
                    , div [ class "humidity" ] [ text ("Humidity: " ++ String.fromInt weather.humidity ++ "%") ]
                    , div [ class "pressure" ] [ text ("Pressure: " ++ String.fromInt weather.pressure ++ " hPa") ]
                    , div [ class "windSpeed" ] [ text ("Wind Speed: " ++ String.fromFloat weather.windSpeed ++ " m/s") ]
                    , div [ class "location" ] [ text ("Location: " ++ weather.city ++ ", " ++ weather.country) ]
                    ]

            Nothing ->
                div [ class "loading" ] [ text "Loading weather data..." ]
        , button [ onClick FetchWeather ] [ text "Refresh" ]

        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

getWeather : Cmd Msg
getWeather =
    Http.get
        { url = "http://localhost:3000/weather"
        , expect = Http.expectJson GotWeather weatherDecoder
        }

weatherDecoder : Decode.Decoder WeatherData
weatherDecoder =
    Decode.map10 WeatherData
        (Decode.field "coord" (Decode.field "lon" Decode.float))
        (Decode.field "coord" (Decode.field "lat" Decode.float))
        (Decode.field "weather" (Decode.index 0 (Decode.field "main" Decode.string)))
        (Decode.field "weather" (Decode.index 0 (Decode.field "description" Decode.string)))
        (Decode.field "main" (Decode.field "temp" Decode.float))
        (Decode.field "main" (Decode.field "humidity" Decode.int))
        (Decode.field "main" (Decode.field "pressure" Decode.int))
        (Decode.field "wind" (Decode.field "speed" Decode.float))
        (Decode.field "sys" (Decode.field "country" Decode.string))
        (Decode.field "name" Decode.string)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
