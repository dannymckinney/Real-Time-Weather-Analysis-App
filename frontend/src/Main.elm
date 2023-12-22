module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, succeed, field, list, string, andThen, map, int, float, maybe, map2, map3, map4, map5, map6)
import Debug exposing (log) -- Import Debug module
import Time exposing (Zone, Posix, toHour, toMinute, toSecond, utc, millisToPosix)
import String exposing (fromInt)




-- MODEL

type alias Model =
    { weather : Maybe WeatherData
    , processedData : Maybe ProcessedData
    , isFetching : Bool
    , fetchButtonClicked : Bool
    }

type alias WeatherData =
    { coord : Coordinates
    , weather : List WeatherDescription
    , base : String
    , main : Main
    , visibility : Int
    , wind : Wind
    , clouds : Clouds
    , dt : Int
    , sys : Sys
    , timezone : Int
    , id : Int
    , name : String
    , cod : Int
    }

-- Additional data structures

type alias Coordinates =
    { lon : Float
    , lat : Float
    }

type alias WeatherDescription =
    { id : Int
    , main : String
    , description : String
    , icon : String
    }

type alias Main =
    { temp : Float
    , feels_like : Float
    , temp_min : Float
    , temp_max : Float
    , pressure : Int
    , humidity : Int
    }

type alias Wind =
    { speed : Float
    , deg : Int
    }

type alias Clouds =
    { all : Int
    }

type alias Sys =
    { type_ : Maybe Int
    , id : Maybe Int
    , country : String
    , sunrise : Int
    , sunset : Int
    }

type alias ProcessedData =
    { avgTemperature : Float
    , trend : String
    , windReport : String
    }

-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( { weather = Nothing, processedData = Nothing, isFetching = False, fetchButtonClicked = False}, Cmd.none )

-- UPDATE

type Msg
    = FetchWeather
    | ReceiveWeather (Result Http.Error WeatherData)
    | FetchProcessedData
    | ReceiveProcessedData (Result Http.Error ProcessedData)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchWeather ->
            ( { model | isFetching = True, fetchButtonClicked = True }, getWeather )
        ReceiveWeather (Ok weatherData) ->
            let
                updatedModel = { model | weather = Just weatherData, isFetching = False }
            in
            (updatedModel, Cmd.none)

        ReceiveWeather (Err decodingError) ->
            let
                _ = Debug.log "Decoding error" decodingError
            in
            ( { model | isFetching = False }, Cmd.none )

        FetchProcessedData ->
            ( { model | isFetching = True }, fetchProcessedData )

        ReceiveProcessedData (Ok pData) ->
            ( { model | processedData = Just pData, isFetching = False }, Cmd.none )

        ReceiveProcessedData (Err decodingError) ->
            let
                _ = Debug.log "Decoding error" decodingError
            in
            ( { model | isFetching = False }, Cmd.none )

-- VIEW

formatTime : Int -> String
formatTime timestamp =
    let
        posixTime = millisToPosix (round (toFloat timestamp * 1000))
        hours = toHour Time.utc posixTime
        minutes = toMinute Time.utc posixTime
        seconds = toSecond Time.utc posixTime
    in
    fromInt hours ++ ":" ++ padWithZeroes minutes ++ ":" ++ padWithZeroes seconds

padWithZeroes : Int -> String
padWithZeroes value =
    if value < 10 then
        "0" ++ fromInt value
    else
        fromInt value


fetchProcessedData : Cmd Msg
fetchProcessedData =
    Http.get
        { url = "http://localhost:9000/processed-data"  -- Update with your backend's URL
        , expect = Http.expectJson ReceiveProcessedData processedDataDecoder
        }

extraClass : Bool -> List (Html.Attribute msg)
extraClass condition =
    if condition then
        [ Html.Attributes.class "disabled" ]
    else
        []



viewWeatherData : WeatherData -> Maybe ProcessedData -> List (Html Msg)
viewWeatherData weather maybeProcessedData =
    let
        formatTemperature k = String.fromInt (round ((k - 273.15) * 9 / 5 + 32))
        tempMinF = formatTemperature weather.main.temp_min
        tempMaxF = formatTemperature weather.main.temp_max
    in
    [ div [ class "weather-container" ]
        [ div [ class "location" ] [ text ("City: " ++ weather.name) ]
        , div [ class "current-weather" ]
            [ div [ class "temperature" ] [ text ("Temperature: " ++ formatTemperature weather.main.temp ++ "°F") ]
            , div [ class "high-low" ]
                [ text ("Today's high will be " ++ tempMaxF ++ "°F and today's low will be " ++ tempMinF ++ "°F") ]
            , div [ class "condition" ]
                [ text "Current Conditions: " 
                , text (weather.weather |> List.head |> Maybe.map .description |> Maybe.withDefault "")
                ]
            ]
        , div [ class "details-container" ]
            [ div [ class "details" ] [ text ("Wind Speed: " ++ String.fromFloat weather.wind.speed ++ " m/s") ]
            , div [ class "details" ] [ text ("Humidity: " ++ String.fromInt weather.main.humidity ++ "%") ]
            , div [ class "details" ] [ text ("Cloudiness: " ++ String.fromInt weather.clouds.all ++ "%") ]
            , div [ class "details feels-like" ] [ text ("Feels like: " ++ formatTemperature weather.main.feels_like ++ "°F") ]
            , div [ class "details" ] [ text ("Sunrise: " ++ formatTime weather.sys.sunrise) ]
            , div [ class "details" ] [ text ("Sunset: " ++ formatTime weather.sys.sunset) ]
            ]
        , div [ class "button-container" ]
            [ button [ class "refresh-button", onClick FetchWeather ] [ text "Fetch Weather" ]
            , button ((class "processed-data-button") :: (extraClass (isNothing maybeProcessedData))) [ text "Processed Data" ]
            ]
        ]
    ]

isNothing : Maybe a -> Bool
isNothing maybeValue =
    case maybeValue of
        Nothing ->
            True

        Just _ ->
            False
updateView : Model -> Html Msg
updateView model =
    div []
        (case model.weather of
            Just weather ->
                -- If `model.processedData` is of type `Maybe ProcessedData`
                -- and `viewWeatherData` expects `WeatherData -> Maybe ProcessedData -> List (Html Msg)`
                viewWeatherData weather model.processedData

            Nothing ->
                [ text "No weather data available." ] -- This now matches the List (Html msg) type
        )

viewProcessedData : ProcessedData -> List (Html Msg)
viewProcessedData pData =
    [ div [ class "avg-temp" ] [ text ("Average Temperature: " ++ String.fromFloat pData.avgTemperature ++ "°F") ]
    , div [ class "weather-trend" ] [ text ("Weather Trend: " ++ pData.trend) ]
    , div [ class "wind-report" ] [ text ("Wind Report: " ++ pData.windReport) ]
    ]

view : Model -> Html Msg
view model =
    div []
        [ if model.fetchButtonClicked then
            text ""
          else     
            button [ onClick FetchWeather, disabled model.isFetching, class "refresh-button" ] [ text "Fetch Weather" ]
        , div [ class "weather-container" ] 
            (case model.weather of
                Just weather -> 
                    -- Now correctly passing both arguments to `viewWeatherData`
                    viewWeatherData weather model.processedData
                Nothing -> 
                    [ text "No weather data available." ]
            )
        , div [ class "processed-data-container" ]
            (case model.processedData of
                Just pData -> viewProcessedData pData
                Nothing -> [ text "" ]
            )
        ]


-- HTTP REQUESTS

getWeather : Cmd Msg
getWeather =
    Http.get
        { url = "http://localhost:9000/weather"
        , expect = Http.expectJson ReceiveWeather weatherDataDecoder
        }

-- JSON DECODERS

coordinatesDecoder : Decoder Coordinates
coordinatesDecoder =
    map2 Coordinates
        (field "lon" float)
        (field "lat" float)

weatherDescriptionDecoder : Decoder WeatherDescription
weatherDescriptionDecoder =
    map4 WeatherDescription
        (field "id" int)
        (field "main" string)
        (field "description" string)
        (field "icon" string)

mainDecoder : Decoder Main
mainDecoder =
    map6 Main
        (field "temp" float)
        (field "feels_like" float)
        (field "temp_min" float)
        (field "temp_max" float)
        (field "pressure" int)
        (field "humidity" int)

windDecoder : Decoder Wind
windDecoder =
    map2 Wind
        (field "speed" float)
        (field "deg" int)

cloudsDecoder : Decoder Clouds
cloudsDecoder =
    map Clouds
        (field "all" int)

sysDecoder : Decoder Sys
sysDecoder =
    map5 Sys
        (field "sysType" (maybe int))
        (field "sysId" (maybe int))
        (field "country" string)
        (field "sunrise" int)
        (field "sunset" int)

weatherDataDecoder : Decoder WeatherData
weatherDataDecoder =
    succeed WeatherData
        |> andThen (\data ->
            field "coord" coordinatesDecoder
                |> andThen (\coord ->
                    field "weather" (list weatherDescriptionDecoder)
                        |> andThen (\weather ->
                            field "base" string
                                |> andThen (\base ->
                                    field "main" mainDecoder
                                        |> andThen (\mainData ->
                                            field "visibility" int
                                                |> andThen (\visibility ->
                                                    field "wind" windDecoder
                                                        |> andThen (\wind ->
                                                            field "clouds" cloudsDecoder
                                                                |> andThen (\clouds ->
                                                                    field "dt" int
                                                                        |> andThen (\dt ->
                                                                            field "sys" sysDecoder
                                                                                |> andThen (\sys ->
                                                                                    field "timezone" int
                                                                                        |> andThen (\timezone ->
                                                                                            field "id" int
                                                                                                |> andThen (\id ->
                                                                                                    field "name" string
                                                                                                        |> andThen (\name ->
                                                                                                            field "cod" int
                                                                                                                |> andThen (\cod ->
                                                                                                                    succeed
                                                                                                                        { coord = coord
                                                                                                                        , weather = weather
                                                                                                                        , base = base
                                                                                                                        , main = mainData
                                                                                                                        , visibility = visibility
                                                                                                                        , wind = wind
                                                                                                                        , clouds = clouds
                                                                                                                        , dt = dt
                                                                                                                        , sys = sys
                                                                                                                        , timezone = timezone
                                                                                                                        , id = id
                                                                                                                        , name = name
                                                                                                                        , cod = cod
                                                                                                                        }
                                                                                                                )
                                                                                                        )
                                                                                                )
                                                                                        )
                                                                                )
                                                                        )
                                                                )
                                                        )
                                                )
                                        )
                                )
                        )
                )
        )

processedDataDecoder : Decoder ProcessedData
processedDataDecoder =
    map3 ProcessedData
        (field "avgTemperature" float)
        (field "trend" string)
        (field "windReport" string)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
