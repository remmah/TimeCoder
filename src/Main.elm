module Main exposing (main)

import Browser
import Browser.Events as Events
import Json.Decode as Decode
import Html exposing (..)
import Html.Events exposing (onClick)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { elapsedTime : Int
    , stopwatchIsRunning : Bool
    , timestamps : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { elapsedTime = 0 -- use 3595 to test hour carryover quickly
      , stopwatchIsRunning = False
      , timestamps = []
      }
    , Cmd.none
    )


    
keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


-- UPDATE


type Msg
    = IncrementElapsedTime Time.Posix
    | RunStopwatch
    | HaltStopwatch
    | LogTimestamp
    | KeyPressed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementElapsedTime time ->
            ( { model | elapsedTime = model.elapsedTime + 1 }
            , Cmd.none
            )

        RunStopwatch ->
            ( { model | stopwatchIsRunning = True }
            , Cmd.none
            )

        HaltStopwatch ->
            ( { model | stopwatchIsRunning = False }
            , Cmd.none
            )
            
        LogTimestamp ->
            ( { model | timestamps = model.timestamps ++ [(model.elapsedTime)] }
            , Cmd.none
            )
            
        KeyPressed key ->
            if (key == " ") then
                update LogTimestamp model
            else
            ( model
            , Cmd.none
            )
            

        --_ ->
          --  ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.stopwatchIsRunning then
            Time.every 1000 IncrementElapsedTime
        else
            Sub.none
        , Events.onKeyPress (Decode.map (KeyPressed) keyDecoder)
        ]


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick RunStopwatch ] [ text "Start" ]
        , button [ onClick HaltStopwatch ] [ text "Stop" ]
        , div [] [ text (timestampStringView model.elapsedTime) ]
        , button [ onClick LogTimestamp ] [ text "Log Current Timestamp" ]
        , div [] [ text "Timestamps:" ]
        , div [] (List.map timestampDivView model.timestamps)
        ]



-- Timestamp formatting functions


timestampStringView : Int -> String
timestampStringView timestamp =
    let
        hours = extractDisplayHours timestamp
        minutes = extractDisplayMinutes timestamp
        seconds = extractDisplaySeconds timestamp
    in
    hours ++ ":" ++ minutes ++ ":" ++ seconds

timestampDivView : Int -> Html Msg
timestampDivView timestamp =
    div []
        [ text (timestampStringView timestamp) ]
        
extractDisplayHours : Int -> String
extractDisplayHours time =
    time // 3600
    |> String.fromInt

extractDisplayMinutes : Int -> String
extractDisplayMinutes time =
    if ( ( time - ( secondsTakenUpByHours time ) ) // 60 ) > 9 then 
        ( time - ( secondsTakenUpByHours time ) ) // 60
        |> String.fromInt
    else 
        "0" ++ String.fromInt ( ( time - ( secondsTakenUpByHours time ) ) // 60 )

extractDisplaySeconds : Int -> String
extractDisplaySeconds time =
    if (time - (secondsTakenUpByMinutes time) ) > 9 then
        time - (secondsTakenUpByMinutes time)
        |> String.fromInt
    else
        "0" ++ String.fromInt ( time - (secondsTakenUpByMinutes time) )

secondsTakenUpByHours : Int -> Int
secondsTakenUpByHours time =
    ( time // 3600 ) * 3600

secondsTakenUpByMinutes : Int -> Int
secondsTakenUpByMinutes time =
    ( time // 60 ) * 60