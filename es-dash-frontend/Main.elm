module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


readServer : String
readServer =
    "ws://localhost:3000"


writeServer : String
writeServer =
    "ws://localhost:3001"



-- MODEL


type alias Model =
    { input : String
    , events : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )



-- UPDATE


type Msg
    = Input String
    | Send
    | NewEvent String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { input, events } =
    case msg of
        Input newInput ->
            ( Model newInput events, Cmd.none )

        Send ->
            ( Model "" events, WebSocket.send writeServer input )

        NewEvent str ->
            ( Model input (str :: events), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen readServer NewEvent



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput Input, value model.input ] []
        , button [ onClick Send ] [ text "Send" ]
        , div [] <| List.map viewMessage model.events
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
