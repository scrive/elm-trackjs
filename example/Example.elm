module Example exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Task
import TrackJS exposing (TrackJS)


token : String
token =
    -- NOTE: TrackJS does not seem to have a demo token, so you need to use a valid one.
    -- Please make sure to create the application in TrackJS before you try this example.
    Debug.todo "00000000000000000000000000000000"


trackJs : TrackJS
trackJs =
    TrackJS.scoped
        (TrackJS.token token)
        (TrackJS.codeVersion "0.0.1")
        (TrackJS.application "elm-trackjs-example")
        TrackJS.emptyContext



-- MODEL --


type alias Model =
    { report : String
    }


initialModel : Model
initialModel =
    { report = "Example report"
    }



-- UPDATE --


type Msg
    = SetText String
    | NoOp
    | Send


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetText text ->
            ( { model | report = text }, Cmd.none )

        Send ->
            ( model, info model.report )


info : String -> Cmd Msg
info report =
    Task.attempt (\_ -> NoOp) (trackJs.info report (Dict.singleton "eg-key" "eg-value"))



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput SetText, value model.report ] []
        , button [ onClick Send ] [ text "Send to TrackJS" ]
        ]



-- INIT --


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , subscriptions = always Sub.none
        , update = update
        , view = \model -> { title = "Elm TrackJS Example", body = [ view model ] }
        }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )
