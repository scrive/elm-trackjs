module Example exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Task
import Time
import TrackJS exposing (TrackJS)


token : String
token =
    -- NOTE: TrackJS does not seem to have a demo token, so you need to use a valid one.
    -- Please make sure to create the application in TrackJS before you try this example.
    Debug.todo "00000000000000000000000000000000"


trackJSReporter : Maybe Time.Posix -> TrackJS
trackJSReporter time =
    let
        context =
            TrackJS.emptyContext
    in
    TrackJS.reporter
        (TrackJS.token token)
        (TrackJS.codeVersion "0.0.1")
        (TrackJS.application "elm-trackjs-example")
        { context | startTime = time }



-- MODEL --


type alias Model =
    { report : String
    , trackJS : TrackJS
    }


initialModel : Model
initialModel =
    { report = "Example report"
    , trackJS = trackJSReporter Nothing
    }



-- UPDATE --


type Msg
    = StartTime Time.Posix
    | SetText String
    | Send
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartTime time ->
            ( { model | trackJS = trackJSReporter (Just time) }, Cmd.none )

        SetText text ->
            ( { model | report = text }, Cmd.none )

        Send ->
            ( model, report model.trackJS model.report )

        NoOp ->
            ( model, Cmd.none )


report : TrackJS -> String -> Cmd Msg
report trackJS message =
    Task.attempt (\_ -> NoOp)
        (trackJS.report
            { message = message, url = "elm-trackjs-example/home", stackTrace = Nothing }
            (Dict.singleton "eg-key" "eg-value")
        )



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


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform StartTime Time.now )
