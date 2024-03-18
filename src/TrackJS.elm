module TrackJS exposing
    ( TrackJS, scoped
    , Token, token, CodeVersion, codeVersion, Application, application
    , send, Level(..)
    )

{-| Send reports to TrackJS.


## Send reports

@docs TrackJS, scoped


### Types

@docs Token, token, CodeVersion, codeVersion, Application, application


## Low-level

@docs send, Level

-}

import Bitwise
import Dict exposing (Dict)
import Http
import Iso8601
import Json.Encode as Encode exposing (Value)
import Murmur3
import Process
import Random
import Task exposing (Task)
import Time exposing (Posix)
import TrackJS.Internal
import Url.Builder
import Uuid exposing (Uuid, uuidGenerator)


{-| Functions preapplied with access token and application, separated by
[`Level`](#Level).

Create one using [`scoped`](#scoped).

-}
type alias TrackJS =
    { error : String -> Dict String String -> Task Http.Error Uuid
    , warning : String -> Dict String String -> Task Http.Error Uuid
    , info : String -> Dict String String -> Task Http.Error Uuid
    , debug : String -> Dict String String -> Task Http.Error Uuid
    , log : String -> Dict String String -> Task Http.Error Uuid
    }


{-| Return a [`TrackJS`](#TrackJS) record configured with the given
[`Application`](#Application).

Requests to TrackJS are subject to two limits:

1.  The total payload should be less than 100 kB
2.  They should not exceed the [Capture Throttle](https://docs.trackjs.com/data-management/limits/#capture-throttle)

Requests that are too large will result in `413 Payload Too Large` and this
library does not prevent you from exceeding the limit or provide any mechanism
to know if you are.

If you exceed the rate limit, these will show as "Dropped" in your
[usage statistics](https://my.trackjs.com/usage), and will not error, so
unfortuntaely we cannot reliably retry throttled requests.
Please read the TrackJS documentation for more details.

  - FIXME: Can we do better re. 413 Payload Too Large?
  - TODO: re-add example

-}
scoped : Token -> CodeVersion -> Application -> TrackJS
scoped vtoken vcodeVersion vapplication =
    { error = send vtoken vcodeVersion vapplication Error
    , warning = send vtoken vcodeVersion vapplication Warning
    , info = send vtoken vcodeVersion vapplication Info
    , debug = send vtoken vcodeVersion vapplication Debug
    , log = send vtoken vcodeVersion vapplication Debug
    }


{-| A TrackJS token, get it from [the install page](https://my.trackjs.com/install).

Create one using [`token`](#token).

    TrackJS.token "12c99de67a444c229fca100e0967486f"

-}
type Token
    = Token String


{-| Create a [`Token`](#token)

    TrackJS.token "12c99de67a444c229fca100e0967486f"

-}
token : String -> Token
token =
    Token


{-| A code version, for example - a git commit hash.

Create one using [`codeVersion`](#codeVersion).

    TrackJS.codeVersion "24dcf3a9a9cf1a5e2ea319018644a68f4743a731"

-}
type CodeVersion
    = CodeVersion String


{-| Create a [`CodeVersion`](#CodeVersion).

    TrackJS.codeVersion "24dcf3a9a9cf1a5e2ea319018644a68f4743a731"

-}
codeVersion : String -> CodeVersion
codeVersion =
    CodeVersion


{-| A TrackJS application created [in your settings page](https://my.trackjs.com/Account/Applications).

Create one using [`application`](#application).

    TrackJS.application "my-application-name-production"

-}
type Application
    = Application String


{-| Create a [`Application`](#Application).

As TrackJS does not have the concept of environments, you should choose how you
want to separate environments (_e.g._ by application, using metadata, code
version). This library leaves the choice up to you!

    TrackJS.application "my-application-name-production"

-}
application : String -> Application
application =
    Application


{-| Send a message to TrackJS. [`scoped`](#scoped)
provides a nice wrapper around this.

Arguments:

  - `Token` - The [TrackJS token](https://docs.trackjs.com/data-api/capture/#token) required to identify your account.
  - `CodeVersion` - A version for your current application.
  - `Application` - Registered in [TrackJS](https://my.trackjs.com/Account/Applications)
  - `Level` - severity, e.g. `Error`, `Warning`, `Info`
  - `String` - message, e.g. "Auth server was down when user tried to sign in."
  - `Dict String String` - arbitrary metadata key-value pairs

If the message was successfully sent to TrackJS, the [`Task`](http://package.elm-lang.org/packages/elm-lang/core/latest/Task#Task)
succeeds with the [`Uuid`](http://package.elm-lang.org/packages/danyx23/elm-uuid/latest/Uuid#Uuid)
it generated and sent to TrackJS to identify the message. Otherwise it fails
with the [`Http.Error`](http://package.elm-lang.org/packages/elm-lang/http/latest/Http#Error)
responsible (however note that [TrackJS always responds](https://docs.trackjs.com/data-api/capture/)
with `200 OK` or `202 ACCEPTED`).

-}
send : Token -> CodeVersion -> Application -> Level -> String -> Dict String String -> Task Http.Error Uuid
send vtoken vcodeVersion vapplication level message metadata =
    Time.now
        |> Task.andThen (sendWithTime vtoken vcodeVersion vapplication level message metadata)


{-| Severity levels.
-}
type Level
    = Error
    | Warning
    | Info
    | Debug
    | Log



-- INTERNAL --


levelToString : Level -> String
levelToString report =
    case report of
        Error ->
            "error"

        Warning ->
            "warn"

        Info ->
            "info"

        Debug ->
            "debug"

        Log ->
            "log"


sendWithTime : Token -> CodeVersion -> Application -> Level -> String -> Dict String String -> Posix -> Task Http.Error Uuid
sendWithTime (Token vtoken) vcodeVersion vapplication level message metadata time =
    let
        uuid : Uuid
        uuid =
            uuidFrom (Token vtoken) vapplication level message metadata time

        body : Http.Body
        body =
            toJsonBody (Token vtoken) vcodeVersion vapplication level message uuid metadata time
    in
    -- POST https://capture.trackjs.com/capture?token={TOKEN}&v={AGENT_VERSION}
    { method = "POST"
    , headers = []
    , url =
        Url.Builder.crossOrigin "https://capture.trackjs.com"
            [ "capture" ]
            [ Url.Builder.string "token" vtoken
            , Url.Builder.string "v" TrackJS.Internal.version
            ]
    , body = body
    , resolver = Http.stringResolver (\_ -> Ok ()) -- TODO
    , timeout = Nothing
    }
        |> Http.task
        |> Task.map (\() -> uuid)


{-| Using the current system time as a random number seed generator, generate a
UUID.

We could theoretically generate the same UUID twice if we tried to send
two messages in extremely rapid succession. To guard against this, we
incorporate the contents of the message in the random number seed so that the
only way we could expect the same UUID is if we were sending a duplicate
message.

-}
uuidFrom : Token -> Application -> Level -> String -> Dict String String -> Posix -> Uuid
uuidFrom (Token vtoken) (Application vapplication) level message metadata time =
    let
        ms =
            Time.posixToMillis time

        hash : Int
        hash =
            [ Encode.string (levelToString level)
            , Encode.string message
            , Encode.string vtoken
            , Encode.string vapplication

            -- FIXME update to work: , Encode.dict identity identity metadata
            ]
                |> Encode.list identity
                |> Encode.encode 0
                |> Murmur3.hashString ms

        combinedSeed =
            Bitwise.xor (floor (ms |> toFloat)) hash
    in
    Random.initialSeed combinedSeed
        |> Random.step uuidGenerator
        |> Tuple.first


{-| See <https://docs.trackjs.com/data-api/capture/#request-payload> for schema
-}
toJsonBody : Token -> CodeVersion -> Application -> Level -> String -> Uuid -> Dict String String -> Posix -> Http.Body
toJsonBody (Token vtoken) (CodeVersion vcodeVersion) (Application vapplication) level message uuid metadata time =
    -- The source platform of the capture. Typically "browser" or "node". {String}
    [ ( "agentPlatform", Encode.string "browser-elm" )

    -- Version of the TrackJS Agent. {String Semver}
    , ( "version", Encode.string TrackJS.Internal.version )

    -- SKIPPED: bindStack: Async Stack Trace content. {String}
    -- SKIPPED: bindTime: Timestamp when the "bindStack" was generated. {ISO 8601 String}
    -- Console Telemetry events. {Array[Object]}
    , ( "console"
      , Encode.object
            -- Formatted console message string. {String}
            [ ( "message", Encode.string message )

            -- Severity of the console event. {String:"log","debug","info","warn","error"}
            , ( "severity", Encode.string (levelToString level) )

            -- Timestamp of the console event. {String ISO 8601}
            , ( "timestamp", Iso8601.encode time )
            ]
      )
    , ( "customer"
      , Encode.object
            -- Application key. Generated this in your TrackJS Dashboard. {String}
            [ ( "application", Encode.string vapplication )

            -- Auto-generated ID for matching visitor to multiple errors. {String}
            -- FIXME seems like this should not be the error UUID but per user session
            , ( "correlationId", Uuid.encode uuid )

            -- Customer-generated Id for the current browser session. {String}
            -- TODO sessionId? How is it different?
            -- Your account token, generated by TrackJS. {String}
            , ( "token", Encode.string vtoken )

            -- Customer-generated Id for the current user. {String}
            -- TODO userId?
            -- Customer-generated Id for the current application version. {String}
            , ( "version", Encode.string vcodeVersion )
            ]
      )

    -- Entry point of the error. {String:"ajax","direct","catch","console","window"}
    , ( "entry", Encode.string "direct" )

    -- SKIPPED: environment
    -- SKIPPED: file: Filename originating the error. {String filename}
    -- Error message. {String}
    , ( "message", Encode.string message )

    -- Customer-provided metadata keys describing the current context. {Array[Object]}
    , ( "metadata"
        -- key: Metadata key. {String}
        -- value: Metadata value. {String}
      , Dict.toList metadata
            |> Encode.list
                (\( k, v ) ->
                    Encode.object
                        [ ( "key", Encode.string k )
                        , ( "value", Encode.string v )
                        ]
                )
      )

    -- SKIPPED: nav: Navigation Telemetry events. {Array[Object]}
    -- SKIPPED: network: Network Telemetry events. {Array[Object]}
    -- URL of the document when the error occurred. {String URL}
    -- SKIPPED: url: URL of the document when the error occurred. {String URL}
    -- SKIPPED: stack: Stack trace of the error. {String}
    -- SKIPPED: throttled: Number of errors that have been dropped by the agent throttles before this one. {Number}
    -- Timestamp when the error occurred. {String ISO 8601}
    , ( "timestamp", Iso8601.encode time )

    -- SKIPPED: visitor: Visitor Telemetry events. {Array[Object]}
    ]
        |> Encode.object
        |> Http.jsonBody
