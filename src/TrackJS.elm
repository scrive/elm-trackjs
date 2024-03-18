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

TODO: Get rid of rate limit? TrackJS does not seem to have it

If the HTTP request to Rollbar fails because of an exceeded rate limit (status
code 429), this will retry the HTTP request once per second, up to 60 times.

FIXME: example

    rollbar = TrackJS.scoped "Page/Home.elm"

    rollbar.debug "Hitting the hats API." Dict.empty

    [ ( "Payload", toString payload ) ]
        |> Dict.fromList
        |> rollbar.error "Unexpected payload from the hats API."

-}
scoped : Token -> CodeVersion -> Application -> TrackJS
scoped vtoken vcodeVersion vapplication =
    { error = send vtoken vcodeVersion vapplication retries.defaultMaxAttempts Error
    , warning = send vtoken vcodeVersion vapplication retries.defaultMaxAttempts Warning
    , info = send vtoken vcodeVersion vapplication retries.defaultMaxAttempts Info
    , debug = send vtoken vcodeVersion vapplication retries.defaultMaxAttempts Debug
    , log = send vtoken vcodeVersion vapplication retries.defaultMaxAttempts Debug
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
  - `Int` - maximum retry attempts - if the response is that the message was rate limited, try resending again (once per second) up to this many times. (0 means "do not retry.")
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
send : Token -> CodeVersion -> Application -> Int -> Level -> String -> Dict String String -> Task Http.Error Uuid
send vtoken vcodeVersion vapplication maxRetryAttempts level message metadata =
    Time.now
        |> Task.andThen (sendWithTime vtoken vcodeVersion vapplication maxRetryAttempts level message metadata)


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


sendWithTime : Token -> CodeVersion -> Application -> Int -> Level -> String -> Dict String String -> Posix -> Task Http.Error Uuid
sendWithTime (Token vtoken) vcodeVersion vapplication maxRetryAttempts level message metadata time =
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
        |> withRetry maxRetryAttempts


withRetry : Int -> Task Http.Error a -> Task Http.Error a
withRetry maxRetryAttempts task =
    let
        retry : Http.Error -> Task Http.Error a
        retry httpError =
            if maxRetryAttempts > 0 then
                case httpError of
                    Http.BadStatus statusCode ->
                        if statusCode == 429 then
                            -- Wait a bit between retries.
                            Process.sleep (Time.posixToMillis retries.msDelayBetweenRetries |> toFloat)
                                |> Task.andThen (\() -> withRetry (maxRetryAttempts - 1) task)

                        else
                            Task.fail httpError

                    _ ->
                        Task.fail httpError

            else
                Task.fail httpError
    in
    Task.onError retry task


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
            [ ( "application", Encode.string vapplication)

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


{-| According to <https://rollbar.com/docs/rate-limits/>
the default rate limit for all access tokens is 5,000 calls per minute.
This window resets every minute, so retry after waiting 1 sec, and default to
retrying up to 60 times.
-}
retries : { defaultMaxAttempts : Int, msDelayBetweenRetries : Posix }
retries =
    { defaultMaxAttempts = 60
    , msDelayBetweenRetries = Time.millisToPosix 1000
    }
