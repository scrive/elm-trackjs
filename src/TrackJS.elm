module TrackJS exposing
    ( TrackJS, Level(..), Token, token, Environment, environment, Scope, scope, CodeVersion, codeVersion
    , scoped, send
    )

{-| Send reports to TrackJS.


## Types

@docs TrackJS, Level, Token, token, Environment, environment, Scope, scope, CodeVersion, codeVersion


## Types

@docs scoped, send

-}

import Bitwise
import Dict exposing (Dict)
import Http
import Json.Encode as Encode exposing (Value)
import Murmur3
import Process
import Random
import Task exposing (Task)
import Time exposing (Posix)
import TrackJS.Internal
import Url.Builder
import Uuid exposing (Uuid, uuidGenerator)


{-| Functions preapplied with access tokens, scopes, and environments,
separated by [`Level`](#Level).

Create one using [`scoped`](#scoped).

-}
type alias TrackJS =
    { error : String -> Dict String String -> Task Http.Error Uuid
    , warning : String -> Dict String String -> Task Http.Error Uuid
    , info : String -> Dict String String -> Task Http.Error Uuid
    , debug : String -> Dict String String -> Task Http.Error Uuid
    , log : String -> Dict String String -> Task Http.Error Uuid
    }


{-| Severity levels.
-}
type Level
    = Error
    | Warning
    | Info
    | Debug
    | Log


{-| A TrackJS token, get it from [the install page](https://my.trackjs.com/install).

Create one using [`token`](#token).

    TrackJS.token "12c99de67a444c229fca100e0967486f"

-}
type Token
    = Token String


{-| A scope, for example `"login"`.

Create one using [`scope`](#scope).

    TrackJS.scope "login"

TODO what does this map to in TrackJS?

-}
type Scope
    = Scope String


{-| A code version, for example - a git commit hash.

Create one using [`codeVersion`](#codeVersion).

    TrackJS.codeVersion "24dcf3a9a9cf1a5e2ea319018644a68f4743a731"

-}
type CodeVersion
    = CodeVersion String


{-| Create a [`Scope`](#Scope).

    TrackJS.scope "login"

-}
scope : String -> Scope
scope =
    Scope


{-| Create a [`CodeVersion`](#CodeVersion).

    TrackJS.codeVersion "24dcf3a9a9cf1a5e2ea319018644a68f4743a731"

-}
codeVersion : String -> CodeVersion
codeVersion =
    CodeVersion


{-| For example, "production", "development", or "staging".

Create one using [`environment`](#environment).

    TrackJS.environment "production"

TODO TrackJS only has applications? How does env work?

-}
type Environment
    = Environment String


{-| Create a [`Token`](#token)

    TrackJS.token "12c99de67a444c229fca100e0967486f"

-}
token : String -> Token
token =
    Token


{-| Create an [`Environment`](#Environment)

    TrackJS.environment "production"

-}
environment : String -> Environment
environment =
    Environment


{-| Send a message to TrackJS. [`scoped`](#scoped)
provides a nice wrapper around this.

Arguments:

  - `Token` - The [TrackJS token](https://docs.trackjs.com/data-api/capture/#token) required to identify your account.
  - `Scope` - Scoping messages essentially namespaces them. For example, this might be the name of the page the user was on when the message was sent.
  - `Environment` - e.g. `"production"`, `"development"`, `"staging"`, etc.
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
send : Token -> CodeVersion -> Scope -> Environment -> Int -> Level -> String -> Dict String String -> Task Http.Error Uuid
send vtoken vcodeVersion vscope venvironment maxRetryAttempts level message metadata =
    Time.now
        |> Task.andThen (sendWithTime vtoken vcodeVersion vscope venvironment maxRetryAttempts level message metadata)



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


sendWithTime : Token -> CodeVersion -> Scope -> Environment -> Int -> Level -> String -> Dict String String -> Posix -> Task Http.Error Uuid
sendWithTime (Token vtoken) vcodeVersion vscope venvironment maxRetryAttempts level message metadata time =
    let
        uuid : Uuid
        uuid =
            uuidFrom (Token vtoken) vscope venvironment level message metadata time

        body : Http.Body
        body =
            toJsonBody (Token vtoken) vscope vcodeVersion venvironment level message uuid metadata
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
uuidFrom : Token -> Scope -> Environment -> Level -> String -> Dict String String -> Posix -> Uuid
uuidFrom (Token vtoken) (Scope vscope) (Environment venvironment) level message metadata time =
    let
        ms =
            Time.posixToMillis time

        hash : Int
        hash =
            [ Encode.string (levelToString level)
            , Encode.string message
            , Encode.string vtoken
            , Encode.string vscope
            , Encode.string venvironment

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
toJsonBody : Token -> Scope -> CodeVersion -> Environment -> Level -> String -> Uuid -> Dict String String -> Http.Body
toJsonBody (Token vtoken) (Scope vscope) (CodeVersion vcodeVersion) (Environment venvironment) level message uuid metadata =
    -- TODO or FIXME use: vscope, venvironment, metadata
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
            -- TODO message req?
            -- Severity of the console event. {String:"log","debug","info","warn","error"}
            [ ( "severity", Encode.string (levelToString level) )

            -- Timestamp of the console event. {String ISO 8601}
            -- TODO timestamp req?
            ]
      )
    , ( "customer"
      , Encode.object
            -- Application key. Generated this in your TrackJS Dashboard. {String}
            [ ( "application", Debug.todo "TODO: application name" )

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

    -- TODO environment?
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
    -- TODO url?
    -- SKIPPED: stack: Stack trace of the error. {String}
    -- SKIPPED: throttled: Number of errors that have been dropped by the agent throttles before this one. {Number}
    -- Timestamp when the error occurred. {String ISO 8601}
    -- TODO timestamp
    -- SKIPPED: visitor: Visitor Telemetry events. {Array[Object]}
    ]
        |> Encode.object
        |> Http.jsonBody


{-| Return a [`Rollbar`](#Rollbar) record configured with the given
[`Environment`](#Environment) and [`Scope`](#Scope) string.

TODO: Get rid of rate limit? TrackJS does not seem to have it
If the HTTP request to Rollbar fails because of an exceeded rate limit (status
code 429), this will retry the HTTP request once per second, up to 60 times.

    rollbar = TrackJS.scoped "Page/Home.elm"

    rollbar.debug "Hitting the hats API." Dict.empty

    [ ( "Payload", toString payload ) ]
        |> Dict.fromList
        |> rollbar.error "Unexpected payload from the hats API."

-}
scoped : Token -> CodeVersion -> Environment -> String -> TrackJS
scoped vtoken vcodeVersion venvironment scopeStr =
    let
        vscope =
            Scope scopeStr
    in
    { error = send vtoken vcodeVersion vscope venvironment retries.defaultMaxAttempts Error
    , warning = send vtoken vcodeVersion vscope venvironment retries.defaultMaxAttempts Warning
    , info = send vtoken vcodeVersion vscope venvironment retries.defaultMaxAttempts Info
    , debug = send vtoken vcodeVersion vscope venvironment retries.defaultMaxAttempts Debug
    , log = send vtoken vcodeVersion vscope venvironment retries.defaultMaxAttempts Debug
    }


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
