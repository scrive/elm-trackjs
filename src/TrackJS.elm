module TrackJS exposing
    ( TrackJS, reporter, Report
    , Token, token
    , CodeVersion, codeVersion
    , Application, application
    , Context, emptyContext
    , send
    )

{-| Send reports to TrackJS.


## Send reports

@docs TrackJS, reporter, Report


### Types

@docs Token, token
@docs CodeVersion, codeVersion
@docs Application, application
@docs Context, emptyContext


## Low-level

@docs send

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
import Url
import Url.Builder
import Uuid exposing (Uuid, uuidGenerator)


{-| Pre-applied function with access token, code version, application, and context.

Create one using [`reporter`](#reporter).

-}
type alias TrackJS =
    { report : Report -> Dict String String -> Task Http.Error ()
    }


{-| Return a [`TrackJS`](#TrackJS) record configured with the supplied values.

Once you have a `TrackJS` value you can use it to send a report:

  - TODO: re-add example

_Note:_ Requests to TrackJS are subject to two limits:

1.  The total payload should be less than 100 kB
2.  They should not exceed the [Capture Throttle](https://docs.trackjs.com/data-management/limits/#capture-throttle)

Requests that are too large will result in `413 Payload Too Large` and this
library does not prevent you from exceeding the limit or provide any mechanism
to know if you are.

If you exceed the rate limit, these will show as "Dropped" in your
[usage statistics](https://my.trackjs.com/usage), and will not error, so
unfortuntaely we cannot reliably retry throttled requests.
Please read the TrackJS documentation for more details.

-}
reporter : Token -> CodeVersion -> Application -> Context -> TrackJS
reporter vtoken vcodeVersion vapplication context =
    { report = send vtoken vcodeVersion vapplication context
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


{-| Optional context to send to TrackJS.

Use `emptyContext` if you don't have or want to provide this information,
although you will get more value from TrackJS if you do.

These values are used primarily for the `environment` field in the TrackJS
schema, the documentation for which suggests these values should be:

  - `sessionId`: Customer-generated Id for the current browser session.
  - `userId`: Customer-generated Id for the current user.
  - `startTime`: Used to compute the `age`: Time elapsed from the page load until the error in milliseconds.
  - `originalUrl`: URL of the document when the page loaded.
  - `referrer`: Referrer URL to this document.
  - `userAgent`: Browser User Agent string.
  - `viewport`: Visitor viewport height and width.

_Note:_ TrackJS silently drops reports which do not match its schema, and the
documentation on the schema is not very precise. So if you provide "strange"
values here, your reports may be silently dropped. Only limited experimentation
was done to ensure this library works, please open an issue if you have
questions or useful information.

-}
type alias Context =
    { sessionId : String
    , userId : String
    , startTime : Maybe Posix
    , originalUrl : String
    , referrer : String
    , userAgent : String
    , viewport : { h : Int, w : Int }
    }


{-| A default empty [`Context`](#Context).

Uses empty strings and `0` values as default, which TrackJS accepts, excepts
for `userAgent` which is set to `"unknown"`.

-}
emptyContext : Context
emptyContext =
    { sessionId = ""
    , userId = ""
    , startTime = Nothing
    , originalUrl = ""
    , referrer = ""
    , userAgent = "unknown"
    , viewport = { h = 0, w = 0 }
    }


{-| An error report with information about the current state of the application.

  - `message` is the headline error message
  - `url` does not need to be a valid URL but must be percent-encoded, you can
    either use the full current URL of your application, or something that will
    help you identify where in your application the report was generated
  - `stackTrace` can be used to provide arbitrary additional information, such as a JSON
    decoding error string (keep the size limit in mind)

-}
type alias Report =
    { message : String
    , url : String
    , stackTrace : Maybe String
    }


{-| Send a message to TrackJS.
[`reporter`](#reporter) provides a nice wrapper around this.

Arguments:

  - `Token` - The [TrackJS token](https://docs.trackjs.com/data-api/capture/#token) required to identify your account.
  - `CodeVersion` - A version for your current application.
  - `Application` - Registered in [TrackJS](https://my.trackjs.com/Account/Applications)
  - `Context` - TODO documentation
  - `Report` - TODO documentation
  - `String` - message, e.g. "Auth server was down when user tried to sign in."
  - `Dict String String` - arbitrary metadata key-value pairs

If the message was successfully sent to TrackJS, the [`Task`](http://package.elm-lang.org/packages/elm-lang/core/latest/Task#Task)
succeeds.
Otherwise it fails with the
[`Http.Error`](http://package.elm-lang.org/packages/elm-lang/http/latest/Http#Error)
responsible (however note that [TrackJS always responds](https://docs.trackjs.com/data-api/capture/)
with `200 OK` or `202 ACCEPTED`).

-}
send : Token -> CodeVersion -> Application -> Context -> Report -> Dict String String -> Task Http.Error ()
send vtoken vcodeVersion vapplication context report metadata =
    Time.now
        |> Task.andThen (sendWithTime vtoken vcodeVersion vapplication context report metadata)


sendWithTime : Token -> CodeVersion -> Application -> Context -> Report -> Dict String String -> Posix -> Task Http.Error ()
sendWithTime (Token vtoken) vcodeVersion vapplication context report metadata time =
    let
        body : Http.Body
        body =
            toJsonBody (Token vtoken) vcodeVersion vapplication context report metadata time
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


{-| Tries to generate a correlationId that might be unique to the current
"user", in the vague sense of a browser tab session, by using various bits of
information.
-}
correlationIdFrom : CodeVersion -> Context -> Report -> Posix -> Uuid
correlationIdFrom (CodeVersion vcodeVersion) context report time =
    let
        ( itemsToHash, seed ) =
            if context == emptyContext then
                -- If the context is empty, there's no easy way to identify a
                -- user, so use the code version and report as hash items,
                -- with the current time as seed, to generate a semi-unique
                -- hash
                ( [ Encode.string vcodeVersion
                  , Encode.string report.message
                  , Encode.string report.url
                  , Encode.string <| Maybe.withDefault "" report.stackTrace
                  ]
                , Time.posixToMillis time
                )

            else
                -- Otherwise use the context information in the hash items,
                -- which should be relatively stable for a user, and the user's
                -- `startTime` as a seed (if possible) to try and correlate
                -- errors
                ( [ Encode.string vcodeVersion
                  , Encode.string context.sessionId
                  , Encode.string context.userId
                  , Encode.string context.originalUrl
                  , Encode.string context.referrer
                  , Encode.string context.userAgent
                  , Encode.int context.viewport.h
                  , Encode.int context.viewport.w
                  ]
                , Maybe.withDefault 0 <| Maybe.map Time.posixToMillis context.startTime
                )

        hash : Int
        hash =
            itemsToHash
                |> Encode.list identity
                |> Encode.encode 0
                |> Murmur3.hashString seed
    in
    Random.initialSeed hash
        |> Random.step uuidGenerator
        |> Tuple.first


{-| See <https://docs.trackjs.com/data-api/capture/#request-payload> for schema
-}
toJsonBody : Token -> CodeVersion -> Application -> Context -> Report -> Dict String String -> Posix -> Http.Body
toJsonBody (Token vtoken) (CodeVersion vcodeVersion) (Application vapplication) context report metadata time =
    -- The source platform of the capture. Typically "browser" or "node". {String}
    [ ( "agentPlatform", Encode.string "browser-elm" )

    -- Version of the TrackJS Agent. {String Semver}
    , ( "version", Encode.string TrackJS.Internal.version )

    -- SKIPPED: Async Stack Trace content. {String}
    , ( "bindStack", Encode.null )

    -- SKIPPED: Timestamp when the "bindStack" was generated. {ISO 8601 String}
    , ( "bindTime", Encode.null )

    -- SKIPPED: Console Telemetry events. {Array[Object]}
    , ( "console", Encode.list Encode.object [] )
    , ( "customer"
      , Encode.object
            -- Application key. Generated this in your TrackJS Dashboard. {String}
            [ ( "application", Encode.string vapplication )

            -- Auto-generated ID for matching visitor to multiple errors. {String}
            , ( "correlationId"
              , Uuid.encode <|
                    correlationIdFrom (CodeVersion vcodeVersion) context report time
              )

            -- Customer-generated Id for the current browser session. {String}
            , ( "sessionId", Encode.string context.sessionId )

            -- Your account token, generated by TrackJS. {String}
            , ( "token", Encode.string vtoken )

            -- Customer-generated Id for the current user. {String}
            , ( "userId", Encode.string context.userId )

            -- Customer-generated Id for the current application version. {String}
            , ( "version", Encode.string vcodeVersion )
            ]
      )

    -- Entry point of the error. {String:"ajax","direct","catch","console","window"}
    , ( "entry", Encode.string "direct" )

    -- environment: we use Context, as "environment" may be misleading (e.g. prod)
    , ( "environment"
      , Encode.object
            [ -- Time elapsed from the page load until the error in milliseconds. {Number}
              ( "age"
              , Encode.int <|
                    case context.startTime of
                        Nothing ->
                            0

                        Just t0 ->
                            Time.posixToMillis time - Time.posixToMillis t0
              )

            -- SKIPPED: List of other libraries in the document.
            , ( "dependencies", Encode.object [] )

            -- URL of the document when the page loaded. {String URL}
            , ( "originalUrl", Encode.string context.originalUrl )

            -- Referrer URL to this document. {String URL}
            , ( "referrer", Encode.string context.referrer )

            -- Browser User Agent string. {String UserAgent}
            , ( "userAgent", Encode.string context.userAgent )

            -- Visitor viewport height. {Number}
            , ( "viewportHeight", Encode.int context.viewport.h )

            -- Visitor viewport width. {Number}
            , ( "viewportWidth", Encode.int context.viewport.w )
            ]
      )

    -- SKIPPED: file: Filename originating the error. {String filename}
    -- Error message. {String}
    , ( "message", Encode.string report.message )

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

    -- SKIPPED: Navigation Telemetry events. {Array[Object]}
    , ( "nav", Encode.list (always Encode.null) [] )

    -- SKIPPED: Network Telemetry events. {Array[Object]}
    , ( "network", Encode.list (always Encode.null) [] )

    -- URL of the document when the error occurred. {String URL}
    , ( "url", Encode.string report.url )

    -- Stack trace of the error. {String}
    , ( "stack", Encode.string <| Maybe.withDefault "" report.stackTrace )

    -- SKIPPED: throttled: Number of errors that have been dropped by the agent throttles before this one. {Number}
    , ( "throttled", Encode.int 0 )

    -- Timestamp when the error occurred. {String ISO 8601}
    , ( "timestamp", Iso8601.encode time )

    -- SKIPPED: Visitor Telemetry events. {Array[Object]}
    , ( "visitor", Encode.list (always Encode.null) [] )
    ]
        |> Encode.object
        |> Encode.encode 0
        |> Http.stringBody "text/plain"
