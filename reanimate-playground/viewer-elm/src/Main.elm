module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Fps
import Html exposing (Html)
import Html.Attributes as Attr exposing (class, disabled, href, id, rel, src, style, title, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Keyboard exposing (RawKey)
import List
import Time
import Platform.Sub
import Ports
import Task
import WebSocket


type Backend
    = Production
    | Local


backend : Backend
backend =
    Production


wsBackend : String
wsBackend =
    case backend of
        Production ->
            "wss://reanimate.clozecards.com/ws/"

        Local ->
            "ws://localhost:10161/"


webBackend : String
webBackend =
    case backend of
        Production ->
            "https://reanimate.clozecards.com/"

        Local ->
            "http://localhost:10162/"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Platform.Sub.batch
        [ Ports.receiveSocketMsg (WebSocket.receive MessageReceived)
        , Ports.receiveEditorMsg Change
        , Ports.receiveControlMsg parseControlMsg
        , case model of
            Animating { player } ->
                case player of
                    Playing _ ->
                        Browser.Events.onAnimationFrameDelta TimeDeltaReceived

                    Paused ->
                        Sub.none

            Problem ConnectionFailed ->
                Time.every 2000 (always AttemptReconnect)
            _ ->
                Sub.none
        ]


parseControlMsg : String -> Msg
parseControlMsg msg =
    case msg of
        "pause" ->
            Pause

        "play" ->
            Play

        "seek1" ->
            Seek 1

        "seek10" ->
            Seek 10

        "seek-1" ->
            Seek -1

        "seek-10" ->
            Seek -10

        _ ->
            NoOp


type Msg
    = MessageReceived (Result Json.Decode.Error WebSocket.WebSocketMsg)
    | TimeDeltaReceived Float
    | AttemptReconnect
    | Pause
    | Play
    | Seek Int
    | NoOp
    | Change String


type Model
    = Disconnected
    | Connected
    | Compiling
    | Animating Animation
    | Problem Problem


type alias Animation =
    { frameCount : Int
    , frames : Frames
    , frameIndex : Int
    , player : Player
    , bestFrame : Maybe String
    , frameDeltas : List Float
    }


initAnimation : Int -> Animation
initAnimation frameCount =
    { frameCount = frameCount
    , frames = Dict.empty
    , frameIndex = 0
    , player = Playing 0
    , bestFrame = Nothing
    , frameDeltas = Fps.init
    }


type Player
    = Paused
      -- Float = time in milliseconds from the beginning/resuming of the animation
    | Playing Float


type Problem
    = CompilationError String
    | ConnectionFailed
    | PortMessageDecodeFailure Json.Decode.Error
    | UnexpectedMessage String


type alias Frames =
    Dict Int String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Disconnected, connectCommand )


connectCommand : Cmd msg
connectCommand =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Connect
            { name = "TheSocket"
            , address = wsBackend
            , protocol = ""
            }


sendSource : String -> Cmd msg
sendSource txt =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Send
            { name = "TheSocket"
            , content = txt
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeDeltaReceived delta ->
            ( updateAnimation
                (\({ frameCount, frames, player, frameDeltas } as animation) ->
                    case player of
                        Playing time ->
                            let
                                newTime =
                                    time + delta

                                newFrameIndex =
                                    frameIndexAt newTime frameCount

                                hasNewFrame =
                                    Dict.member newFrameIndex frames
                            in
                            { animation
                                | player = Playing newTime
                                , frameIndex = newFrameIndex
                                , bestFrame = lookupBestFrame newFrameIndex frames
                                , frameDeltas = Fps.update hasNewFrame delta frameDeltas
                            }

                        Paused ->
                            animation
                )
                model
            , Cmd.none
            )

        Play ->
            ( updateAnimation
                (\({ frameCount, frameIndex, player } as animation) ->
                    case player of
                        Paused ->
                            let
                                newTime =
                                    -- resume from currently displayed frame
                                    toFloat frameIndex / framesPerMillisecond
                            in
                            { animation
                                | player = Playing newTime
                                , frameIndex = frameIndexAt newTime frameCount
                            }

                        Playing _ ->
                            animation
                )
                model
            , blurPlayOrPause
            )

        Seek delta ->
            ( updateAnimation
                (\({ player, frameIndex, frameCount, frames } as animation) ->
                    case player of
                        Paused ->
                            let
                                newFrameIndex =
                                    (frameIndex + delta) |> modBy frameCount
                            in
                            { animation
                                | bestFrame = lookupBestFrame newFrameIndex frames
                                , frameIndex = newFrameIndex
                            }

                        Playing _ ->
                            animation
                )
                model
            , Cmd.none
            )

        MessageReceived result ->
            ( processResult result model, Cmd.none )

        Pause ->
            ( updateAnimation (\animation -> { animation | player = Paused }) model
            , blurPlayOrPause
            )

        AttemptReconnect ->
            ( model, connectCommand )

        NoOp ->
            ( model, Cmd.none )

        Change txt ->
            ( model, sendSource txt )


lookupBestFrame : Int -> Frames -> Maybe String
lookupBestFrame frameIndex frames =
    case Dict.get frameIndex frames of
        Just svgUrl ->
            Just svgUrl

        Nothing ->
            -- The specific frame in question is not loaded yet => approximate lookup
            List.head (List.reverse (Dict.values (Dict.filter (\x _ -> x <= frameIndex) frames)))


updateAnimation : (Animation -> Animation) -> Model -> Model
updateAnimation f model =
    case model of
        Animating animation ->
            Animating (f animation)

        other ->
            other


playOrPauseId : String
playOrPauseId =
    "play-or-pause"


{-| Hack: Mouse click on Play/Pause focuses the button.
Without this hack pressing SPACE with the button focused triggers the Play/Pause event twice
-}
blurPlayOrPause : Cmd Msg
blurPlayOrPause =
    Task.attempt (always NoOp) (Browser.Dom.blur playOrPauseId)


processResult : Result Json.Decode.Error WebSocket.WebSocketMsg -> Model -> Model
processResult result model =
    case result of
        Err decodeError ->
            Problem (PortMessageDecodeFailure decodeError)

        Ok wsMsg ->
            case wsMsg of
                WebSocket.Error { error } ->
                    Problem (UnexpectedMessage error)

                WebSocket.Data { data } ->
                    processMessage data model


processMessage : String -> Model -> Model
processMessage data model =
    case String.lines data of
        [ "connection established" ] ->
            Connected

        [ "connection failed" ] ->
            Problem ConnectionFailed

        [ "status", status ] ->
            case status of
                "Compiling" ->
                    Compiling

                "Done" ->
                    -- TODO there's probably no need for Done message, as frontend doesn't need to do anything special
                    model

                _ ->
                    Problem (UnexpectedMessage ("Unknown status: '" ++ status ++ "'"))

        "error" :: errorLines ->
            Problem (CompilationError (String.join "\n" errorLines))

        [ "frame_count", n ] ->
            case String.toInt n of
                Just frameCount ->
                    Animating (initAnimation frameCount)

                Nothing ->
                    Problem (UnexpectedMessage ("frame_count wasn't number, but '" ++ n ++ "'"))

        [ "frame", n, svgUrl ] ->
            case String.toInt n of
                Just frameIndex ->
                    case model of
                        Animating animation ->
                            Animating { animation | frames = Dict.insert frameIndex svgUrl animation.frames }

                        _ ->
                            Problem (UnexpectedMessage "Got 'frame' message while not Animating")

                Nothing ->
                    Problem (UnexpectedMessage ("Frame index wasn't number, but '" ++ n ++ "'"))

        _ ->
            Problem (UnexpectedMessage data)


view : Model -> Html Msg
view model =
    Html.div [ class "app" ]
        [ Html.div [ Attr.id "view" ]
            [ case model of
                Disconnected ->
                    Html.text "Disconnected"

                Connected ->
                    Html.text "Connected"

                Compiling ->
                    -- it would be nice to have some progress indication
                    -- (at least animated spinner or something)
                    Html.text "Compiling ..."

                Problem problem ->
                    problemView problem

                Animating { bestFrame } ->
                    frameView bestFrame
            ]
        ]


frameIndexAt : Float -> Int -> Int
frameIndexAt time frameCount =
    floor (time * framesPerMillisecond) |> modBy frameCount


{-| At 30 FPS there is 30 / 1000 = 0.03 frames per millisecond
-}
framesPerMillisecond : Float
framesPerMillisecond =
    0.03



frameView : Maybe String -> Html Msg
frameView bestFrame =
    let
        image =
            case bestFrame of
                Just svgUrl ->
                    Html.img [ src (webBackend ++ svgUrl) ] []

                Nothing ->
                    Html.text ""
    in
    Html.div [ class "viewer" ]
        [ image
        ]


problemView : Problem -> Html msg
problemView problem =
    case problem of
        CompilationError error ->
            Html.div []
                [ Html.h1 [] [ Html.text "Compilation failed" ]
                , Html.pre [] [ Html.text error ]
                ]

        ConnectionFailed ->
            Html.div []
                [ Html.text "Failed to establish connection to server. Sorry. :-/"
                ]

        PortMessageDecodeFailure decodeError ->
            Html.text ("Failed to decode Port message. The error was: " ++ Json.Decode.errorToString decodeError)

        UnexpectedMessage problemDescription ->
            Html.text ("Unexpected message: " ++ problemDescription)
