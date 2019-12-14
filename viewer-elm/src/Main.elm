module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr exposing (class, disabled, src, style, title, value)
import Html.Events exposing (onClick)
import Json.Decode
import Platform.Sub
import Ports
import Time exposing (Posix, millisToPosix, posixToMillis)
import WebSocket


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
        , case model.status of
            AnimationRunning _ _ ->
                Browser.Events.onAnimationFrame TimestampReceived

            ReceivingFrames _ _ ->
                Browser.Events.onAnimationFrame TimestampReceived

            SomethingWentWrong ConnectionFailed ->
                Time.every 1000 (always AttemptReconnect)

            _ ->
                Sub.none
        ]


type Msg
    = MessageReceived (Result Json.Decode.Error WebSocket.WebSocketMsg)
    | TimestampReceived Posix
    | AttemptReconnect
    | PauseClicked Int
    | PlayClicked
    | SeekClicked Int


type Status
    = Disconnected
    | Connected
    | Compiling
    | ReceivingFrames Int Frames
    | AnimationRunning Int Frames
    | AnimationPaused Int Frames Int
    | SomethingWentWrong Problem


type Problem
    = CompilationError String
    | ConnectionFailed
    | DoneWithoutFrames
    | FramesMissing Int
    | PortMessageDecodeFailure Json.Decode.Error
    | UnexpectedMessage String


type alias Model =
    { status : Status
    , clock : Posix
    }


type alias Frames =
    Dict Int String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Disconnected
      , clock = millisToPosix 0
      }
    , connectCommand
    )


connectCommand : Cmd msg
connectCommand =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Connect
            { name = "TheSocket"
            , address = "ws://localhost:9161"
            , protocol = ""
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttemptReconnect ->
            ( model, connectCommand )

        TimestampReceived clock ->
            ( { model | clock = clock }, Cmd.none )

        MessageReceived result ->
            ( processResult result model, Cmd.none )

        PauseClicked frameIndex ->
            ( case model.status of
                AnimationRunning frameCount frames ->
                    { model | status = AnimationPaused frameCount frames frameIndex }

                _ ->
                    model
            , Cmd.none
            )

        PlayClicked ->
            ( case model.status of
                AnimationPaused frameCount frames _ ->
                    { model | status = AnimationRunning frameCount frames }

                _ ->
                    model
            , Cmd.none
            )

        SeekClicked delta ->
            ( case model.status of
                AnimationPaused frameCount frames frameIndex ->
                    { model | status = AnimationPaused frameCount frames (modBy frameCount (frameIndex + delta)) }

                _ ->
                    model
            , Cmd.none
            )


processResult : Result Json.Decode.Error WebSocket.WebSocketMsg -> Model -> Model
processResult result model =
    case result of
        Err decodeError ->
            { model | status = SomethingWentWrong (PortMessageDecodeFailure decodeError) }

        Ok wsMsg ->
            case wsMsg of
                WebSocket.Error { error } ->
                    { model | status = SomethingWentWrong (UnexpectedMessage error) }

                WebSocket.Data { data } ->
                    processMessage data model


processMessage : String -> Model -> Model
processMessage data model =
    case String.lines data of
        [ "connection established" ] ->
            { model | status = Connected }

        [ "connection failed" ] ->
            somethingWentWrong ConnectionFailed model

        [ "status", status ] ->
            case status of
                "Compiling" ->
                    { model | status = Compiling }

                "Done" ->
                    case model.status of
                        ReceivingFrames frameCount frames ->
                            if Dict.keys frames == List.range 0 (frameCount - 1) then
                                { model | status = AnimationRunning frameCount frames }

                            else
                                somethingWentWrong (FramesMissing frameCount) model

                        _ ->
                            somethingWentWrong DoneWithoutFrames model

                _ ->
                    somethingWentWrong (UnexpectedMessage ("Unknown status: '" ++ status ++ "'")) model

        "error" :: errorLines ->
            somethingWentWrong (CompilationError (String.join "\n" errorLines)) model

        [ "frame_count", n ] ->
            case String.toInt n of
                Just frameCount ->
                    { model | status = ReceivingFrames frameCount Dict.empty }

                Nothing ->
                    somethingWentWrong (UnexpectedMessage ("frame_count wasn't number, but '" ++ n ++ "'")) model

        [ "frame", n, svgUrl ] ->
            case String.toInt n of
                Just frameIndex ->
                    case model.status of
                        ReceivingFrames frameCount frames ->
                            { model | status = ReceivingFrames frameCount (Dict.insert frameIndex svgUrl frames) }

                        _ ->
                            somethingWentWrong (UnexpectedMessage "Got 'frame' message while not ReceivingFrames") model

                Nothing ->
                    somethingWentWrong (UnexpectedMessage ("Frame index wasn't number, but '" ++ n ++ "'")) model

        _ ->
            somethingWentWrong (UnexpectedMessage data) model


somethingWentWrong : Problem -> Model -> Model
somethingWentWrong what model =
    { model | status = SomethingWentWrong what }


view : Model -> Html Msg
view model =
    Html.div [ class "app" ]
        [ case model.status of
            Disconnected ->
                Html.text "Disconnected"

            Connected ->
                Html.text "Connected"

            Compiling ->
                Html.text "Compiling.."

            SomethingWentWrong problem ->
                problemView problem

            ReceivingFrames frameCount frames ->
                preliminaryAnimationView frameCount frames model.clock

            AnimationRunning frameCount frames ->
                animationView frameCount frames model.clock

            AnimationPaused frameCount frames frameIndex ->
                manualControlsView frameCount frames frameIndex
        ]


frameIndexAt : Posix -> Int -> Int
frameIndexAt now frameCount =
    (posixToMillis now * 60) // 1000 |> modBy frameCount


preliminaryAnimationView : Int -> Frames -> Posix -> Html Msg
preliminaryAnimationView frameCount frames clock =
    let
        frameIndex =
            frameIndexAt clock frameCount

        bestFrame =
            List.head (List.reverse (Dict.values (Dict.filter (\x _ -> x <= frameIndex) frames)))

        controls =
            progressView (Dict.size frames) frameCount
    in
    frameView frameIndex frameCount controls bestFrame


animationView : Int -> Frames -> Posix -> Html Msg
animationView frameCount frames clock =
    let
        frameIndex =
            frameIndexAt clock frameCount

        controls =
            playControls False frameIndex
    in
    Dict.get frameIndex frames
        |> frameView frameIndex frameCount controls


manualControlsView : Int -> Frames -> Int -> Html Msg
manualControlsView frameCount frames frameIndex =
    let
        controls =
            playControls True frameIndex
    in
    Dict.get frameIndex frames
        |> frameView frameIndex frameCount controls


playControls : Bool -> Int -> Html Msg
playControls paused frameIndex =
    Html.div [ class "media-controls" ]
        [ Html.button [ onClick (SeekClicked -10), disabled (not paused), title "10 frames back" ] [ Html.text "<<" ]
        , Html.button [ onClick (SeekClicked -1), disabled (not paused), title "1 frame back" ] [ Html.text "<" ]
        , if paused then
            Html.button [ onClick PlayClicked, disabled (not paused) ] [ Html.text "Play" ]

          else
            Html.button [ onClick (PauseClicked frameIndex), disabled paused ] [ Html.text "Pause" ]
        , Html.button [ onClick (SeekClicked 1), disabled (not paused), title "1 frame forward" ] [ Html.text ">" ]
        , Html.button [ onClick (SeekClicked 10), disabled (not paused), title "10 frames forward" ] [ Html.text ">>" ]
        ]


frameView : Int -> Int -> Html Msg -> Maybe String -> Html Msg
frameView frameIndex frameCount controls maybeSvgUrl =
    let
        image =
            case maybeSvgUrl of
                Just svgUrl ->
                    Html.img [ src svgUrl ] []

                Nothing ->
                    Html.text ""

        frameCountStr =
            String.fromInt frameCount

        digitCount =
            String.length frameCountStr

        bar =
            Html.pre [ class "bar" ]
                [ Html.text ("Frame: " ++ String.padLeft digitCount '0' (String.fromInt frameIndex) ++ " / " ++ frameCountStr ++ " ")
                , controls
                ]
    in
    Html.div [ class "viewer" ]
        [ image
        , bar
        ]


progressView : Int -> Int -> Html msg
progressView receivedFrames frameCount =
    Html.label []
        [ Html.text "Loading frames "
        , Html.progress
            [ value (String.fromInt receivedFrames)
            , Attr.max (String.fromInt frameCount)
            ]
            []
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
                [ Html.text "Failed to establish connection. Possible causes include: "
                , Html.ul []
                    [ Html.li [] [ Html.text "The reanimate script is not running" ]
                    , Html.li [] [ Html.text "At most one viewer window can connect at time. Maybe there's another browser window/tab already connected?" ]
                    ]
                ]

        DoneWithoutFrames ->
            Html.text "Received 'done' message, but I was not receiving frames!"

        PortMessageDecodeFailure decodeError ->
            Html.text ("Failed to decode Port message. The error was: " ++ Json.Decode.errorToString decodeError)

        UnexpectedMessage problemDescription ->
            Html.text ("Unexpected message: " ++ problemDescription)

        FramesMissing frameCount ->
            Html.text ("Frame indices were not continuous block of number from 0 to " ++ String.fromInt (frameCount - 1))
