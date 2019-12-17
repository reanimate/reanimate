module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Fps
import Html exposing (Html)
import Html.Attributes as Attr exposing (class, disabled, id, src, style, title, value)
import Html.Events exposing (onClick)
import Json.Decode
import Keyboard exposing (RawKey)
import Platform.Sub
import Ports
import Task
import Time
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
        , Keyboard.downs KeyPressed
        , case model.status of
            AnimationRunning _ _ ->
                Browser.Events.onAnimationFrameDelta TimeDeltaReceived

            SomethingWentWrong ConnectionFailed ->
                Time.every 1000 (always AttemptReconnect)

            _ ->
                Sub.none
        ]


type Msg
    = MessageReceived (Result Json.Decode.Error WebSocket.WebSocketMsg)
    | TimeDeltaReceived Float
    | AttemptReconnect
    | PauseAtFrame Int
    | Play
    | Seek Int
    | KeyPressed Keyboard.RawKey
    | ToggleHelp
    | NoOp


type Status
    = Disconnected
    | Connected
    | Compiling
    | AnimationRunning Int Frames
    | AnimationPaused Int Frames Int
    | SomethingWentWrong Problem


type Problem
    = CompilationError String
    | ConnectionFailed
    | PortMessageDecodeFailure Json.Decode.Error
    | UnexpectedMessage String


type alias Model =
    { status : Status

    -- Milliseconds from the beginning of animation
    , time : Float
    , showingHelp : Bool
    , frameDeltas : List Float
    }


type alias Frames =
    Dict Int String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Disconnected
      , time = 0
      , showingHelp = False
      , frameDeltas = []
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

        TimeDeltaReceived delta ->
            ( { model
                | time = model.time + delta
                , frameDeltas = Fps.update delta model.frameDeltas
              }
            , Cmd.none
            )

        MessageReceived result ->
            ( processResult result model, Cmd.none )

        KeyPressed rawKey ->
            ( model, processKeyPress rawKey model )

        PauseAtFrame pauseIndex ->
            ( case model.status of
                AnimationRunning frameCount frames ->
                    { model | status = AnimationPaused frameCount frames pauseIndex }

                _ ->
                    model
            , blurPlayOrPause
            )

        Play ->
            ( case model.status of
                AnimationPaused frameCount frames pauseIndex ->
                    { model
                        | status = AnimationRunning frameCount frames

                        -- To resume animation from currently displayed frame
                        , time = toFloat pauseIndex / framesPerMillisecond
                    }

                _ ->
                    model
            , blurPlayOrPause
            )

        Seek delta ->
            ( case model.status of
                AnimationPaused frameCount frames pauseIndex ->
                    { model | status = AnimationPaused frameCount frames (modBy frameCount (pauseIndex + delta)) }

                _ ->
                    model
            , Cmd.none
            )

        ToggleHelp ->
            ( { model | showingHelp = not model.showingHelp }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
                    -- TODO there's probably no need for Done message, as frontend doesn't need to do anything special
                    model

                _ ->
                    somethingWentWrong (UnexpectedMessage ("Unknown status: '" ++ status ++ "'")) model

        "error" :: errorLines ->
            somethingWentWrong (CompilationError (String.join "\n" errorLines)) model

        [ "frame_count", n ] ->
            case String.toInt n of
                Just frameCount ->
                    { model | status = AnimationRunning frameCount Dict.empty }

                Nothing ->
                    somethingWentWrong (UnexpectedMessage ("frame_count wasn't number, but '" ++ n ++ "'")) model

        [ "frame", n, svgUrl ] ->
            case String.toInt n of
                Just frameIndex ->
                    case model.status of
                        AnimationRunning frameCount frames ->
                            { model | status = AnimationRunning frameCount (Dict.insert frameIndex svgUrl frames) }

                        AnimationPaused frameCount frames pausedIndex ->
                            { model | status = AnimationPaused frameCount (Dict.insert frameIndex svgUrl frames) pausedIndex }

                        _ ->
                            somethingWentWrong (UnexpectedMessage "Got 'frame' message while not in AnimationRunning or AnimationPaused state") model

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
                -- TODO it would be nice to have some progress indication (at least animated spinner or something)
                Html.text "Compiling ..."

            SomethingWentWrong problem ->
                problemView problem

            AnimationRunning frameCount frames ->
                let
                    frameIndex =
                        frameIndexAt model.time frameCount
                in
                frameView frameIndex frameCount False frames model.showingHelp model.frameDeltas

            AnimationPaused frameCount frames frameIndex ->
                frameView frameIndex frameCount True frames model.showingHelp model.frameDeltas
        ]


frameIndexAt : Float -> Int -> Int
frameIndexAt time frameCount =
    floor (time * framesPerMillisecond) |> modBy frameCount


{-| At 60 FPS there is 60 / 1000 = 0.06 frames per millisecond
-}
framesPerMillisecond : Float
framesPerMillisecond =
    0.06


playControls : Bool -> Int -> Html Msg
playControls paused pauseIndex =
    Html.div [ class "media-controls" ]
        [ Html.button [ onClick (Seek -10), disabled (not paused), title "10 frames back" ] [ Html.text "<<" ]
        , Html.button [ onClick (Seek -1), disabled (not paused), title "1 frame back" ] [ Html.text "<" ]
        , if paused then
            Html.button [ onClick Play, id playOrPauseId ] [ Html.text "Play" ]

          else
            Html.button [ onClick (PauseAtFrame pauseIndex), id playOrPauseId ] [ Html.text "Pause" ]
        , Html.button [ onClick (Seek 1), disabled (not paused), title "1 frame forward" ] [ Html.text ">" ]
        , Html.button [ onClick (Seek 10), disabled (not paused), title "10 frames forward" ] [ Html.text ">>" ]
        ]


frameView : Int -> Int -> Bool -> Frames -> Bool -> List Float -> Html Msg
frameView frameIndex frameCount isPaused frames showingHelp frameDeltas =
    let
        bestFrame =
            List.head (List.reverse (Dict.values (Dict.filter (\x _ -> x <= frameIndex) frames)))

        image =
            case bestFrame of
                Just svgUrl ->
                    Html.img [ src svgUrl ] []

                Nothing ->
                    Html.text ""

        frameCountStr =
            String.fromInt frameCount

        digitCount =
            String.length frameCountStr

        progressView =
            let
                receivedFrames =
                    Dict.size frames
            in
            if receivedFrames /= frameCount then
                progressBar receivedFrames frameCount

            else
                Html.text ""

        helpView =
            if showingHelp then
                helpModal

            else
                Html.button [ class "help-button", onClick ToggleHelp ] [ Html.text "?" ]

        bar =
            Html.pre [ class "bar" ]
                [ playControls isPaused frameIndex
                , Html.span [ class "text" ] [Html.text <|
                    " Frame: "
                        ++ String.padLeft digitCount '0' (String.fromInt (frameIndex + 1))
                        ++ " / "
                        ++ frameCountStr
                        ++ (if isPaused then
                                " "

                            else
                                Fps.showAverage frameDeltas
                           )]
                , progressView
                , helpView
                ]
    in
    Html.div [ class "viewer" ]
        [ image
        , bar
        ]


progressBar : Int -> Int -> Html msg
progressBar receivedFrames frameCount =
    Html.label [ ]
        [ Html.span [ class "text" ] [ Html.text " | Loading frames " ]
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

        PortMessageDecodeFailure decodeError ->
            Html.text ("Failed to decode Port message. The error was: " ++ Json.Decode.errorToString decodeError)

        UnexpectedMessage problemDescription ->
            Html.text ("Unexpected message: " ++ problemDescription)


processKeyPress : RawKey -> Model -> Cmd Msg
processKeyPress rawKey model =
    Keyboard.oneOf [ Keyboard.navigationKey, Keyboard.whitespaceKey ] rawKey
        |> Maybe.andThen
            (\key ->
                case key of
                    Keyboard.ArrowDown ->
                        Just (Seek -10)

                    Keyboard.ArrowUp ->
                        Just (Seek 10)

                    Keyboard.ArrowRight ->
                        Just (Seek 1)

                    Keyboard.ArrowLeft ->
                        Just (Seek -1)

                    Keyboard.Spacebar ->
                        case model.status of
                            AnimationPaused _ _ _ ->
                                Just Play

                            AnimationRunning frameCount _ ->
                                Just (PauseAtFrame (frameIndexAt model.time frameCount))

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )
        |> Maybe.map (Task.succeed >> Task.perform identity)
        |> Maybe.withDefault Cmd.none


helpModal : Html Msg
helpModal =
    let
        explainKey key legend =
            Html.tr []
                [ Html.td [] [ Html.b [] [ Html.text key ] ]
                , Html.td [] [ Html.text legend ]
                ]
    in
    Html.div [ class "help-dialog" ]
        [ Html.h2 [ style "margin-top" "0px" ] [ Html.text "Keyboard shortcuts" ]
        , Html.button [ class "help-button", onClick ToggleHelp ] [ Html.text "X" ]
        , Html.table []
            [ explainKey "SPACEBAR" "Pause / Play animation"
            , explainKey "ARROW LEFT" " Move back 1 frame"
            , explainKey "ARROW RIGHT" " Move forward 1 frame"
            , explainKey "ARROW UP" " Move forward 10 frames"
            , explainKey "ARROW DOWN" " Move back 10 frames"
            ]
        ]
