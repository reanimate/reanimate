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
import Platform.Sub
import Ports
import Task
import Time
import WebSocket


backend : String
backend =
    "149.56.132.163"



-- backend = "localhost"


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

        -- , Keyboard.downs KeyPressed
        , case model of
            Animating { player } ->
                case player of
                    Playing _ ->
                        Browser.Events.onAnimationFrameDelta TimeDeltaReceived

                    Paused ->
                        Sub.none

            -- Problem ConnectionFailed ->
            --     Time.every 2000 (always AttemptReconnect)
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
    | KeyPressed Keyboard.RawKey
    | ToggleHelp
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
    , showingHelp : Bool
    , frameDeltas : List Float
    }


initAnimation : Int -> Animation
initAnimation frameCount =
    { frameCount = frameCount
    , frames = Dict.empty
    , frameIndex = 0
    , player = Playing 0
    , bestFrame = Nothing
    , showingHelp = False
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
            , address = "ws://localhost:10161"
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

        KeyPressed rawKey ->
            ( model, processKeyPress rawKey model )

        Pause ->
            ( updateAnimation (\animation -> { animation | player = Paused }) model
            , blurPlayOrPause
            )

        ToggleHelp ->
            ( updateAnimation (\animation -> { animation | showingHelp = not animation.showingHelp }) model, Cmd.none )

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
                    -- TODO it would be nice to have some progress indication (at least animated spinner or something)
                    Html.text "Compiling ..."

                Problem problem ->
                    problemView problem

                Animating { frameCount, frames, frameIndex, player, bestFrame, showingHelp, frameDeltas } ->
                    Html.div []
                        [ linkPrefetches frames
                        , case player of
                            Paused ->
                                frameView bestFrame frameIndex frameCount frames showingHelp frameDeltas True

                            Playing _ ->
                                frameView bestFrame frameIndex frameCount frames showingHelp frameDeltas False
                        ]
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


playControls : Bool -> Html Msg
playControls paused =
    Html.div [ class "media-controls" ]
        [ Html.button [ class "button", onClick (Seek -10), disabled (not paused), title "10 frames back" ] [ Html.text "<<" ]
        , Html.button [ class "button", onClick (Seek -1), disabled (not paused), title "1 frame back" ] [ Html.text "<" ]
        , if paused then
            Html.button [ class "button", onClick Play, id playOrPauseId ] [ Html.text "Play" ]

          else
            Html.button [ class "button", onClick Pause, id playOrPauseId ] [ Html.text "Pause" ]
        , Html.button [ class "button", onClick (Seek 1), disabled (not paused), title "1 frame forward" ] [ Html.text ">" ]
        , Html.button [ class "button", onClick (Seek 10), disabled (not paused), title "10 frames forward" ] [ Html.text ">>" ]
        ]


linkPrefetches : Frames -> Html Msg
linkPrefetches frames =
    Html.div []
        (List.map mkLink (Dict.values frames))


mkLink : String -> Html Msg
mkLink svgUrl =
    Html.node "link"
        [ Attr.rel "prefetch"
        , Attr.href ("http://" ++ backend ++ ":10162/" ++ svgUrl)
        ]
        []


frameView : Maybe String -> Int -> Int -> Frames -> Bool -> List Float -> Bool -> Html Msg
frameView bestFrame frameIndex frameCount frames showingHelp frameDeltas isPaused =
    let
        image =
            case bestFrame of
                Just svgUrl ->
                    Html.img [ src ("http://localhost:10162/" ++ svgUrl) ] []

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
                Html.button [ class "help-button button", onClick ToggleHelp ] [ Html.text "?" ]

        bar =
            Html.div [ class "bar" ]
                [ playControls isPaused
                , Html.span []
                    [ Html.text <|
                        " Frame: "
                            ++ String.padLeft digitCount '0' (String.fromInt (frameIndex + 1))
                            ++ " / "
                            ++ frameCountStr

                    -- ++ (if isPaused then
                    --         " "
                    --     else
                    --         Fps.showAverage frameDeltas
                    --    )
                    ]
                , progressView
                , helpView
                ]
    in
    Html.div [ class "viewer" ]
        [ bar
        , image
        ]


progressBar : Int -> Int -> Html msg
progressBar receivedFrames frameCount =
    Html.label []
        [ Html.span [] [ Html.text " | Loading frames " ]
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
                        case model of
                            Animating { player } ->
                                case player of
                                    Playing _ ->
                                        Just Pause

                                    Paused ->
                                        Just Play

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
        , Html.button [ class "help-button button", onClick ToggleHelp ] [ Html.text "X" ]
        , Html.table []
            [ explainKey "SPACEBAR" "Pause / Play animation"
            , explainKey "ARROW LEFT" " Move back 1 frame"
            , explainKey "ARROW RIGHT" " Move forward 1 frame"
            , explainKey "ARROW UP" " Move forward 10 frames"
            , explainKey "ARROW DOWN" " Move back 10 frames"
            ]
        ]
