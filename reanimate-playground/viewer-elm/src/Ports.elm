port module Ports exposing (prefetchImageCommand, receiveControlMsg, receiveEditorMsg, receiveSocketMsg, sendSocketCommand)

import Json.Decode exposing (Value)


port receiveEditorMsg : (String -> msg) -> Sub msg


port receiveControlMsg : (String -> msg) -> Sub msg


port receiveSocketMsg : (Value -> msg) -> Sub msg


port sendSocketCommand : Value -> Cmd msg


port prefetchImageCommand : String -> Cmd msg
