port module Ports exposing (receiveSocketMsg, sendSocketCommand)

import Json.Decode exposing (Value)


port receiveSocketMsg : (Value -> msg) -> Sub msg


port sendSocketCommand : Value -> Cmd msg
