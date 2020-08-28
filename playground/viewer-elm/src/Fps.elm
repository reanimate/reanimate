module Fps exposing (init, showAverage, update)

-- FPS logic inspired by https://github.com/w0rm/elm-physics/blob/master/examples/Common/Fps.elm


update : Bool -> Float -> List Float -> List Float
update hasNewFrame dt fps =
    List.take 50 <|
        if hasNewFrame then
            dt :: fps

        else
            addToHead dt fps


addToHead : Float -> List Float -> List Float
addToHead dt fps =
    case fps of
        [] ->
            []

        x :: xs ->
            (x + dt) :: xs


showAverage : List Float -> String
showAverage fps =
    " @ " ++ String.fromInt (round (1000 / averageHelp 1 0 0 fps)) ++ " FPS"


averageHelp : Float -> Float -> Float -> List Float -> Float
averageHelp currentWeight sumOfWeights weightedSum fps =
    case fps of
        [] ->
            weightedSum / sumOfWeights

        el :: rest ->
            averageHelp
                (currentWeight * 0.9)
                (currentWeight + sumOfWeights)
                (el * currentWeight + weightedSum)
                rest


{-| Corresponds to initial FPS of 1
-}
init : List Float
init =
    List.repeat 50 1000
