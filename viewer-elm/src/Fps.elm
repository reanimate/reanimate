module Fps exposing (showAverage, update)

-- FPS logic copied from https://github.com/w0rm/elm-physics/blob/master/examples/Common/Fps.elm


update : Float -> List Float -> List Float
update dt fps =
    List.take 50 (dt :: fps)


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
