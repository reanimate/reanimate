module Reanimate.Signal
  ( Signal
  , signalFlat
  , signalLinear
  , signalFromTo
  , signalReverse
  , signalCurve
  , signalBell
  , signalOscillate
  , signalFromList
  ) where

type Signal = Double -> Double

signalFromList :: [(Double, Signal)] -> Signal
signalFromList fns t = worker 0 fns
  where
    worker _ [] = 0
    worker now [(len, fn)] = fn (min 1 ((t-now) / min (1-now) len))
    worker now ((len, fn):rest)
      | now+len < t = worker (now+len) rest
      | otherwise = fn ((t-now) / len)

signalFlat :: Double -> Signal
signalFlat x = const x

signalLinear :: Signal
signalLinear = id

signalFromTo :: Double -> Double -> Signal -> Signal
signalFromTo from to c t = from + (to-from)*(c t)

signalReverse :: Signal -> Signal
signalReverse fn t = fn (1-t)

signalCurve :: Double -> Signal
signalCurve steepness s =
  if s < 0.5
    then 0.5 * (2*s)**steepness
    else 1-0.5 * (2 - 2*s)**steepness

signalOscillate :: Signal -> Signal
signalOscillate fn t =
  if t < 1/2
    then fn (t*2)
    else fn (2-t*2)

signalBell :: Double -> Signal
signalBell = signalOscillate . signalCurve
