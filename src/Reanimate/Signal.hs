module Reanimate.Signal where

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

signalBell :: Double -> Signal
signalBell steepness s
  | s < 0.5   = signalCurve steepness (s/0.5)
  | otherwise = signalCurve steepness (1-((s-0.5)/0.5))
