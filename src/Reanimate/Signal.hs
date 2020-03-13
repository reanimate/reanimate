module Reanimate.Signal
  ( Signal
  , constantS
  , fromToS
  , reverseS
  , curveS
  , powerS
  , bellS
  , oscillateS
  , fromListS
  , cubicBezierS
  ) where

-- | Signals are time-varying variables. Signals can be composed using function
--   composition.
type Signal = Double -> Double

fromListS :: [(Double, Signal)] -> Signal
fromListS fns t = worker 0 fns
  where
    worker _ [] = 0
    worker now [(len, fn)] = fn (min 1 ((t-now) / min (1-now) len))
    worker now ((len, fn):rest)
      | now+len < t = worker (now+len) rest
      | otherwise = fn ((t-now) / len)

-- | Constant signal.
--
--   Example:
--
--   > signalA (constantS 0.5) drawProgress
--
--   <<docs/gifs/doc_constantS.gif>>
constantS :: Double -> Signal
constantS = const

-- | Signal with new starting and end values.
--
--   Example:
--
--   > signalA (fromToS 0.8 0.2) drawProgress
--
--   <<docs/gifs/doc_fromToS.gif>>
fromToS :: Double -> Double -> Signal
fromToS from to t = from + (to-from)*t

-- | Reverse signal order.
--
--   Example:
--
--   > signalA reverseS drawProgress
--
--   <<docs/gifs/doc_reverseS.gif>>
reverseS :: Signal
reverseS t = 1-t

-- | S-curve signal. Takes a steepness parameter. 2 is a good default.
--
--   Example:
--
--   > signalA (curveS 2) drawProgress
--
--   <<docs/gifs/doc_curveS.gif>>
curveS :: Double -> Signal
curveS steepness s =
  if s < 0.5
    then 0.5 * (2*s)**steepness
    else 1-0.5 * (2 - 2*s)**steepness

powerS :: Double -> Signal
powerS steepness s = s**steepness

-- | Oscillate signal.
--
--   Example:
--
--   > signalA oscillateS drawProgress
--
--   <<docs/gifs/doc_oscillateS.gif>>
oscillateS :: Signal
oscillateS t =
  if t < 1/2
    then t*2
    else 2-t*2

-- | Bell-curve signal. Takes a steepness parameter. 2 is a good default.
--
--   Example:
--
--   > signalA (bellS 2) drawProgress
--
--   <<docs/gifs/doc_bellS.gif>>
bellS :: Double -> Signal
bellS steepness = curveS steepness . oscillateS

-- | Cubic Bezier signal. Gives you a fair amount of control over how the
--   signal will 'curve'.
--
--   Example:
--
--   > signalA (cubicBezierS (0.0, 0.8, 0.9, 1.0)) drawProgress
--   
--   <<docs/gifs/doc_cubicBezierS.gif>>
cubicBezierS :: (Double, Double, Double, Double) -> Signal
cubicBezierS (x1, x2, x3, x4) s = 
  let ms = 1-s
  in x1*ms^3 + 3*x2*ms^2*s + 3*x3*ms*s^2 + x4*s^3
