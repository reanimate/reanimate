{-|
  Easing functions modify the rate of change in animations.
  More examples can be seen here: <https://easings.net/>.
-}
module Reanimate.Ease
  ( Signal
  , constantS
  , fromToS
  , reverseS
  , curveS
  , powerS
  , bellS
  , oscillateS
  , cubicBezierS
  ) where

-- | Signals are time-varying variables. Signals can be composed using function
--   composition.
type Signal = Double -> Double

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

-- | Power curve signal. Takes a steepness parameter. 2 is a good default.
--
--   Example:
--
--   > signalA (powerS 2) drawProgress
--
--   <<docs/gifs/doc_powerS.gif>>
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
--   signal will curve.
--
--   Example:
--
--   > signalA (cubicBezierS (0.0, 0.8, 0.9, 1.0)) drawProgress
--   
--   <<docs/gifs/doc_cubicBezierS.gif>>
cubicBezierS :: (Double, Double, Double, Double) -> Signal
cubicBezierS (x1, x2, x3, x4) s = 
  let ms = 1-s
  in x1*ms^(3::Int) + 3*x2*ms^(2::Int)*s + 3*x3*ms*s^(2::Int) + x4*s^(3::Int)
