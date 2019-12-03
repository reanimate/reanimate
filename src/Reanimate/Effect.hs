module Reanimate.Effect where

import           Graphics.SvgTree    (Tree)
import           Reanimate.Animation
import           Reanimate.Svg

type Effect = Double -> Double -> Tree -> Tree

overBeginning :: Double -> Effect -> Effect
overBeginning maxT fn = \_d t ->
  if t < maxT
    then fn maxT t
    else id

overEnding :: Double -> Effect -> Effect
overEnding minT fn d t =
  if t >= blankDur
    then fn minT (t-blankDur)
    else id
  where
    blankDur = d-minT

reverseE :: Effect -> Effect
reverseE fn = \d t -> fn d (d-t)

delayE :: Double -> Effect -> Effect
delayE delayT fn = \d t -> overEnding (d-delayT) fn d t

applyE :: Effect -> Animation -> Animation
applyE fn (Animation d genFrame) = Animation d $ \t -> fn d (d*t) $ genFrame t

constE :: (Tree -> Tree) -> Effect
constE fn _d _t = fn

fadeInE :: Effect
fadeInE d t = withGroupOpacity (t/d)

fadeOutE :: Effect
fadeOutE = reverseE fadeInE

fadeLineInE :: Double -> Effect
fadeLineInE w d t = withStrokeWidth (w*(t/d))

fadeLineOutE :: Double -> Effect
fadeLineOutE = reverseE . fadeLineInE

drawInE :: Effect
drawInE d t = withFillOpacity 0 . partialSvg (t/d) . pathify

drawOutE :: Effect
drawOutE = reverseE drawInE

fillInE :: Effect
fillInE d t = withFillOpacity f
  where
    f = t/d

scaleE :: Double -> Effect
scaleE target d t = scale (1 + (target-1) * t/d)

translateE :: Double -> Double -> Effect
translateE x y d t = translate (x * t/d) (y * t/d)

aroundCenterE :: Effect -> Effect
aroundCenterE e d t = aroundCenter (e d t)
