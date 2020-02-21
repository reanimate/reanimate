module Reanimate.Transition
  ( Transition
  , signalT
  , mapT
  , overlapT
  , chainT
  , effectT
  , fadeT
  ) where

import Reanimate.Animation
import Reanimate.Signal
import Reanimate.Effect

-- | A transition transforms one animation into another.
type Transition = Animation -> Animation -> Animation

-- | Apply a signal to the timing of a transition.
signalT :: Signal -> Transition -> Transition
signalT = mapT . signalA

-- | Map the result of a transition.
mapT :: (Animation -> Animation) -> Transition -> Transition
mapT fn t = \a b -> fn (t a b)

-- | Apply transition only to @overlap@ seconds of the first
--   animation and to the last @overlap@ seconds of the second animation.
overlapT :: Double -> Transition -> Transition
overlapT overlap t a b =
    aBefore `seqA` t aOverlap bOverlap `seqA` bAfter
  where
    aBefore  = takeA (duration a - overlap) a
    aOverlap = lastA overlap a
    bOverlap = takeA overlap b
    bAfter   = dropA overlap b


-- | Create a transition between two animations by applying an effect to each respective animation.
effectT :: Effect -- ^ Effect to be applied to the first animation.
        -> Effect -- ^ Effect to be applied to the second animation.
        -> Transition
effectT eA eB a b = applyE eA a `parA` applyE eB b

-- | Combine a list of animations using a given transition.
chainT :: Transition -> [Animation] -> Animation
chainT _ [] = pause 0
chainT t (x:xs) = foldl t x xs

-- | Fade out left-hand-side animation while fading in right-hand-side animation.
fadeT :: Transition
fadeT = effectT fadeOutE fadeInE
