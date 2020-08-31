{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
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
import Reanimate.Ease
import Reanimate.Effect

-- | A transition transforms one animation into another.
type Transition = Animation -> Animation -> Animation

-- | Apply a signal to the timing of a transition.
signalT :: Signal -> Transition -> Transition
signalT = mapT . signalA

-- | Map the result of a transition.
mapT :: (Animation -> Animation) -> Transition -> Transition
mapT fn t a b = fn (t a b)

-- | Apply transition only to @N@ seconds of the first
--   animation and to the last @N@ seconds of the second animation.
--
--   Example:
--
-- @
-- 'overlapT' 0.5 'fadeT' 'Reanimate.Builtin.Documentation.drawBox' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_overlapT.gif>>
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
--
--   Example:
--
-- @
-- 'chainT' ('overlapT' 0.5 'fadeT') ['Reanimate.Builtin.Documentation.drawBox', 'Reanimate.Builtin.Documentation.drawCircle', 'Reanimate.Builtin.Documentation.drawProgress']
-- @
--
--   <<docs/gifs/doc_chainT.gif>>
chainT :: Transition -> [Animation] -> Animation
chainT _ [] = pause 0
chainT t (x:xs) = foldl t x xs

-- | Fade out left-hand-side animation while fading in right-hand-side animation.
--
--   Example:
--
-- @
-- 'Reanimate.Builtin.Documentation.drawBox' `'fadeT'` 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_fadeT.gif>>
fadeT :: Transition
fadeT = effectT fadeOutE fadeInE
