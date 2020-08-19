{-| Effects represent modifications applied to frames of the 'Animation'.
Effects can (and usually do) depend on time.
One or more effects can be applied over the entire duration of animation, or modified to affect
only a specific portion at the beginning \/ middle \/ end of the animation.
-}
module Reanimate.Effect
  ( -- * Primitive Effects
  Effect
  , fadeInE
  , fadeOutE
  , fadeLineInE
  , fadeLineOutE
  , fillInE
  , drawInE
  , drawOutE
  , translateE
  , scaleE
  , constE
  -- * Modifying Effects
  , overBeginning
  , overEnding
  , overInterval
  , reverseE
  , delayE
  , aroundCenterE
  -- * Applying Effects to Animations
  , applyE
  ) where

import           Graphics.SvgTree    (Tree)
import           Reanimate.Animation
import           Reanimate.Svg

-- | An Effect represents a modification of a SVG 'Tree' that can vary with time.
type Effect = Duration -- ^ Duration of the effect (in seconds)
           -> Time -- ^ Time elapsed from when the effect started (in seconds)
           -> Tree -- ^ Image to be modified
           -> Tree -- ^ Image after modification

-- | Modify the effect so that it only applies to the initial part of the animation.
overBeginning :: Duration -- ^ Duration of the initial segment of the animation over which the Effect should be applied
              -> Effect -- ^ The Effect to modify
              -> Effect -- ^ Effect which will only affect the initial segment of the animation
overBeginning maxT effect _d t =
  if t < maxT
    then effect maxT t
    else id

-- | Modify the effect so that it only applies to the ending part of the animation.
overEnding :: Duration -- ^ Duration of the ending segment of the animation over which the Effect should be applied
           -> Effect  -- ^ The Effect to modify
           -> Effect -- ^ Effect which will only affect the ending segment of the animation
overEnding minT effect d t =
  if t >= blankDur
    then effect minT (t-blankDur)
    else id
  where
    blankDur = d-minT

-- | Modify the effect so that it only applies within given interval of animation's running time.
overInterval :: Time -- ^ time after start of animation when the effect should start
             -> Time -- ^ time after start of the animation when the effect should finish
             -> Effect  -- ^ The Effect to modify
             -> Effect -- ^ Effect which will only affect the specified interval within the animation
overInterval start end effect _d t =
  if start <= t && t <= end
    then effect dur ((t - start) / dur)
    else id
  where
    dur = end - start

-- | @reverseE effect@ starts where the @effect@ ends and vice versa.
reverseE :: Effect -> Effect
reverseE fn d t = fn d (d-t)

-- | Delay the effect so that it only starts after specified duration and then runs till the end of animation.
delayE :: Duration -> Effect -> Effect
delayE delayT fn d = overEnding (d-delayT) fn d

-- | Modify the animation by applying the effect. If desired, you can apply multiple effects to single animation by calling this function multiple times.
applyE :: Effect -> Animation -> Animation
applyE fn ani = let d = duration ani
                in mkAnimation d $ \t -> fn d (d*t) $ frameAt (d*t) ani

-- | Build an effect from an image-modifying function. This effect does not change as time passes.
constE :: (Tree -> Tree) -> Effect
constE fn _d _t = fn

-- | Change image opacity from 0 to 1.
fadeInE :: Effect
fadeInE d t = withGroupOpacity (t/d)

-- | Change image opacity from 1 to 0. Reverse of 'fadeInE'.
fadeOutE :: Effect
fadeOutE = reverseE fadeInE

-- | Change stroke width from 0 to given value.
fadeLineInE :: Double -> Effect
fadeLineInE w d t = withStrokeWidth (w*(t/d))

-- | Change stroke width from given value to 0. Reverse of 'fadeLineInE'.
fadeLineOutE :: Double -> Effect
fadeLineOutE = reverseE . fadeLineInE

-- | Effect of progressively drawing the image. Note that this will only affect primitive shapes (see 'pathify').
drawInE :: Effect
drawInE d t = withFillOpacity 0 . partialSvg (t/d) . pathify

-- | Reverse of 'drawInE'.
drawOutE :: Effect
drawOutE = reverseE drawInE

-- | Change fill opacity from 0 to 1.
fillInE :: Effect
fillInE d t = withFillOpacity f
  where
    f = t/d

-- | Change scale from 1 to given value.
scaleE :: Double -> Effect
scaleE target d t = scale (1 + (target-1) * t/d)

-- | Move the image from its current position to the target x y coordinates.
translateE :: Double -> Double -> Effect
translateE x y d t = translate (x * t/d) (y * t/d)

-- | Transform the effect so that the image passed to the effect's image-modifying
-- function has coordinates (0, 0) shifted to the center of its bounding box.
-- Also see 'aroundCenter'.
aroundCenterE :: Effect -> Effect
aroundCenterE e d t = aroundCenter (e d t)
