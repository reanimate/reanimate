module Reanimate.Monad where

import           Control.Arrow            ()
import           Data.Fixed               (mod')
import qualified Data.Map                 as M
import           Graphics.SvgTree         (Document (..), Number (..),
                                           Tree (..), xmlOfTree)
import           Graphics.SvgTree.Printer
import           Reanimate.Svg
import           Text.XML.Light.Output

-- | Duration of an animation or effect. Usually measured in seconds.
type Duration = Double
-- | Time signal. Goes from 0 to 1, inclusive.
type Time = Double

type SVG = Tree

-- | Animations are SVGs over a finite time.
data Animation = Animation Duration (Time -> SVG)

mkAnimation :: Duration -> (Time -> SVG) -> Animation
mkAnimation = Animation

animate :: (Time -> SVG) -> Animation
animate = Animation 1

duration :: Animation -> Duration
duration (Animation d _) = d

-- | Play animations in sequence. The @lhs@ animation is removed after it has
--   completed. New animation duration is @duration lhs + duration rhs@.
before :: Animation -> Animation -> Animation
before (Animation d1 f1) (Animation d2 f2) =
  Animation totalD $ \t ->
    if t < d1/totalD
      then f1 (t * totalD/d1)
      else f2 ((t-d1/totalD) * totalD/d2)
  where
    totalD = d1+d2

-- | Play two animation concurrently. Shortest animation freezes on last frame.
--   New animation duration is @max (duration lhs) (duration rhs)@.
sim :: Animation -> Animation -> Animation
sim (Animation d1 f1) (Animation d2 f2) =
  Animation (max d1 d2) $ \t ->
    let t1 = t * totalD/d1
        t2 = t * totalD/d2 in
    mkGroup
    [ f1 (min 1 t1)
    , f2 (min 1 t2) ]
  where
    totalD = max d1 d2
-- | Play two animation concurrently. Shortest animation loops.
--   New animation duration is @max (duration lhs) (duration rhs)@.
simLoop :: Animation -> Animation -> Animation
simLoop (Animation d1 f1) (Animation d2 f2) =
  Animation totalD $ \t ->
    let t1 = t * totalD/d1
        t2 = t * totalD/d2 in
    mkGroup
    [ f1 (t1 `mod'` 1)
    , f2 (t2 `mod'` 1) ]
  where
    totalD = max d1 d2

-- | Play two animation concurrently. Animations disappear after playing once.
--   New animation duration is @max (duration lhs) (duration rhs)@.
simDrop :: Animation -> Animation -> Animation
simDrop (Animation d1 f1) (Animation d2 f2) =
  Animation totalD $ \t ->
    let t1 = t * totalD/d1
        t2 = t * totalD/d2 in
    mkGroup
    [ if t1>1 then None else f1 t1
    , if t2>1 then None else f2 t2 ]
  where
    totalD = max d1 d2

-- | Empty animation (no SVG output) with a fixed duration.
pause :: Double -> Animation
pause d = Animation d (const None)

-- | Play left animation and freeze on the last frame, then play the right
--   animation. New duration is @duration lhs + duration rhs@.
andThen :: Animation -> Animation -> Animation
andThen a b = a `sim` (pause (duration a) `before` b)

frameAt :: Double -> Animation -> Tree
frameAt t (Animation d f) = f t'
  where
    t' = min 1 (max 0 (t/d))

renderTree :: Tree -> String
renderTree t = maybe "" ppElement $ xmlOfTree t

renderSvg :: Maybe Number -> Maybe Number -> Tree -> String
renderSvg w h t = ppDocument doc
-- renderSvg w h t = ppFastElement (xmlOfDocument doc)
  where
    width = 16
    height = 9
    doc = Document
      { _viewBox = Just (-width/2, -height/2, width, height)
      , _width = w
      , _height = h
      , _elements = [withStrokeWidth (Num 0.05) $ scaleXY 1 (-1) t]
      , _definitions = M.empty
      , _description = ""
      , _documentLocation = ""
      }

-- | Map over the SVG produced by an animation at every frame.
mapA :: (Tree -> Tree) -> Animation -> Animation
mapA fn (Animation d f) = Animation d (fn . f)

-- | Freeze the last frame for @t@ seconds at the end of the animation.
pauseAtEnd :: Double -> Animation -> Animation
pauseAtEnd t a = a `andThen` pause t

-- | Freeze the first frame for @t@ seconds at the beginning of the animation.
pauseAtBeginning :: Double -> Animation -> Animation
pauseAtBeginning t a =
    Animation t (freezeFrame 0 a) `before` a

-- | Freeze the first and the last frame of the animation for a specified duration.
pauseAround :: Double -> Double -> Animation -> Animation
pauseAround start end = pauseAtEnd end . pauseAtBeginning start

-- Freeze frame at time @t@.
freezeFrame :: Double -> Animation -> (Time -> SVG)
freezeFrame t (Animation d f) = const $ f (t/d)

adjustSpeed :: Double -> Animation -> Animation
adjustSpeed factor (Animation d fn) =
  Animation (d/factor) fn

-- | Set the duration of an animation by adjusting its playback rate. The
--   animation is still played from start to finish without being cropped.
setDuration :: Double -> Animation -> Animation
setDuration newD (Animation _ fn) = Animation newD fn

-- | Play an animation in reverse. Duration remains unchanged.
reverseAnimation :: Animation -> Animation
reverseAnimation (Animation d fn) = Animation d $ \t ->
  fn (1-t)

-- | Play animation before playing it again in reverse. Duration is twice
--   the duration of the input.
autoReverse :: Animation -> Animation
autoReverse a = a `before` reverseAnimation a

-- | Loop animation @n@ number of times. This number may be fractional and it
--   may be less than 1. It must be greater than or equal to 0, though.
--   New duration is @n*duration input@.
repeatAnimation :: Double -> Animation -> Animation
repeatAnimation n (Animation d f) = Animation (d*n) $ \t ->
  f (t `mod'` recip n)

freezeAtPercentage :: Double -> Animation -> Animation
freezeAtPercentage frac (Animation d genFrame) =
  Animation d $ const $ genFrame frac
