module Reanimate.Monad where

import           Control.Arrow            ()
import           Control.Monad.State
import           Data.Fixed               (mod')
import qualified Data.Map                 as M
import           Graphics.SvgTree         (Document (..), Number (..),
                                           Tree (..),
                                           xmlOfTree)
import           Graphics.SvgTree.Printer
import           Reanimate.Signal
import           Reanimate.Svg
import           Text.XML.Light.Output

type Duration = Double
type Time = Double

data Frame a = Frame {unFrame :: Time -> State ([Tree] -> [Tree]) a}

instance Functor Frame where
  fmap fn f = Frame $ \t -> fmap fn (unFrame f t)

instance Applicative Frame where
  pure a = Frame $ \_ -> pure a
  fn <*> fa = Frame $ \t -> do
    f <- unFrame fn t
    a <- unFrame fa t
    pure (f a)

instance Monad Frame where
  return a = Frame $ \_ -> pure a
  f >>= g = Frame $ \t -> do
    a <- unFrame f t
    unFrame (g a) t

-- End behavior:
--   Freeze at last frame
--   Loop
--   Disappear
-- | Animations are SVGs over a finite time. Isomorphic to
--   (Duration, Double -> Tree).
data Animation = Animation Duration (Frame ())

mkAnimation :: Duration -> Frame () -> Animation
mkAnimation = Animation

duration :: Animation -> Duration
duration (Animation d _) = d

emit :: Tree -> Frame ()
emit svg = Frame $ \_ -> modify (.(svg:))

-- | Play animations in sequence. The @lhs@ animation is removed after it has
--   completed. New animation duration is @duration lhs + duration rhs@.
before :: Animation -> Animation -> Animation
before (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation totalD $ Frame $ \t ->
    if t < d1/totalD
      then f1 (t * totalD/d1)
      else f2 ((t-d1/totalD) * totalD/d2)
  where
    totalD = d1+d2

-- | Play two animation concurrently. Shortest animation freezes on last frame.
--   New animation duration is @max (duration lhs) (duration rhs)@.
sim :: Animation -> Animation -> Animation
sim (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation (max d1 d2) $ Frame $ \t -> do
    let t1 = t * totalD/d1
        t2 = t * totalD/d2
    f1 (min 1 t1)
    f2 (min 1 t2)
  where
    totalD = max d1 d2
-- | Play two animation concurrently. Shortest animation loops.
--   New animation duration is @max (duration lhs) (duration rhs)@.
simLoop :: Animation -> Animation -> Animation
simLoop (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation totalD $ Frame $ \t -> do
    let t1 = t * totalD/d1
        t2 = t * totalD/d2
    f1 (t1 `mod'` 1)
    f2 (t2 `mod'` 1)
  where
    totalD = max d1 d2

-- | Play two animation concurrently. Animations disappear after playing once.
--   New animation duration is @max (duration lhs) (duration rhs)@.
simDrop :: Animation -> Animation -> Animation
simDrop (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation totalD $ Frame $ \t -> do
    let t1 = t * totalD/d1
        t2 = t * totalD/d2
    unless (t1>1) (f1 t1)
    unless (t2>1) (f2 t2)
  where
    totalD = max d1 d2

-- | Empty animation (no SVG output) with a fixed duration.
pause :: Double -> Animation
pause d = Animation d (pure ())

-- | Play left animation and freeze on the last frame, then play the right
--   animation. New duration is @duration lhs + duration rhs@.
andThen :: Animation -> Animation -> Animation
andThen a b = a `sim` (pause (duration a) `before` b)

getSignal :: Signal -> Frame Double
getSignal s = Frame $ \t -> pure $ s t

frameAt :: Double -> Animation -> Tree
frameAt t (Animation d (Frame f)) = mkGroup $ execState (f t') id []
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
mapA fn (Animation d f) = Animation d (mapF fn f)

mapF :: (Tree -> Tree) -> Frame a -> Frame a
mapF fn frame = Frame $ \t -> do
  case runState (unFrame frame t) id of
    (a, children) -> modify (. (fn (mkGroup (children [])):)) >> pure a

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
freezeFrame :: Double -> Animation -> Frame ()
freezeFrame t (Animation d f) = Frame $ \_ -> unFrame f (t/d)

adjustSpeed :: Double -> Animation -> Animation
adjustSpeed factor (Animation d fn) =
  Animation (d/factor) fn

-- | Set the duration of an animation by adjusting its playback rate. The
--   animation is still played from start to finish without being cropped.
setDuration :: Double -> Animation -> Animation
setDuration newD (Animation _ fn) = Animation newD fn

-- | Play an animation in reverse. Duration remains unchanged.
reverseAnimation :: Animation -> Animation
reverseAnimation (Animation d fn) = Animation d $ Frame $ \t ->
  unFrame fn (1-t)

-- | Play animation before playing it again in reverse. Duration is twice
--   the duration of the input.
autoReverse :: Animation -> Animation
autoReverse a = a `before` reverseAnimation a

oscillate :: Frame a -> Frame a
oscillate f = Frame $ \t -> do
  if t < 1/2
    then unFrame f (t*2)
    else unFrame f (2-t*2)

-- | Loop animation @n@ number of times. This number may be fractional and it
--   may be less than 1. It must be greater than or equal to 0, though.
--   New duration is @n*duration input@.
repeatAnimation :: Double -> Animation -> Animation
repeatAnimation n (Animation d f) = Animation (d*n) $ Frame $ \t ->
  unFrame f (t `mod'` recip n)

freezeAtPercentage :: Double -> Animation -> Animation
freezeAtPercentage frac (Animation d genFrame) =
  Animation d $ Frame $ \_ -> unFrame genFrame frac
