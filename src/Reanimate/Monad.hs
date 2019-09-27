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

data Frame a = Frame {unFrame :: Duration -> Time -> State ([Tree] -> [Tree]) a}

instance Functor Frame where
  fmap fn f = Frame $ \d t -> fmap fn (unFrame f d t)

instance Applicative Frame where
  pure a = Frame $ \_ _ -> pure a
  fn <*> fa = Frame $ \d t -> do
    f <- unFrame fn d t
    a <- unFrame fa d t
    pure (f a)

instance Monad Frame where
  return a = Frame $ \_ _ -> pure a
  f >>= g = Frame $ \d t -> do
    a <- unFrame f d t
    unFrame (g a) d t

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
emit svg = Frame $ \_ _ -> modify (.(svg:))

-- | Play animations in sequence. The @lhs@ animation is removed after it has
--   completed. New animation duration is @duration lhs + duration rhs@.
before :: Animation -> Animation -> Animation
before (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation (d1+d2) (Frame $ \_ t -> if t < d1 then f1 d1 t else f2 d2 (t-d1))

-- | Play two animation concurrently. Shortest animation freezes on last frame.
--   New animation duration is @max (duration lhs) (duration rhs)@.
sim :: Animation -> Animation -> Animation
sim (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation (max d1 d2) $ Frame $ \_ t -> do
    f1 d1 (min d1 t)
    f2 d2 (min d2 t)

-- | Play two animation concurrently. Shortest animation loops.
--   New animation duration is @max (duration lhs) (duration rhs)@.
simLoop :: Animation -> Animation -> Animation
simLoop (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation (max d1 d2) $ Frame $ \_ t -> do
    f1 d1 (t `mod'` d1)
    f2 d2 (t `mod'` d2)

-- | Play two animation concurrently. Animations disappear after playing once.
--   New animation duration is @max (duration lhs) (duration rhs)@.
simDrop :: Animation -> Animation -> Animation
simDrop (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation (max d1 d2) $ Frame $ \_ t -> do
    when (t < d1) (f1 d1 t)
    when (t < d2) (f2 d2 t)

-- | Empty animation (no SVG output) with a fixed duration.
pause :: Double -> Animation
pause d = Animation d (pure ())

-- | Play left animation and freeze on the last frame, then play the right
--   animation. New duration is @duration lhs + duration rhs@.
andThen :: Animation -> Animation -> Animation
andThen a b = a `sim` (pause (duration a) `before` b)

getSignal :: Signal -> Frame Double
getSignal s = Frame $ \d t -> pure $ s (t/d)

frameAt :: Double -> Animation -> Tree
frameAt t (Animation d (Frame f)) = mkGroup $ execState (f d (min d t)) id []

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
mapF fn frame = Frame $ \d t -> do
  case runState (unFrame frame d t) id of
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

freezeFrame :: Double -> Animation -> Frame ()
freezeFrame t (Animation d f) = Frame $ \_ _ -> unFrame f d t

adjustSpeed :: Double -> Animation -> Animation
adjustSpeed factor (Animation d fn) =
  Animation (d/factor) $ Frame $ \_dur t -> unFrame fn d (t*factor)

-- | Set the duration of an animation by adjusting its playback rate. The
--   animation is still played from start to finish without being cropped.
setDuration :: Double -> Animation -> Animation
setDuration newD (Animation _ fn) =
  Animation newD $ Frame $ \dur t -> unFrame fn dur t

-- | Play an animation in reverse. Duration remains unchanged.
reverseAnimation :: Animation -> Animation
reverseAnimation (Animation d fn) = Animation d $ Frame $ \_dur t ->
  unFrame fn d (d-t)

-- | Play animation before playing it again in reverse. Duration is twice
--   the duration of the input.
autoReverse :: Animation -> Animation
autoReverse a = a `before` reverseAnimation a

oscillate :: Frame a -> Frame a
oscillate f = Frame $ \d t -> do
  if t < d/2
    then unFrame f d (t*2)
    else unFrame f d (d*2-t*2)

-- | Loop animation @n@ number of times. This number may be fractional and it
--   may be less than 1. It must be greater than or equal to 0, though.
--   New duration is @n*duration input@.
repeatAnimation :: Double -> Animation -> Animation
repeatAnimation n (Animation d f) = Animation (d*n) $ Frame $ \_ t ->
  unFrame f d (t `mod'` d)

freezeAtPercentage :: Double -> Animation -> Animation
freezeAtPercentage frac (Animation d genFrame) =
  Animation d $ Frame $ \_ _ -> unFrame genFrame d (d*frac)
