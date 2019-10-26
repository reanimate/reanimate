module Reanimate.Animation where

import           Control.Arrow              ()
import           Data.Fixed                 (mod')
import           Graphics.SvgTree           (Alignment (..), Document (..),
                                             Number (..),
                                             PreserveAspectRatio (..),
                                             Tree (..), xmlOfTree)
import           Graphics.SvgTree.Printer
import           Reanimate.Constants
import           Reanimate.Signal
import           Reanimate.Svg.Constructors
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

-- | Construct animation with a duration of @1@.
animate :: (Time -> SVG) -> Animation
animate = Animation 1

-- | Query the duration of an animation.
duration :: Animation -> Duration
duration (Animation d _) = d

-- | Play animations in sequence. The @lhs@ animation is removed after it has
--   completed. New animation duration is '@duration lhs + duration rhs@'.
--
--   Example:
--
--   > drawBox `seqA` drawCircle
--
--   <<docs/gifs/doc_seqA.gif>>
seqA :: Animation -> Animation -> Animation
seqA (Animation d1 f1) (Animation d2 f2) =
  Animation totalD $ \t ->
    if t < d1/totalD
      then f1 (t * totalD/d1)
      else f2 ((t-d1/totalD) * totalD/d2)
  where
    totalD = d1+d2

-- | Play two animation concurrently. Shortest animation freezes on last frame.
--   New animation duration is '@max (duration lhs) (duration rhs)@'.
--
--   Example:
--
--   > drawBox `parA` adjustDuration (*2) drawCircle
--
--   <<docs/gifs/doc_parA.gif>>
parA :: Animation -> Animation -> Animation
parA (Animation d1 f1) (Animation d2 f2) =
  Animation (max d1 d2) $ \t ->
    let t1 = t * totalD/d1
        t2 = t * totalD/d2 in
    mkGroup
    [ f1 (min 1 t1)
    , f2 (min 1 t2) ]
  where
    totalD = max d1 d2

-- | Play two animation concurrently. Shortest animation loops.
--   New animation duration is '@max (duration lhs) (duration rhs)@'.
--
--   Example:
--
--   > drawBox `parLoopA` adjustDuration (*2) drawCircle
--
--   <<docs/gifs/doc_parLoopA.gif>>
parLoopA :: Animation -> Animation -> Animation
parLoopA (Animation d1 f1) (Animation d2 f2) =
  Animation totalD $ \t ->
    let t1 = t * totalD/d1
        t2 = t * totalD/d2 in
    mkGroup
    [ f1 (t1 `mod'` 1)
    , f2 (t2 `mod'` 1) ]
  where
    totalD = max d1 d2

-- | Play two animation concurrently. Animations disappear after playing once.
--   New animation duration is '@max (duration lhs) (duration rhs)@'.
--
--   Example:
--
--   > drawBox `parLoopA` adjustDuration (*2) drawCircle
--
--   <<docs/gifs/doc_parDropA.gif>>
parDropA :: Animation -> Animation -> Animation
parDropA (Animation d1 f1) (Animation d2 f2) =
  Animation totalD $ \t ->
    let t1 = t * totalD/d1
        t2 = t * totalD/d2 in
    mkGroup
    [ if t1>1 then None else f1 t1
    , if t2>1 then None else f2 t2 ]
  where
    totalD = max d1 d2

-- | Empty animation (no SVG output) with a fixed duration.
--
--   Example:
--
--   > pause 1 `seqA` drawProgress
--
--   <<docs/gifs/doc_pause.gif>>
pause :: Duration -> Animation
pause d = Animation d (const None)

-- | Play left animation and freeze on the last frame, then play the right
--   animation. New duration is '@duration lhs + duration rhs@'.
--
--   Example:
--
--   > drawBox `andThen` drawCircle
--
--   <<docs/gifs/doc_andThen.gif>>
andThen :: Animation -> Animation -> Animation
andThen a b = a `parA` (pause (duration a) `seqA` b)

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
      , _elements = [withStrokeWidth defaultStrokeWidth $ scaleXY 1 (-1) t]
      , _description = ""
      , _documentLocation = ""
      , _documentAspectRatio = PreserveAspectRatio False AlignNone Nothing
      }

-- | Map over the SVG produced by an animation at every frame.
--
--   Example:
--
--   > mapA (scale 0.5) drawCircle
--
--   <<docs/gifs/doc_mapA.gif>>

mapA :: (Tree -> Tree) -> Animation -> Animation
mapA fn (Animation d f) = Animation d (fn . f)

-- | Freeze the last frame for @t@ seconds at the end of the animation.
--
--   Example:
--
--   > pauseAtEnd 1 drawProgress
--
--   <<docs/gifs/doc_pauseAtEnd.gif>>
pauseAtEnd :: Duration -> Animation -> Animation
pauseAtEnd t a = a `andThen` pause t

-- | Freeze the first frame for @t@ seconds at the beginning of the animation.
--
--   Example:
--
--   > pauseAtBeginning 1 drawProgress
--
--   <<docs/gifs/doc_pauseAtBeginning.gif>>
pauseAtBeginning :: Duration -> Animation -> Animation
pauseAtBeginning t a =
    Animation t (freezeFrame 0 a) `seqA` a

-- | Freeze the first and the last frame of the animation for a specified duration.
--
--   Example:
--
--   > pauseAround 1 1 drawProgress
--
--   <<docs/gifs/doc_pauseAround.gif>>
pauseAround :: Duration -> Duration -> Animation -> Animation
pauseAround start end = pauseAtEnd end . pauseAtBeginning start

-- XXX: Rename to 'setDurationFreeze'. Add 'setDurationDrop' and
--      'setDurationLoop'.
pauseUntil :: Duration -> Animation -> Animation
pauseUntil d a = pauseAtEnd (d-duration a) a

-- Freeze frame at time @t@.
freezeFrame :: Double -> Animation -> (Time -> SVG)
freezeFrame t (Animation d f) = const $ f (t/d)

-- | Change the duration of an animation. Animates are stretched or squished
--   (rather than truncated) to fit the new duration.
adjustDuration :: (Duration -> Duration) -> Animation -> Animation
adjustDuration fn (Animation d gen) =
  Animation (fn d) gen

-- | Set the duration of an animation by adjusting its playback rate. The
--   animation is still played from start to finish without being cropped.
setDuration :: Duration -> Animation -> Animation
setDuration newD = adjustDuration (const newD)

-- | Play an animation in reverse. Duration remains unchanged. Shorthand for:
--   @'signalA' 'reverseS'@.
--
--   Example:
--
--   > reverseA drawCircle
--
--   <<docs/gifs/doc_reverseA.gif>>
reverseA :: Animation -> Animation
reverseA = signalA reverseS

-- | Play animation before playing it again in reverse. Duration is twice
--   the duration of the input.
--
--   Example:
--
--   > playThenReverseA drawCircle
--
--   <<docs/gifs/doc_playThenReverseA.gif>>
playThenReverseA :: Animation -> Animation
playThenReverseA a = a `seqA` reverseA a

-- | Loop animation @n@ number of times. This number may be fractional and it
--   may be less than 1. It must be greater than or equal to 0, though.
--   New duration is @n*duration input@.
--
--   Example:
--
--   > repeatA 1.5 drawCircle
--
--   <<docs/gifs/doc_repeatA.gif>>
repeatA :: Double -> Animation -> Animation
repeatA n (Animation d f) = Animation (d*n) $ \t ->
  f ((t*n) `mod'` 1)

freezeAtPercentage :: Time -> Animation -> Animation
freezeAtPercentage frac (Animation d genFrame) =
  Animation d $ const $ genFrame frac

-- | Modify the time component of an animation. Animation duration is unchanged.
--
--   Example:
--
--   > signalA (fromToS 0.25 0.75) drawCircle
--
--   <<docs/gifs/doc_signalA.gif>>
signalA :: Signal -> Animation -> Animation
signalA fn (Animation d gen) = Animation d $ gen . fn

takeA :: Double -> Animation -> Animation
takeA len (Animation d gen) = Animation len' $ \t ->
    gen (t * len'/d)
  where
    len' = min len d

dropA :: Double -> Animation -> Animation
dropA len (Animation d gen) = Animation len' $ \t ->
    gen (t * len'/d + len/d)
  where
    len' = max (d-len) 0
