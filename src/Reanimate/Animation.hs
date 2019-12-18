module Reanimate.Animation
  ( Duration
  , Time
  , SVG
  , Animation(..) -- TODO should this be exposed? The constructor is used directly in Effect.hs
  -- * Creating animations
  , mkAnimation
  , animate
  , staticFrame
  , pause
  -- * Querying animations
  , duration
  , frameAt
  -- * Composing animations
  , seqA
  , andThen
  , parA
  , parLoopA
  , parDropA
  -- * Modifying animations
  , setDuration
  , adjustDuration
  , mapA
  , takeA
  , dropA
  , pauseAtEnd
  , pauseAtBeginning
  , pauseAround
  , pauseUntil
  , repeatA
  , reverseA
  , playThenReverseA
  , signalA
  , freezeAtPercentage
  -- * Rendering
  , renderTree
  , renderSvg
  ) where

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

-- | Construct an animation with a duration of @1@.
animate :: (Time -> SVG) -> Animation
animate = Animation 1

-- | Create an animation with provided @duration@, which consists of stationary frame displayed for its entire duration.
staticFrame :: Duration -> SVG -> Animation
staticFrame d svg = Animation d (const svg)

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

-- | Calculate the frame that would be displayed at given point in @time@ of running @animation@.
--
-- The provided time parameter is clamped between 0 and animation duration.
frameAt :: Time -> Animation -> SVG
frameAt t (Animation d f) = f t'
  where
    t' = clamp 0 1 (t/d)

renderTree :: SVG -> String
renderTree t = maybe "" ppElement $ xmlOfTree t

renderSvg :: Maybe Number -- ^ The number to use as value of the @width@ attribute of the resulting top-level svg element. If @Nothing@, the width attribute won't be rendered.
          -> Maybe Number -- ^ Similar to previous argument, but for @height@ attribute.
          -> SVG          -- ^ SVG to render
          -> String       -- ^ String representation of SVG XML markup
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

mapA :: (SVG -> SVG) -> Animation -> Animation
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
freezeFrame :: Time -> Animation -> (Time -> SVG)
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


-- | @freezeAtPercentage time animation@ creates an animation consisting of stationary frame,
-- that would be displayed in the provided @animation@ at given @time@.
-- The duration of the new animation is the same as the duration of provided @animation@.
freezeAtPercentage :: Time  -- ^ value between 0 and 1. The frame displayed at this point in the original animation will be displayed for the duration of the new animation
                   -> Animation -- ^ original animation, from which the frame will be taken
                   -> Animation -- ^ new animation consisting of static frame displayed for the duration of the original animation
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

-- | @takeA duration animation@ creates a new animation consisting of initial segment of 
--   @animation@ of given @duration@, played at the same rate as the original animation.
--
--  The @duration@ parameter is clamped to be between 0 and @animation@'s duration.
--  New animation duration is equal to (eventually clamped) @duration@.
takeA :: Duration -> Animation -> Animation
takeA len (Animation d gen) = Animation len' $ \t ->
    gen (t * len'/d)
  where
    len' = clamp 0 d len

-- | @dropA duration animation@ creates a new animation by dropping initial segment
--   of length @duration@ from the provided @animation@, played at the same rate as the original animation.
--
--  The @duration@ parameter is clamped to be between 0 and @animation@'s duration.
--  The duration of the resulting animation is duration of provided @animation@ minus (eventually clamped) @duration@.
dropA :: Duration -> Animation -> Animation
dropA len (Animation d gen) = Animation len' $ \t ->
    gen (t * len'/d + len/d)
  where
    len' = d - clamp 0 d len

clamp :: Double -> Double -> Double -> Double
clamp a b number
  | a < b     = max a (min b number)
  | otherwise = max b (min a number)
