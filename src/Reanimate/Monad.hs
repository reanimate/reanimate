module Reanimate.Monad where

import           Control.Arrow ()
import qualified Control.Category      as C
import           Control.Monad.State
import           Data.Fixed
import           Data.Fixed            (mod')
import qualified Data.Map              as M
import           Data.Monoid           ((<>))
import           Data.Text             (Text, pack)
import           Graphics.Svg          (Document (..), Number (..), Tree,
                                        xmlOfDocument, Tree(..), Text(..), TextSpan(..), TextSpanContent(..))
import           Reanimate.LaTeX
import           Reanimate.Svg
import           Text.XML.Light        (elContent)
import           Text.XML.Light.Output

import           Reanimate.Combinators (approxFnData, morphPath)

type Duration = Double
type Time = Double

data Frame a = Frame {unFrame :: Duration -> Time -> State ([Tree] -> [Tree]) a}

instance Functor Frame where
  fmap fn f = Frame $ \d t -> fmap fn (unFrame f d t)

instance Applicative Frame where
  pure a = Frame $ \_ _ -> pure a
  fn <*> fa = Frame $ \d t -> do
    fn <- unFrame fn d t
    a <- unFrame fa d t
    pure (fn a)

instance Monad Frame where
  return a = Frame $ \_ _ -> pure a
  f >>= g = Frame $ \d t -> do
    a <- unFrame f d t
    unFrame (g a) d t

-- End behavior:
--   Freeze at last frame
--   Loop
--   Disappear
data Animation = Animation Duration (Frame ())

mkAnimation :: Duration -> Frame () -> Animation
mkAnimation = Animation

duration :: Animation -> Duration
duration (Animation d _) = d

emit :: Tree -> Frame ()
emit svg = Frame $ \_ _ -> modify (.(svg:))

before :: Animation -> Animation -> Animation
before (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation (d1+d2) (Frame $ \_ t -> if t < d1 then f1 d1 t else f2 d2 (t-d1))

-- Play two animation concurrently. Shortest animation freezes on last frame.
sim :: Animation -> Animation -> Animation
sim (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation (max d1 d2) $ Frame $ \d t -> do
    f1 d1 (min d1 t)
    f2 d2 (min d2 t)

-- Play two animation concurrently. Shortest animation loops.
simLoop :: Animation -> Animation -> Animation
simLoop (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation (max d1 d2) $ Frame $ \d t -> do
    f1 d1 (t `mod'` d1)
    f2 d2 (t `mod'` d2)

-- Play two animation concurrently. Animations disappear after playing once.
simDrop :: Animation -> Animation -> Animation
simDrop (Animation d1 (Frame f1)) (Animation d2 (Frame f2)) =
  Animation (max d1 d2) $ Frame $ \d t -> do
    when (t < d1) (f1 d1 t)
    when (t < d2) (f2 d2 t)

pause :: Double -> Animation
pause d = Animation d (pure ())

andThen :: Animation -> Animation -> Animation
andThen a b = a `sim` (pause (duration a) `before` b)

signal :: Double -> Double -> Frame Double
signal from to = Frame $ \d t -> pure $
  from + (to-from)*(t/d)

signalSCurve :: Double -> Double -> Double -> Frame Double
signalSCurve steepness from to = do
  s <- signal 0 1
  let s' = if s < 0.5
              then 0.5 * (2*s)**steepness
              else 1-0.5 * (2 - 2*s)**steepness
  pure $ from + (to-from)*s'

frameAt :: Double -> Animation -> Tree
frameAt t (Animation d (Frame f)) = mkGroup $ execState (f d (min d t)) id []

renderTree :: Tree -> String
renderTree = renderSizedTree Nothing Nothing

renderSizedTree :: Maybe Number -> Maybe Number -> Tree -> String
renderSizedTree w h t = ppElement $ xmlOfDocument doc
  where
    width = 320
    height = width / (16/9)
    doc = Document
      { _viewBox = Just (-width/2, -height/2, width, height)
      , _width = w
      , _height = h
      , _elements = [t]
      , _definitions = M.empty
      , _description = ""
      , _styleRules = []
      , _documentLocation = ""
      }

mapA :: (Tree -> Tree) -> Animation -> Animation
mapA fn (Animation d f) = Animation d (mapF fn f)

mapF :: (Tree -> Tree) -> Frame a -> Frame a
mapF fn frame = Frame $ \d t -> do
  case runState (unFrame frame d t) id of
    (a, children) -> modify (. (fn (mkGroup (children [])):)) >> pure a

pauseAtEnd :: Double -> Animation -> Animation
pauseAtEnd p a = a `andThen` pause p

adjustSpeed :: Double -> Animation -> Animation
adjustSpeed factor (Animation d fn) =
  Animation (d/factor) $ Frame $ \_dur t -> unFrame fn d (t*factor)

reverseAnimation :: Animation -> Animation
reverseAnimation (Animation d fn) = Animation d $ Frame $ \_dur t ->
  unFrame fn d (d-t)

autoReverse :: Animation -> Animation
autoReverse a = a `before` reverseAnimation a

oscillate :: Frame a -> Frame a
oscillate f = Frame $ \d t -> do
  if t < d/2
    then unFrame f d (t*2)
    else unFrame f d (d*2-t*2)

repeatAnimation :: Double -> Animation -> Animation
repeatAnimation n (Animation d f) = Animation (d*n) $ Frame $ \_ t ->
  unFrame f d (t `mod'` d)
