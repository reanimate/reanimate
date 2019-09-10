module Reanimate.Monad where

import           Control.Arrow         ()
import qualified Control.Category      as C
import           Control.Monad.State
import           Data.Fixed
import           Data.Fixed            (mod')
import Text.Printf
import qualified Data.Map              as M
import           Data.Monoid           ((<>))
import           Data.Text             (Text, pack)
import           Graphics.SvgTree      (Document (..), Number (..), Text (..),
                                        TextSpan (..), TextSpanContent (..),
                                        Tree, Tree (..), xmlOfDocument, xmlOfTree)
import           Graphics.SvgTree.Printer
import           Reanimate.Svg
import           Reanimate.Signal
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
    width = 320
    height = width / (16/9)
    doc = Document
      { _viewBox = Just (-width/2, -height/2, width, height)
      , _width = w
      , _height = h
      , _elements = [t]
      , _definitions = M.empty
      , _description = ""
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

pauseAtBeginning :: Double -> Animation -> Animation
pauseAtBeginning d1 a =
    Animation d1 (freezeFrame 0 a) `before` a

pauseAround :: Double -> Double -> Animation -> Animation
pauseAround start end = pauseAtEnd end . pauseAtBeginning start

freezeFrame :: Double -> Animation -> Frame ()
freezeFrame t (Animation d f) = Frame $ \_ _ -> unFrame f d t

adjustSpeed :: Double -> Animation -> Animation
adjustSpeed factor (Animation d fn) =
  Animation (d/factor) $ Frame $ \_dur t -> unFrame fn d (t*factor)

setDuration :: Double -> Animation -> Animation
setDuration newD (Animation d fn) =
  Animation newD $ Frame $ \dur t -> unFrame fn dur t

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
