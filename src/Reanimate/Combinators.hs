{-# LANGUAGE OverloadedStrings, Arrows #-}
module Reanimate.Combinators where

import Control.Arrow
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Fixed
import Lucid.Svg

import Reanimate.Arrow

fadeIn :: Double -> Ani a -> Ani a
fadeIn window (Animation d fn) =
    Animation d $ \dur t () ->
      let s = (t/window) in
      if s < 1
        then g_ [opacity_ (pack $ show s)] (fn dur t ())
        else fn dur t ()

fadeOut :: Double -> Ani a -> Ani a
fadeOut window (Animation d fn) =
    Animation d $ \dur t () ->
      let s = (d-t)/window in
      if s < 1
        then g_ [opacity_ (pack $ show s)] (fn dur t ())
        else fn dur t ()

fade :: Double -> Ani a -> Ani a
fade window = fadeIn window . fadeOut window

annotate :: (Svg a -> Svg a) -> Ani a -> Ani a
annotate fn1 (Animation d fn2) =
  Animation d (\dur t -> fn1 . fn2 dur t)

-- data Animation a b = Animation Double (Double -> a -> Svg b)
annotate' :: Ani a -> Animation (Svg a -> Svg a) a
annotate' (Animation d f) =
  Animation d (\dur t g -> g (f dur t ()))

progress :: Ani () -> Ani ()
progress ani = proc () -> do
  ani -< ()
  t <- getTime -< ()
  let txt = show (round ((t / animationDuration ani) * 100)) ++ "%"
  emit -< text_   [x_ "10", y_ "20", font_size_ "20"
                  , text_anchor_ "bottom"
                  , fill_ "white"] (toHtml txt)


before :: Animation a () -> Animation a () -> Animation a ()
before (Animation d1 fn1) (Animation d2 fn2) =
  Animation (d1+d2) (\d t -> if t < d1 then fn1 d t else fn2 d (t-d1))

follow :: [Animation a ()] -> Animation a ()
follow [] = arr $ pure ()
follow (x:xs) = foldl before x xs

sim :: [Ani ()] -> Ani ()
sim = foldr par (arr $ pure ())

par :: Ani () -> Ani () -> Ani ()
par a b = proc t -> do
  a -< t
  b -< t

type Path = [(Double, Double)]

approxFnData :: Int -> (Double -> (Double, Double)) -> Path
approxFnData steps fn =
  fn 0 : [ fn (fromIntegral n/fromIntegral steps) | n <- [0..steps] ]

renderPath :: Path -> Svg ()
renderPath [] = return ()
renderPath dat =
    path_ [stroke_ "white", fill_ "translucent", d_ path]
  where
    path = renderPathText dat

renderPathText :: Path -> Text
renderPathText ((startX,startY):rest) = T.unlines $
    ("M " <> pack (show startX) <> " " <> pack (show startY)):
    [ "L " <> pack (show x) <> " " <> pack (show y) | (x, y) <- rest]

morphPath :: Path -> Path -> Double -> Path
morphPath src dst idx = zipWith worker src dst
  where
    worker (x1, y1) (x2, y2) =
      (x1 + (x2-x1)*idx
      ,y1 + (y2-y1)*idx)

approxFn :: Int -> (Double -> (Double, Double)) -> Ani ()
approxFn steps fn = proc t -> do
    emit -< renderPath $ approxFnData steps fn

constantSvg :: Svg a -> Ani a
constantSvg svg = Animation 0 (\dur t () -> svg)

signal :: Double -> Double -> Ani Double
signal from to =
  Animation 0 (\dur t () -> pure (from + (to-from)*(t/dur)))

signalSigmoid :: Double -> Double -> Double -> Ani Double
signalSigmoid steepness from to = proc () -> do
  s <- signal 0 1 -< ()
  let s' = (s-0.5)*steepness
  let sigmoid = exp s' / (exp s'+1)
      ret = (from + (to-from)*sigmoid)
  returnA -< ret

signalSCurve :: Double -> Double -> Double -> Ani Double
signalSCurve steepness from to = proc () -> do
  s <- signal 0 1 -< ()
  let s' = if s < 0.5
              then 0.5 * (2*s)**steepness
              else 1-0.5 * (2 - 2*s)**steepness
  returnA -< from + (to-from)*s'

signalOscillate :: Double -> Double -> Ani Double
signalOscillate from to = proc () -> do
  s <- signal from (to+diff) -< ()
  returnA -< if s < to then s
            else (to*2 - s)
  where
    diff = abs (to-from)

signalOscillateSCurve :: Double -> Double -> Double -> Ani Double
signalOscillateSCurve steepness from to = proc () -> do
  s <- signalSCurve steepness from (to+diff) -< ()
  returnA -< if s < to then s
            else (to*2 - s)
  where
    diff = abs (to-from)

adjustSpeed :: Double -> Ani a -> Ani a
adjustSpeed factor (Animation d fn) =
  Animation (d/factor) (\dur t -> fn dur (t*factor))

freeze :: Double -> Ani a -> Ani a
freeze d (Animation d' fn) =
  Animation (d+d') (\dur t -> if t > d' then fn d' d' else fn d' t)

loop :: Ani a -> Ani a
loop (Animation d fn) =
  Animation d (\dur t -> fn dur (mod' t d))

repeatAni :: Double -> Ani a -> Ani a
repeatAni times (Animation d fn) =
  Animation (d*times) $ \dur t -> fn dur (mod' t d)

sync :: Ani () -> Ani () -> Ani ()
sync (Animation d1 fn1) (Animation d2 fn2) =
  Animation (max d1 d2) $ \dur t () -> do
    fn1 dur (t*(d1/maxDuration)) ()
    fn2 dur (t*(d2/maxDuration)) ()
  where
    maxDuration = max d1 d2

syncAll :: [Ani ()] -> Ani ()
syncAll = foldl sync (arr (pure ()))

delay :: Double -> Ani () -> Ani ()
delay pause (Animation d fn) =
  Animation (d+pause) $ \dur t a ->
    if t < pause
      then return ()
      else fn dur (t-pause) a

num_ :: (Show a, Num a) => (Text -> Attribute) -> (a -> Attribute)
num_ attr n = attr (pack (show n))
