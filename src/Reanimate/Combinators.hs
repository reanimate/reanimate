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
    Animation d $ \t ->
      let s = (t/window) in
      if s < 1
        then g_ [opacity_ (pack $ show s)] (fn t)
        else fn t

fadeOut :: Double -> Ani a -> Ani a
fadeOut window (Animation d fn) =
    Animation d $ \t ->
      let s = (d-t)/window in
      if s < 1
        then g_ [opacity_ (pack $ show s)] (fn t)
        else fn t

fade :: Double -> Ani a -> Ani a
fade window = fadeIn window . fadeOut window

annotate :: (Svg a -> Svg a) -> Ani a -> Ani a
annotate fn1 (Animation d fn2) =
  Animation d (fn1 . fn2)

progress :: Ani () -> Ani ()
progress ani = proc t -> do
  ani -< t
  let txt = show (round ((t / getDuration ani) * 100)) ++ "%"
  emit -< text_   [x_ "10", y_ "20", font_size_ "20"
                  , text_anchor_ "bottom"
                  , fill_ "white"] (toHtml txt)


before :: Ani () -> Ani () -> Ani ()
before (Animation d1 fn1) (Animation d2 fn2) =
  Animation (d1+d2) (\t -> if t < d1 then fn1 t else fn2 (t-d1))

follow :: [Ani ()] -> Ani ()
follow = foldr before (arr $ pure ())

sim :: [Ani ()] -> Ani ()
sim = foldr par (arr $ pure ())

par :: Ani () -> Ani () -> Ani ()
par a b = proc t -> do
  a -< t
  b -< t
  returnA -< ()

type Path = [(Double, Double)]

approxFnData :: Int -> (Double -> (Double, Double)) -> Path
approxFnData steps fn =
  fn 0 : [ fn (fromIntegral n/fromIntegral steps) | n <- [0..steps] ]

renderPath :: Path -> Svg ()
renderPath ((startX, startY):rest) =
    path_ [stroke_ "white", fill_ "translucent", d_ path]
  where
    path = T.unlines $
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
