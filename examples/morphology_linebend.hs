#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad
import qualified Data.Vector              as V
import           Linear.V2
import           Reanimate
import           Reanimate.ColorMap
import           Reanimate.Math.Common
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear
import           Reanimate.Morph.LineBend

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

main :: IO ()
main = reanimate $
  setDuration 10 $
  addStatic (mkBackgroundPixel bgColor) $
  mapA (withStrokeWidth defaultStrokeWidth) $
  mapA (withStrokeColor "black") $
  mapA (withFillOpacity 1) $
    sceneAnimation $ do
      _ <- newSpriteSVG $
        withStrokeWidth 0 $ translate (-3) 4 $
        center $ latex "linear"
      _ <- newSpriteSVG $
        withStrokeWidth 0 $ translate (3) 4 $
        center $ latex "line bend"
      forM_ pairs $ uncurry showPair
  where
    showPair from to =
      waitOn $ do
        fork $ play $ animate (morph linear from to)
          # mapA (translate (-3) (-0.5))
          # signalA (curveS 4)
        fork $ play $ animate (morph myMorph from to)
          # mapA (translate (3) (-0.5))
          # signalA (curveS 4)
    myMorph = linear{morphTrajectory = lineBend }
    pairs = zip stages (tail stages ++ [head stages])
    stages = map (scale 6 . center) $ colorize
      [ latex "X"
      , latex "$\\aleph$"
      , latex "Y"
      , latex "$\\infty$"
      , latex "I"
      , latex "$\\pi$"
      , latex "1"
      , latex "S"
      ]

colorize :: [SVG] -> [SVG]
colorize lst =
  [ withFillColorPixel (promotePixel $ parula (n/fromIntegral (length lst-1))) elt
  | elt <- lst
  | n <- [0..]
  ]
