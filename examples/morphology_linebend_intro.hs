#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad
import           Data.List                (transpose)
import qualified Data.Vector              as V
import           Graphics.SvgTree         (LineJoin (..))
import           Linear.V2
import           Reanimate
import           Reanimate.Math.Common
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear
import           Reanimate.Morph.LineBend

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

spike1 :: Polygon
spike1 = scalePolygon 2.5 $ V.map (\x -> x - V2 1 1) $ V.fromList
  [ V2 0 0, V2 2 0
  , V2 2 1 ]

spike2 :: Polygon
spike2 = scalePolygon 2.5 $ V.map (\x -> x - V2 1 1) $ V.fromList
  [ V2 0 0, V2 2 0
  , V2 0 1 ]

spike3 :: Polygon
spike3 = scalePolygon 2.5 $ V.map (\x -> x - V2 1 1) $ V.fromList
  [ V2 0.5 0, V2 1.5 0
  , V2 1 2 ]

polygonShape :: Polygon -> SVG
polygonShape p = mkLinePathClosed
  [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList p  ++ [pAccess p 0] ]


main :: IO ()
main = reanimate $
  addStatic (mkBackgroundPixel bgColor) $
  mapA (withStrokeWidth defaultStrokeWidth) $
  mapA (withStrokeColor "black") $
  mapA (withStrokeLineJoin JoinRound) $
  mapA (withFillOpacity 1) $
    sceneAnimation $ do
      _ <- newSpriteSVG $
        withStrokeWidth 0 $ translate (-4) 4 $
        center $ latex "linear"
      _ <- newSpriteSVG $
        withStrokeWidth 0 $ translate (4) 4 $
        center $ latex "line bend"
      showTrails
      forM_ pairs $ uncurry showPair
  where
    showTrails = do
      _ <- newSpriteSVG $
            translate (-4) (-0.5) $
            genTrails (map linearTrajectory pairs)
      _ <- newSpriteSVG $
            translate (4) (-0.5) $
            genTrails (map lineBend pairs)
      return ()
    showPair from to =
      waitOn $ do
        fork $ play $ mkAnimation 4 (morph linear (polygonShape from) (polygonShape to))
          # mapA (translate (-4) (-0.5))
          # mapA (withFillColor "lightgreen")
          # signalA (curveS 4)
        fork $ play $ mkAnimation 4 (morph myMorph (polygonShape from) (polygonShape to))
          # mapA (translate (4) (-0.5))
          # mapA (withFillColor "cyan")
          # signalA (curveS 4)
    myMorph = linear{morphTrajectory = lineBend }
    pairs = zip stages (tail stages ++ [head stages])
    stages =
      [ spike1
      , spike2
      , spike3
      ]

genTrails :: [(Double -> Polygon)] -> SVG
genTrails plotters =
    withFillOpacity 0 $
    withStrokeWidth (defaultStrokeWidth*0.5) $
    withStrokeColor "black" $
    withStrokeDashArray [0.1,0.05] $
    mkGroup $ map mkTrail $ transpose $ concat
      [
        [ V.toList $ V.map (fmap realToFrac) poly
        | n <- [0..steps]
        , let poly = plotter (fromIntegral n / fromIntegral steps)
        ]
      | plotter <- plotters ]
  where
    steps = 100 :: Int
    mkTrail lst = mkLinePath [ (x,y) | V2 x y <- lst ]
