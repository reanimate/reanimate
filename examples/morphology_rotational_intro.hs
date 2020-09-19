#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

import           Codec.Picture
import           Control.Lens               ((&))
import           Data.List                  (transpose)
import qualified Data.Vector                as V
import           Graphics.SvgTree           (LineJoin (..))
import           Linear.V2
import           Linear.Vector
import           Reanimate
import           Reanimate.Math.Polygon
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear
import           Reanimate.Morph.Rotational

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

spike1 :: Polygon
spike1 = pScale 2.5 $ mkPolygon $ V.map (\x -> x - V2 1 1) $ V.fromList
  [ V2 0 0, V2 2 0
  , V2 2 1 ]

spike2 :: Polygon
spike2 = pScale 2.5 $ mkPolygon $ V.map (\x -> x - V2 1 1) $ V.fromList
  [ V2 0 0, V2 2 0
  , V2 0 1 ]

polygonShape :: Polygon -> SVG
polygonShape p = mkLinePathClosed
  [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList (polygonPoints p) ]


main :: IO ()
main = reanimate $
  addStatic (mkBackgroundPixel bgColor) $
  mapA (withStrokeWidth defaultStrokeWidth) $
  mapA (withStrokeColor "black") $
  mapA (withStrokeLineJoin JoinRound) $
  mapA (withFillOpacity 1) $
    scene $ do
      _ <- newSpriteSVG $
        withStrokeWidth 0 $ translate (-4) 4 $
        center $ latex "linear"
      _ <- newSpriteSVG $
        withStrokeWidth 0 $ translate (4) 4 $
        center $ latex "rotational"
      originVar <- newVar (0.5, 0.5)

      showTrails originVar

      let pushLeft newOrigin = do
            fork $ tweenVar originVar 0.5 $ \(x,y) t ->
                let t' = curveS 2 t in
                (fromToS x (fst newOrigin) t', fromToS y (snd newOrigin) t')
            showPair originVar spike1 spike2
          pushRight newOrigin = do
                fork $ tweenVar originVar 0.5 $ \(x,y) t ->
                    let t' = curveS 2 t in
                    (fromToS x (fst newOrigin) t', fromToS y (snd newOrigin) t')
                showPair originVar spike2 spike1

      pushLeft (1, 0.5)
      pushRight (0.1, 0.95)
      pushLeft (0.5, 1)
      pushRight (0.5, 2)
      pushLeft (-0.3, -0.2)
      pushRight (0.5, 0.5)
  where
    showTrails originVar = do
      _ <- newSpriteSVG $
            translate (-4) (-0.5) $
            genTrails (map linearTrajectory [(spike1, spike2)])
      _ <- newSprite $ do
            origin <- unVar originVar
            pure $
              translate (4) (-0.5) $
              genTrails (map (rotationalTrajectory origin) [(spike1, spike2)])
      return ()
    showPair originVar from to =
      waitOn $ do
        fork $ play $ mkAnimation 2 (morph linear (polygonShape from) (polygonShape to))
          & mapA (translate (-4) (-0.5))
          & mapA (withFillColor "lightgreen")
          & signalA (curveS 4)
        s <- fork $ newSprite $ do
          origin <- unVar originVar
          t <- spriteT
          d <- spriteDuration
          pure $
            let localTime = curveS 4 (t/d)
                myMorph = linear{morphTrajectory = rotationalTrajectory origin }
                originSrc = polygonOrigin from origin
                originDst = polygonOrigin to origin
                V2 originX originY = lerp localTime originDst originSrc
            in
            translate (4) (-0.5) $ mkGroup
            [ withFillColor "cyan" $
              morph myMorph (polygonShape from) (polygonShape to) localTime
            , withFillColor "red" $ translate originX originY $
              mkCircle 0.1
            ]
        wait 2
        destroySprite s

genTrails :: [(Double -> Polygon)] -> SVG
genTrails plotters =
    withFillOpacity 0 $
    withStrokeWidth (defaultStrokeWidth*0.5) $
    withStrokeColor "black" $
    withStrokeDashArray [0.1,0.05] $
    mkGroup $ map mkTrail $ transpose $ concat
      [
        [ V.toList $ V.map (fmap realToFrac) (polygonPoints poly)
        | n <- [0..steps]
        , let poly = plotter (fromIntegral n / fromIntegral steps)
        ]
      | plotter <- plotters ]
  where
    steps = 100 :: Int
    mkTrail lst = mkLinePath [ (x,y) | V2 x y <- lst ]
