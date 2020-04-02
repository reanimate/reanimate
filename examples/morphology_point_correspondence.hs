#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad                   (replicateM_)
import           Data.List
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import           Graphics.SvgTree                (LineJoin (..))
import           Linear.V2
import           Linear.Vector
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Interpolate
import           Reanimate.Math.Common
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear
import           Reanimate.PolyShape

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

main :: IO ()
main = reanimate $
  addStatic (mkBackgroundPixel bgColor) $
  mapA (withStrokeWidth 0) $
  mapA (withStrokeColor "black") $
  mapA (withFillOpacity 1) $
    sceneAnimation $ do
      let pl1 t = translate (-4) 0 $ mkGroup
            [ lowerTransformations $ scale 3 $ withFillOpacity 1 $
              withStrokeColor "black" $
              withStrokeWidth defaultStrokeWidth $
              withFillColor "lightgreen" $
              polygonShape octogon
            , lowerTransformations $ scale 3 $ polygonNumDots octogon 0
            ]
          pl2 n t = translate 4 0 $ lowerTransformations $ scale 3 $ mkGroup
            [ withFillOpacity 1 $
              withStrokeColor "black" $
              withStrokeWidth defaultStrokeWidth $
              withFillColor "cyan" $
              polygonShape (cyclePolygons star !! (n `mod` length star))
            , polygonNumDots (cyclePolygons star !! (n `mod` length star)) t ]
      offset <- newVar 0
      slide <- newVar 0
      s2 <- newSprite $ pl2 <$> unVar offset <*> unVar slide
      _ <- newSpriteSVG $ pl1 0
      let slideLeft = do
            tweenVar slide 1 $ \v -> fromToS v 1 . curveS 4
            writeVar slide 0
            modifyVar offset succ
      replicateM_ 8 $ do
        offsetVal <- readVar offset
        play $ step (pl1 0) (pl2 offsetVal 0)
          # setDuration 3
        slideLeft
  where
    radius = 2.5
    step from to =
      signalA (curveS 2) $ animate $ morph rawLinear from to
    stage1 = translate (-3) 0 $ withFillColor "red" $ mkCircle radius
    stage2 = translate 3 0 $ withFillColor "blue" $ mkRect (radius*2) (radius*2)
    stage3 = mkGroup
      [translate (-1) (-1) $ withFillColor "green" $ mkRect (radius*0.5) (radius*0.5)
      ,translate 1 (1) $ withFillColor "black" $ mkRect (radius*0.5) (radius*0.5) ]
    stage4 = translate (-3) 0 $ withFillColor "purple" $ mkCircle (radius*0.24)

octogon :: Polygon
octogon = V.fromList
  [ realToFrac <$> V2 (cos phi) (sin phi)
  | n <- [0..7]
  , let phi = n*2*pi/8 + pi/8
  ]

star :: Polygon
star = scalePolygon 0.5 $ cyclePolygons (V.fromList
  [ V2 0 1, V2 (-2) 2, V2 (-1) 0
  , V2 (-2) (-2), V2 0 (-1), V2 2 (-2)
  , V2 1 0, V2 2 2 ]) !! 0

genColor :: Int -> Int -> PixelRGBA8
genColor n m =
    promotePixel $ parula (fromIntegral (n+offset) / fromIntegral (m+offset))
  where
    offset = 5

polygonNumDots :: Polygon -> Double -> SVG
polygonNumDots p t = mkGroup $ reverse
    [ mkGroup
      [ colored n $ withStrokeWidth (defaultStrokeWidth*0.5) $ withStrokeColor "black" $
        translate x y $ pathify $ mkCircle circR
      , withFillColor "black" $
        translate x y $ ppNum n ]
    | n <- [0..length p-1]
    , let a = realToFrac <$> pAccess p n
          b = realToFrac <$> pAccess p (pNext p n)
          V2 x y = lerp t b a ]
  where
    circR = 0.1
    colored n =
      let c = genColor n (length p-1)
      in withFillColorPixel c
    ppNum n = scaleToHeight (circR*1.5) $ center $ latex $ T.pack $ "\\texttt{" ++ show n ++ "}"

polygonShape :: Polygon -> SVG
polygonShape p = mkLinePathClosed
  [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList p  ++ [pAccess p 0] ]
