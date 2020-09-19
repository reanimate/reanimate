#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens           ((&))
import           Control.Monad          (replicateM_)
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Linear.V2
import           Linear.Vector
import           Reanimate
import           Reanimate.Math.Polygon
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

main :: IO ()
main = reanimate $
  addStatic (mkBackgroundPixel bgColor) $
  mapA (withStrokeWidth 0) $
  mapA (withStrokeColor "black") $
  mapA (withFillOpacity 1) $
    scene $ do
      let pl1 = translate (-4) 0 $ mkGroup
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
              polygonShape (pSetOffset star n)
            , polygonNumDots (pSetOffset star n) t ]
      offset <- newVar 0
      slide <- newVar 0
      _ <- newSprite $ pl2 <$> unVar offset <*> unVar slide
      _ <- newSpriteSVG $ pl1
      let slideLeft = do
            tweenVar slide 1 $ \v -> fromToS v 1 . curveS 4
            writeVar slide 0
            modifyVar offset succ
      replicateM_ 8 $ do
        offsetVal <- readVar offset
        play $ step pl1 (pl2 offsetVal 0)
          & setDuration 3
        slideLeft
  where
    step from to =
      signalA (curveS 2) $ animate $ morph rawLinear from to

octogon :: Polygon
octogon = mkPolygon $ V.fromList
  [ realToFrac <$> V2 (cos phi) (sin phi)
  | n <- [0..7]
  , let phi = n*2*pi/8 + pi/8 :: Double
  ]

star :: Polygon
star = pScale 0.5 $ mkPolygon $ V.fromList
  [ V2 0 1, V2 (-2) 2, V2 (-1) 0
  , V2 (-2) (-2), V2 0 (-1), V2 2 (-2)
  , V2 1 0, V2 2 2 ]

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
    | n <- [0..pSize p-1]
    , let a = realToFrac <$> pAccess p n
          b = realToFrac <$> pAccess p (pNext p n)
          V2 x y = lerp t b a ]
  where
    circR = 0.1
    colored n =
      let c = genColor n (pSize p-1)
      in withFillColorPixel c
    ppNum n = scaleToHeight (circR*1.5) $ center $ latex $ T.pack $ "\\texttt{" ++ show n ++ "}"

polygonShape :: Polygon -> SVG
polygonShape p = mkLinePathClosed
  [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList (polygonPoints p) ]
