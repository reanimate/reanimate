#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Chiphunk.Low
import           Control.Monad
import           Graphics.SvgTree    (Tree)
import           Linear.V2
import           Reanimate.Chiphunk
import           Reanimate
import           Reanimate.PolyShape
import           System.IO.Unsafe
import           Codec.Picture

test :: Animation
test = unsafePerformIO $ do
  bodyStore <- newBodyStore
  let gravity = Vect 0 (-1)

  -- Create an empty space.
  space <- spaceNew
  -- spaceCollisionSlop space $= (screenWidth/2560)
  spaceGravity space $= gravity

  static <- get $ spaceStaticBody space
  ground <- segmentShapeNew static
    (Vect (-screenWidth/2) (-screenHeight/2))
    (Vect (screenWidth/2) (-screenHeight/2)) 0
  shapeFriction ground $= 1
  spaceAddShape space ground
  shapeElasticity ground $= 1

  let toVect (V2 x y) = Vect x y

  let svg = center $ scale 4 $ latex "$F=ma$"
      poly = svgToPolyShapes svg
      vectGroup = plDecompose poly

  forM_ vectGroup $ \polygon -> do
    bd <- polygonsToBody space [map toVect polygon]
    bodyPosition bd $= Vect 0 (screenHeight/3)
    addToBodyStore bodyStore bd $
      renderPolyShape $ plFromPolygon polygon

  ani <- simulate space bodyStore 60 10 10
  spaceFreeRecursive space
  return ani


chunkPolyshapes :: Tree -> Tree
chunkPolyshapes t =
  mkGroup
  [
    withStrokeWidth 0.01 $
    withStrokeColor "black" $
    -- withFillOpacity 0 $
    withFillColor "black" $
    mkGroup $ map renderPolyShape $
    svgToPolyShapes t
  ]

reorient :: Tree -> Tree
reorient = id -- scale 6 . translate 0 (-0.9)


main :: IO ()
main = reanimate $ bg `parA` mapA reorient (mapA chunkPolyshapes test)
  where
    bg = animate $ const $ mkBackgroundPixel (PixelRGBA8 252 252 252 0xFF)

