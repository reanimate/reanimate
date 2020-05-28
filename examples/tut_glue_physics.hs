#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Chiphunk.Low
import           Codec.Picture       (PixelRGBA8 (..))
import           Control.Monad       (forM_)
import           Linear.V2           (V2 (..))
import           Reanimate
import           Reanimate.Chiphunk
import           Reanimate.PolyShape
import           Reanimate.Ease
import           System.IO.Unsafe    (unsafePerformIO)

shatter :: Animation
shatter = unsafePerformIO $ do
    bodyStore <- newBodyStore
    let gravity = Vect 0 (-1) -- Gravity points down (negative 1 y/s^2)

    space <- spaceNew
    spaceGravity space $= gravity

    static <- get $ spaceStaticBody space
    ground <- segmentShapeNew static
      (Vect (-screenWidth/2) (-screenHeight/2))
      (Vect (screenWidth/2) (-screenHeight/2)) 0
    shapeFriction ground $= 1
    spaceAddShape space ground

    let toVect (V2 x y) = Vect x y
        vectGroup = plDecompose $ svgToPolyShapes $ center $ scale 4 $
          latex "$F=ma$"

    forM_ vectGroup $ \polygon -> do
      bd <- polygonsToBody space [map toVect polygon]
      bodyPosition bd $= Vect 0 (screenHeight/3)
      addToBodyStore bodyStore bd $
        renderPolyShape $ plFromPolygon polygon

    ani <- simulate space bodyStore fps stepsPerFrame shatterT
    spaceFreeRecursive space
    return $ mapA pp ani
  where
    shatterT = 10
    fps = 60
    stepsPerFrame = 10
    pp = withStrokeWidth 0.01 . withStrokeColor "black" . withFillColor "black"

main :: IO ()
main = reanimate $ parA bg $ sceneAnimation $ do
    play $ shatter
    play $ shatter
      # reverseA
      # setDuration 5
      # signalA (powerS 2)
  where
    bg = animate $ const $ mkBackgroundPixel (PixelRGBA8 252 252 252 0xFF)
