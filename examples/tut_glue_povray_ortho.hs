#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Reanimate
import           Reanimate.Scene
import           Reanimate.Animation
import           Reanimate.Povray
import           Reanimate.Raster
import           Data.String.Here
import Data.Text (Text)
import Codec.Picture
import           Reanimate.Effect
import           Reanimate.Scene
import           System.Random
import           System.Random.Shuffle
import           Graphics.SvgTree hiding (Text)
import           Control.Lens          ((^.))
import           Control.Monad
import           Codec.Picture.Types
import           Data.Monoid


main :: IO ()
main = reanimate $ parA bg $ sceneAnimation $ do
    xRot <- newVar (-30)
    yRot <- newVar 180
    zRot <- newVar 0
    s <- newSprite $ do
      getX <- freezeVar xRot
      getY <- freezeVar yRot
      getZ <- freezeVar zRot
      return $ \real_t dur t ->
        povraySlow [] $
        script (svgAsPngFile (texture (t/dur))) (getX real_t) (getY real_t) (getZ real_t) 
    wait 2
    let tDuration = 20
    tweenVar yRot tDuration (\t v -> fromToS v (v+180) $ curveS 2 (t/tDuration))
    tweenVar xRot (tDuration/2) (\t v -> fromToS v (v+60) $ curveS 2 (t/(tDuration/2)))
    fork $ do
      wait (tDuration/2)
      tweenVar xRot (tDuration/2) (\t v -> fromToS v (v-60) $ curveS 2 (t/(tDuration/2)))
    --tweenVar zRot 9 (\t v -> fromToS v 360 $ curveS 2 (t/9))
    wait tDuration
    wait 2
    --tweenVar zPos 2 (\t v -> fromToS v 0 $ curveS 3 (t/2))
    --wait 2
  where
    bg = animate $ const $ mkBackgroundPixel $ PixelRGBA8 252 252 252 0xFF

texture :: Double -> SVG
texture t = mkGroup
  [ checker 10 10
  , frameAt (t*duration latexExample) latexExample
  ]

script :: FilePath -> Double -> Double -> Double -> Text
script png rotX rotY rotZ = [iTrim|
//Files with predefined colors and textures
#include "colors.inc"

#include "shapes3.inc"

//Place the camera
camera {
  orthographic
  location <0,0,-10>
  look_at  <0,0,0>
  up <0,9,0>
  right <16,0,0>
}


//Ambient light to "brighten up" darker pictures
global_settings { ambient_light White*3 }

//Set a background color
//background { color White }
//background { color rgbt <0.1, 0, 0, 0> } // red
background { color rgbt <0, 0, 0, 1> } // transparent

//Sphere with specified center point and radius
sphere {
  <0,0,0>, 4
  texture {
    uv_mapping pigment{
      image_map{ png ${png} }
    }
  }
  rotate <0,${rotY},${rotZ}>
  rotate <${rotX},0,0>
}

|]

checker :: Int -> Int -> SVG
checker w h =
  withFillColor "white" $
  withStrokeColor "white" $
  withStrokeWidth 0.1 $
  mkGroup
  [ withStrokeWidth 0 $
    withFillOpacity 0.7 $ mkBackground "blue"
  , mkGroup
    [ translate (stepX*x-offsetX + stepX/2) 0 $
      mkLine (0, -screenHeight/2*0.9) (0, screenHeight/2*0.9)
    | x <- map fromIntegral [0..w-1]
    ]
  ,
    mkGroup
    [ translate 0 (stepY*y-offsetY) $
      mkLine (-screenWidth/2, 0) (screenWidth/2, 0)
    | y <- map fromIntegral [0..h]
    ]
  ]
  where
    stepX = screenWidth/fromIntegral w
    stepY = screenHeight/fromIntegral h
    offsetX = screenWidth/2
    offsetY = screenHeight/2




-----------------------------------
-- COPIED FROM tut_glue_latex.hs --


latexExample :: Animation
latexExample = sceneAnimation $ do
    -- Draw equation
    play $ drawAnimation strokedSvg
    sprites <- forM glyphs $ \(fn, _, elt) ->
      newSpriteA $ animate $ const $ fn elt
    -- Yoink each glyph
    forM_ (reverse sprites) $ \sprite -> do
      spriteE sprite (overBeginning 1 $ aroundCenterE $ highlightE)
      wait 0.5
    -- Flash glyphs randomly with color
    forM_ (shuffleList (sprites++sprites)) $ \sprite -> do
      spriteE sprite (overBeginning 0.5 $ aroundCenterE $ flashE)
      wait 0.1
    wait 0.5
    mapM_ destroySprite sprites
    -- Undraw equations
    play $ drawAnimation' (Just 0xdeadbeef) 1 0.1 strokedSvg
      # reverseA
  where
    glyphs = svgGlyphs svg
    strokedSvg =
      withStrokeWidth (defaultStrokeWidth*0.5) $
      withStrokeColor "black" svg
    svg = lowerTransformations $ simplify $ scale 2 $ center $
      latexAlign "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
    bg = animate $ const $ mkBackgroundPixel (PixelRGBA8 252 252 252 0xFF)
    shuffleList lst = shuffle' lst (length lst) (mkStdGen 0xdeadbeef)

highlightE :: Effect
highlightE d t =
  scale (1 + bellS 2 (t/d)*0.5) . rotate (wiggleS (t/d) * 20)

flashE :: Effect
flashE d t =
  withStrokeColor "black" .
  withStrokeWidth (defaultStrokeWidth*0.5*bellS 2 (t/d)) .
  withFillColorPixel (promotePixel $ turbo (t/d))

-- s-curve, sin, s-curve
wiggleS :: Signal
wiggleS t
  | t < 0.25  = curveS 2 (t*4)
  | t < 0.75  = sin ((t-0.25)*2*pi+pi/2)
  | otherwise = curveS 2 ((t-0.75)*4)-1

--

drawAnimation :: SVG -> Animation
drawAnimation = drawAnimation' Nothing 0.5 0.3

drawAnimation' :: Maybe Int -> Double -> Double -> SVG -> Animation
drawAnimation' mbSeed fillDur step svg = sceneAnimation $ do
  forM_ (zip [0..] $ shuf $ svgGlyphs svg) $ \(n, (fn, attr, tree)) -> do
    let sWidth =
          case toUserUnit defaultDPI <$> getLast (attr ^. strokeWidth) of
            Just (Num d) -> d
            _            -> defaultStrokeWidth
    fork $ do
      wait (n*step)
      play $ mapA fn $ (animate (\t -> withFillOpacity 0 $ partialSvg t tree)
        # applyE (overEnding fillDur $ fadeLineOutE sWidth))
    fork $ do
      wait (n*step+(1-fillDur))
      newSprite $ do
        return $ \_real_t _d t ->
          withStrokeWidth 0 $ fn $ withFillOpacity (min 1 $ t/fillDur) tree
  where
    shuf lst =
      case mbSeed of
        Nothing -> lst
        Just seed -> shuffle' lst (length lst) (mkStdGen seed)

