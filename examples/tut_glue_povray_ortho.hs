#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE PackageImports    #-}
module Main (main) where

import           Reanimate
import           Reanimate.Povray

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens          ((^.),(&))
import           Control.Monad
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Graphics.SvgTree      hiding (Text)
import           NeatInterpolation
import           System.Random
import "random-shuffle" System.Random.Shuffle


main :: IO ()
main = reanimate $ parA bg $ scene $ do
    xRot <- newVar (-30)
    yRot <- newVar 180
    zRot <- newVar 0
    newSprite_ $ do
      getX <- unVar xRot
      getY <- unVar yRot
      getZ <- unVar zRot
      t <- spriteT
      dur <- spriteDuration
      return $
        povraySlow [] $
        script (svgAsPngFile (texture (t/dur))) getX getY getZ
    wait 2
    let tDuration = 10
    fork $ tweenVar yRot tDuration $ \v -> fromToS v (v+180) . curveS 2
    fork $ tweenVar xRot (tDuration/2) $ \v -> fromToS v (v+60) . curveS 2
    fork $ do
      wait (tDuration/2)
      tweenVar xRot (tDuration/2) $ \v -> fromToS v (v-60) . curveS 2
    wait tDuration
    wait 2
  where
    bg = animate $ const $ mkBackgroundPixel $ PixelRGBA8 252 252 252 0xFF

texture :: Double -> SVG
texture t = mkGroup
  [ checker 20 20
  , frameAt (t*duration latexExample) latexExample
  ]

script :: FilePath -> Double -> Double -> Double -> Text
script png rotX rotY rotZ =
  let png_ = T.pack png
      rotX_ = T.pack $ show rotX
      rotY_ = T.pack $ show rotY
      rotZ_ = T.pack $ show rotZ
  in [text|
//Files with predefined colors and textures
#include "colors.inc"

#include "shapes3.inc"

//Place the camera
camera {
  orthographic
  location <0,0,-10>
  look_at  <0,0,0>
  up y*9
  right x*16
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
      image_map{ png "${png_}" }
    }
  }
  rotate <0,${rotY_},${rotZ_}>
  rotate <${rotX_},0,0>
}

|]

checker :: Int -> Int -> SVG
checker w h =
  withStrokeColor "lightblue" $
  withStrokeWidth (defaultStrokeWidth/2) $
  mkGroup
  [ withStrokeWidth 0 $
    withFillOpacity 0.8 $ mkBackground "white"
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
latexExample = scene $ do
    -- Draw equation
    play $ drawAnimation strokedSvg
    sprites <- forM glyphs $ \(fn, _, elt) ->
      newSpriteSVG $ fn elt
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
      & reverseA
  where
    glyphs = svgGlyphs svg
    strokedSvg =
      withStrokeWidth (defaultStrokeWidth*0.5) $
      withStrokeColor "black" svg
    svg = lowerTransformations $ simplify $ scale 2 $ center $
      latexAlign "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
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
drawAnimation' mbSeed fillDur step svg = scene $ do
  forM_ (zip [0..] $ shuf $ svgGlyphs svg) $ \(n, (fn, attr, tree)) -> do
    let sWidth =
          case toUserUnit defaultDPI <$> getLast (attr ^. strokeWidth) of
            Just (Num d) -> d
            _            -> defaultStrokeWidth
    fork $ do
      wait (n*step)
      play $ mapA fn $ (animate (\t -> withFillOpacity 0 $ partialSvg t tree)
        & applyE (overEnding fillDur $ fadeLineOutE sWidth))
    fork $ do
      wait (n*step+(1-fillDur))
      newSprite $ do
        t <- spriteT
        return $
          withStrokeWidth 0 $ fn $ withFillOpacity (min 1 $ t/fillDur) tree
  where
    shuf lst =
      case mbSeed of
        Nothing   -> lst
        Just seed -> shuffle' lst (length lst) (mkStdGen seed)
