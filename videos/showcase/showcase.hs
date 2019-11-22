#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Codec.Picture
import           Control.Lens             ((^.))
import           Control.Monad
import           Data.Fixed
import           Data.Maybe
import           Data.Monoid
import           Data.String.Here
import           Data.Text                (Text)
import           Graphics.SvgTree         (Number (..), strokeWidth, toUserUnit)
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Builtin.Images
import           Reanimate.Effect
import           Reanimate.Povray
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Signal
import           Reanimate.Svg

{- SCRIPT

Some ideas lend themselves well to be illustrated. Take spheres, for example: It
just so happens that the surface of a sphere is exactly 4 times the area of a
circle with the same radius.

Now, this relationship has already been visually explored by others in mucher
greater detail so I'll leave it at this. But there are countless other ideas and
concepts that deserve to be illustrated yet have not. I want to remedy this, in
part, by animating ideas I find interesting, but also by encouraging you to make
your own animations.

Every video on this channel, including this one, will be open-source with the source files
linked in the description. If any of the graphics pique your interest then
please download the code and play with it. The animations are created with a
library called 'reanimate' which, at its core, is a set of tools for generating
individual frames as SVG images. Layered on top of this core are external
components such as:

 * LaTeX, for typesetting equations,
 * a bitmap tracer, for turning pixel data into vector graphics,
 * and a raytracer, for generating 3D graphics,

Raytracing is inherently a pixel-based graphics technique but, with a bit of math,
it and vector graphics can be overlayed with pixel-perfect precision.

Everything is held together by the purely functional language Haskell. Haskell
is particularlly well suited for this task since animations are inherently
immutable.




-}

main :: IO ()
main = reanimate $ animate (const $ mkBackground "black") `parA`
  -- animate $ const $ checker 10 10
  -- rotateSphere
  -- rotateWireSphere
  sphereIntro
  -- introSVG
  -- mapA (scale 2) $ setDuration 20 featLaTeX

playbackTest :: Animation
playbackTest = setDuration 10 feat3D

sphereIntro :: Animation
sphereIntro = sceneAnimation $ do
  -- play $ drawSphere
  --   # setDuration 15
  --   # pauseAtEnd 2
  -- play $ rotateWireSphere
  --   # setDuration 2
  --   # signalA (powerS 2)
  fork $ play $ rotateWireSphere
    # setDuration 1
    # repeatA 10
    # takeA (2+5)
    # applyE (delayE 2 fadeOutE)
  sphereX <- newVar 0
  sphereS <- newSpriteA $
    rotateSphere # repeatA (5+3+1+2)
  -- sphereS <- newSprite $ do
  --   -- xValue <- freezeVar sphereX
  --   let a = rotateSphere # setDuration 1 # repeatA (5+3+1+2)
  --   return $ \real_t d t ->
  --     -- translate (xValue real_t) 0 $
  --     frameAt t a
  applyVar sphereX sphereS (\xValue -> translate xValue 0)

  spriteE sphereS (overBeginning 1 $ constE $ withGroupOpacity 0)
  spriteE sphereS (delayE 1 $ overBeginning 2 fadeInE)
  -- fork $ play $ rotateSphere
  --   # setDuration 1
  --   # repeatA (5+3+1+2)
  --   # applyE (overBeginning 1 $ constE $ withGroupOpacity 0)
  --   # applyE (delayE 1 $ overBeginning 2 fadeInE)
  --   # applyE (delayE 8 $ translateE (-3) 0)
  wait 5
  playZ 1 $ setDuration 3 $ animate $ \t ->
    partialSvg t $
    withFillOpacity 0 $
    rotate 180 $
    pathify $
    circ
  -- playZ 1 $ pauseAtEnd 2 $ setDuration 1 $ animate $ \t ->
  --   withFillOpacity t $
  --   circ
  let scaleFactor = 0.05
  tweenVar sphereX 1 $ \t x -> fromToS x (-3) (curveS 3 t)
  fork $ playZ 1 $ pauseAtEnd 2 $ setDuration 1 $ animate $ \t ->
    let p = curveS 3 t in
    withFillOpacity p $
    translate (1*p) (2*p) $
    scale (1-scaleFactor*p) $
    circ
  fork $ playZ 1 $ pauseAtEnd 2 $ setDuration 1 $ animate $ \t ->
    let p = curveS 3 t in
    withFillOpacity p $
    translate (1*p) (-2*p) $
    scale (1-scaleFactor*p) $
    circ
  fork $ playZ 1 $ pauseAtEnd 2 $ setDuration 1 $ animate $ \t ->
    let p = curveS 3 t in
    withFillOpacity p $
    translate (5*p) (2*p) $
    scale (1-scaleFactor*p) $
    circ
  fork $ playZ 1 $ pauseAtEnd 2 $ setDuration 1 $ animate $ \t ->
    let p = curveS 3 t in
    withFillOpacity p $
    translate (5*p) (-2*p) $
    scale (1-scaleFactor*p) $
    circ
  where
    circ =
      withFillColor "blue" $
      withStrokeColor "white" $
      mkCircle 2
    circEq = mkGroup
      [ circ
      , withFillColor "white" $
        scale 0.5 $
        center $ latexAlign "A=\\pi r^2"]

mkFeatSprite :: Double -> Double -> Animation
             -> Scene s (Var s Double, Var s Double, Sprite s)
mkFeatSprite xPos yPos ani = do
  spriteAt <- newVar 0
  spriteTMod <- newVar 0
  sprite <- newSprite $ do
    genAt <- freezeVar spriteAt
    genT <- freezeVar spriteTMod
    return $ \real_t d t ->
      let i = 1-genAt real_t in
      translate (xPos*i) (yPos*i) $
      scale (1+0.5*genAt real_t) $
      frameAtT (((t+genT real_t)/d) `mod'` 1) ani
  return (spriteAt, spriteTMod, sprite)

featSVG :: Animation
featSVG = animate $ const $ scale 0.4 $ svgLogo

feat3D :: Animation
feat3D = rotateSphere
  # mapA (scale 0.5)
  # repeatA 10

frameAtT :: Double -> Animation -> SVG
frameAtT t (Animation d f) = f t

featLaTeX :: Animation
featLaTeX = animate $ \t ->
    translate 0 0.5 $
    mkGroup
    [ scale 1.5 $
      center $
      withFillColor "white" $
      latex "\\LaTeX"
    , frameAtT t $
      fadeTransitions 0.2 $ map mkEQ [eq1, eq3, eq4, eq5]
    ]
  where
    eq1 = "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
    eq2 = "e=mc^2"
    -- eq3 = "\\int_{a}^{b}f'(x)dx=f(b)-f(a)"
    eq3 = "\\Delta \\times E=- \\frac{\\partial B}{\\partial t}"
    eq4 = "\\frac{d}{dt}(\\frac{\\partial L}{\\partial \\dot{q}}) = \\frac{\\partial L}{\\partial q}"
    eq5 = "\\vec{E} = \\frac{\\sigma}{2\\epsilon_0}\\hat{n}"
    mkEQ txt =
      drawAnimation $
      withStrokeColor "white" $
      withFillColor "white" $
      withStrokeWidth 0.01 $
      translate 0 (-1.5) $
      scale 0.5 $
      center $
      latexAlign txt

fadeTransition :: Double -> Animation -> Animation -> Animation
fadeTransition overlap a b =
  (a
  # pauseAtEnd overlap
  # applyE (overEnding overlap $ fadeOutE)
  ) `seqA` (
  b
  # applyE (overBeginning overlap $ fadeInE)
  )

fadeTransitions :: Double -> [Animation] -> Animation
fadeTransitions overlap = foldl (fadeTransition overlap) (pause 0)

featWireSphere :: Animation
featWireSphere = rotateWireSphere
  # mapA (scale 0.5)
  # reverseA
  # repeatA 10

introSVG :: Animation
introSVG = sceneAnimation $ do
  fork $ play $ animate $ const $
    mkBackground "black"
  -- Title
  title <- newSprite $ do
    return $ \_ _d _t ->
      translate 0 3.5 $
      center $
      withFillColor "white" $
      latex "reanimate"
  spriteZ title 2
  -- Shading
  shadeOpacity <- newVar 0
  shade <- newSprite $ do
    opacity <- freezeVar shadeOpacity
    return $ \real_t d t ->
      withFillOpacity (0.8 * opacity real_t) $
      withFillColor "black" $
      mkRect screenWidth screenHeight
  spriteZ shade 1
  -- Modifier
  let tweenFeat sp var varT initWait dur = do
        wait initWait
        spriteZ sp 2
        tweenVar shadeOpacity 1 $ \t i -> fromToS i 1 (curveS 2 t)
        tweenVar var 1 $ \t i -> fromToS i 1 (curveS 2 t)
        tweenVar varT 1 $ \t i -> fromToS i 1 (curveS 2 t)
        wait 1
        tweenVar varT dur $ \t i -> fromToS i (1+dur) (t/dur)
        wait dur
        tweenVar var 1 $ \t i -> fromToS i 0 (curveS 2 t)
        tweenVar varT dur $ \t i -> fromToS i (1+dur+1) $ curveS 2 (t/dur)
        tweenVar shadeOpacity 1 $ \t i -> fromToS i 0 (curveS 2 t)
        wait 1
        spriteZ sp 0
  -- SVG
  (svgAt, svgT, svgS) <- mkFeatSprite (-5.5) (1.5) featSVG
  fork $ tweenFeat svgS svgAt svgT svgHighlight svgHighlightDur
  -- LaTeX
  (latexAt, latexT, latexS) <- mkFeatSprite (5.5) (1.5) featLaTeX
  fork $ tweenFeat latexS latexAt latexT latexHighlight latexHighlightDur
  -- Tracing
  (traceAt, traceT, traceS) <- mkFeatSprite (-5.5) (-2.5) featWireSphere
  fork $ tweenFeat traceS traceAt traceT traceHighlight traceHighlightDur
  -- Raytracing
  (rayAt, rayT, rayS) <- mkFeatSprite (5.5) (-2.5) feat3D
  fork $ tweenFeat rayS rayAt rayT rayHighlight rayHighlightDur
  -- wait
  wait $ rayHighlight + rayHighlightDur + 2 + 10
  return ()
  where
    svgHighlight = 1
    svgHighlightDur = 3
    latexHighlight = svgHighlight+svgHighlightDur+2
    latexHighlightDur = 3
    traceHighlight = latexHighlight+latexHighlightDur+2
    traceHighlightDur = 3
    rayHighlight = traceHighlight+traceHighlightDur+2
    rayHighlightDur = 3

drawAnimation :: SVG -> Animation
drawAnimation = drawAnimation' 0.5 0.3

drawAnimation' :: Double -> Double -> SVG -> Animation
drawAnimation' fillDur step svg = sceneAnimation $ do
  forM_ (zip [0..] $ svgGlyphs svg) $ \(n, (fn, attr, tree)) -> do
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
        return $ \_real_t d t ->
          withStrokeWidth 0 $ fn $ withFillOpacity (min 1 $ t/fillDur) tree
      -- play $ animate (\t -> withStrokeWidth 0 $ fn $ withFillOpacity t tree)
      --   # setDuration fillDur
      --   # pauseAtEnd ((len-n)*step)
  where
    len = fromIntegral $ length $ svgGlyphs svg

drawSphere :: Animation
drawSphere = animate $ \t ->
    partialSvg t $
    withStrokeColor "white" $
    withStrokeWidth 0.01 $
    withFillOpacity 0 $
    lowerTransformations $
    flipYAxis $
    translate (-screenWidth/2) (-screenHeight/2) $
    scale 0.00625 $
    mkPath $ extractPath $
    vectorize_ ["-i"] $
    povraySlow' [] (script (svgAsPngFile texture) 0)

rotateWireSphere :: Animation
rotateWireSphere = animate $ \t ->
    withStrokeColor "white" $
    withStrokeWidth 0.01 $
    withFillOpacity 0 $
    lowerTransformations $
    flipYAxis $
    translate (-screenWidth/2) (-screenHeight/2) $
    scale (screenWidth/2560) $
    mkPath $ extractPath $
    vectorize_ ["-i"] $
    povraySlow' [] (script (svgAsPngFile texture) (t*360/10))

rotateSphere :: Animation
rotateSphere = animate $ \t ->
    povraySlow [] (script (svgAsPngFile texture) (t*360/10))

texture :: SVG
texture = checker 10 10

script :: FilePath -> Double -> Text
script png s = [iTrim|
//EXAMPLE OF SPHERE

//Files with predefined colors and textures
#include "colors.inc"
#include "glass.inc"
#include "golds.inc"
#include "metals.inc"
#include "stones.inc"
#include "woods.inc"

#include "shapes3.inc"

//Place the camera
camera {
  orthographic
  // angle 50
  location <0,0,-10>
  look_at  <0,0,0>
  //right x*image_width/image_height
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
  <0,0,0>, 2
  texture {
    //pigment{ color rgbt <0,0,1,0.1> }
    uv_mapping pigment{
      image_map{ png ${png} }
      //color rgbt <0,0,1,0.1>
    }
  }
  rotate <0,${s'},0>
  rotate <-30,0,0>
}
             |]
  where
    precision = 0.1
    s' = fromIntegral (round (s / precision)) * precision

checker :: Int -> Int -> SVG
checker w h =
  withFillColor "white" $
  withStrokeColor "white" $
  withStrokeWidth 0.1 $
  mkGroup
  [ withStrokeWidth 0 $
    withFillOpacity 0.8 $ mkBackground "blue"
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
