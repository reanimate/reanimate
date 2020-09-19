#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE PackageImports    #-}
module Main (main) where

import           Reanimate
import           Reanimate.Builtin.Documentation

import           Codec.Picture.Types
import           Control.Lens                    ((&), (^.))
import           Control.Monad
import           Data.Monoid
import qualified Data.Text                       as T
import           Graphics.SvgTree
import           NeatInterpolation
import           System.Random
import "random-shuffle" System.Random.Shuffle

-- spritePercent = (/) <$> spriteT <*> spriteDur

main :: IO ()
main = seq texture $ reanimate $ pauseAtEnd 1 $ addStatic bg $ scene $ do
    bend <- newVar 0
    trans <- newVar 0
    rotX <- newVar 0
    rotY <- newVar 0
    newSprite_ $ do
      getBend <- unVar bend
      getTrans <- unVar trans
      getRotX <- unVar rotX
      getRotY <- unVar rotY
      t <- spriteT
      dur <- spriteDuration
      return $ seq (texture (t/dur)) $
        blender (script (texture (t/dur)) getBend getTrans getRotX getRotY)
    wait 2
    fork $ tweenVar trans 5 $ \v -> fromToS v (-2) .  curveS 2
    fork $ tweenVar bend 5 $ \v -> fromToS v 1 . curveS 2
    fork $ tweenVar rotY 15 $ \v -> fromToS v (pi*2*2) . curveS 2
    fork $ do
      tweenVar rotX 5 $ \v -> fromToS v (-pi/5) . curveS 2
      tweenVar rotX 5 $ \v -> fromToS v (pi/5) . curveS 2
    wait (15-5)
    fork $ tweenVar bend 5 $ \v -> fromToS v 0 . curveS 2
    fork $ tweenVar rotX 5 $ \v -> fromToS v 0 . curveS 2
    fork $ tweenVar trans 5 $ \v -> fromToS v 0 . curveS 2
    wait 4
    -- tweenVar trans 1 (\t v -> fromToS v 0 $ curveS 2 t)
    wait 1
    wait 2
  where
    bg = mkBackgroundPixel rtfdBackgroundColor

texture :: Double -> FilePath
texture t = svgAsPngFile $ mkGroup
  [ checker 20 20
  , frameAt (t*duration latexExample) latexExample
  ]

script :: FilePath -> Double -> Double -> Double -> Double -> T.Text
script img bend transZ rotX rotY =
  let img_ = T.pack img
      bend_ = T.pack $ show bend
      transZ_ = T.pack $ show transZ
      rotX_ = T.pack $ show rotX
      rotY_ = T.pack $ show rotY
      yScale_ = T.pack $ show (fromToS (9/2) 4 bend)
      pWidthT = T.pack $ show (max 800 pWidth)
      pHeightT = T.pack $ show (max 450 pHeight)
  in [text|
import os
import math

import bpy

light = bpy.data.objects['Light']
bpy.ops.object.select_all(action='DESELECT')
light.select_set(True)
bpy.ops.object.delete()


cam = bpy.data.objects['Camera']
cam.location = (0,0,22.25 + ${transZ_})
cam.rotation_euler = (0, 0, 0)
bpy.ops.object.empty_add(location=(0.0, 0, 0))
focus_target = bpy.context.object
bpy.ops.object.select_all(action='DESELECT')
cam.select_set(True)
focus_target.select_set(True)
bpy.ops.object.parent_set()

focus_target.rotation_euler = (${rotX_}, 0, 0)


origin = bpy.data.objects['Cube']
bpy.ops.object.select_all(action='DESELECT')
origin.select_set(True)
bpy.ops.object.delete()

x = ${bend_}
bpy.ops.mesh.primitive_plane_add()
plane = bpy.context.object
plane.scale = (16/2,${yScale_},1)
bpy.ops.object.shade_smooth()

bpy.context.object.active_material = bpy.data.materials['Material']
mat = bpy.context.object.active_material
image_node = mat.node_tree.nodes.new('ShaderNodeTexImage')
output = mat.node_tree.nodes['Material Output']
#texture = mat.node_tree.nodes['Principled BSDF']
#texture.inputs['Roughness'].default_value = 1
#mat.node_tree.links.new(image_node.outputs['Color'], texture.inputs['Base Color'])
mat.node_tree.links.new(image_node.outputs['Color'], output.inputs['Surface'])


image_node.image = bpy.data.images.load('${img_}')


modifier = plane.modifiers.new(name='Subsurf', type='SUBSURF')
modifier.levels = 7
modifier.render_levels = 7
modifier.subdivision_type = 'SIMPLE'

bpy.ops.object.empty_add(type='ARROWS',rotation=(math.pi/2,0,0))
empty = bpy.context.object

bendUp = plane.modifiers.new(name='Bend up', type='SIMPLE_DEFORM')
bendUp.deform_method = 'BEND'
bendUp.origin = empty
bendUp.deform_axis = 'X'
bendUp.factor = -math.pi*x

bendAround = plane.modifiers.new(name='Bend around', type='SIMPLE_DEFORM')
bendAround.deform_method = 'BEND'
bendAround.origin = empty
bendAround.deform_axis = 'Z'
bendAround.factor = -math.pi*2*x

bpy.context.view_layer.objects.active = plane
bpy.ops.object.modifier_apply(modifier='Subsurf')
bpy.ops.object.modifier_apply(modifier='Bend up')
bpy.ops.object.modifier_apply(modifier='Bend around')

bpy.ops.object.select_all(action='DESELECT')
plane.select_set(True);
#bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
bpy.ops.object.origin_clear()
bpy.ops.object.origin_set(type='GEOMETRY_ORIGIN')

plane.rotation_euler = (0, ${rotY_}, 0)

scn = bpy.context.scene

scn.view_settings.view_transform = 'Standard'

#scn.render.engine = 'CYCLES'
#scn.render.resolution_percentage = 10

scn.render.resolution_x = ${pWidthT} #3200
scn.render.resolution_y = ${pHeightT} #1800

scn.render.film_transparent = True

bpy.ops.render.render( write_still=True )
|]

checker :: Int -> Int -> SVG
checker w h =
  withStrokeColor "lightgrey" $
  withStrokeWidth (defaultStrokeWidth/2) $
  mkGroup
  [ withStrokeWidth 0 $
    withFillOpacity 1 $ mkBackground "darkgrey"
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
        pure $
          withStrokeWidth 0 $ fn $ withFillOpacity (min 1 $ t/fillDur) tree
  where
    shuf lst =
      case mbSeed of
        Nothing   -> lst
        Just seed -> shuffle' lst (length lst) (mkStdGen seed)
