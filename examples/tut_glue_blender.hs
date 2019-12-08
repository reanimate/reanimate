#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE BangPatterns       #-}
module Main (main) where

import           Codec.Picture
import           Data.String.Here
import qualified Data.Text         as T
import           Reanimate
import           Reanimate.Blender
import           Reanimate.Raster
import           Reanimate.Scene
import Graphics.SvgTree
import System.IO.Unsafe
import qualified Data.Text.IO as T

main :: IO ()
main = seq texture $ reanimate $ parA bg $ sceneAnimation $ do
    bend <- newVar 0
    trans <- newVar 0
    rotX <- newVar 0
    rotY <- newVar 0
    _ <- newSprite $ do
      getBend <- freezeVar bend
      getTrans <- freezeVar trans
      getRotX <- freezeVar rotX
      getRotY <- freezeVar rotY
      return $ \real_t dur t ->
        blender (script (getBend real_t) (getTrans real_t) (getRotX real_t) (getRotY real_t))
    tweenVar trans 5 (\t v -> fromToS v (-2) $ curveS 2 (t/5))
    tweenVar bend 5 (\t v -> fromToS v 1 $ curveS 2 (t/5))
    tweenVar rotY 15 (\t v -> fromToS v (pi*2*2) $ curveS 2 (t/15))
    fork $ do
      tweenVar rotX 5 (\t v -> fromToS v (-pi/5) $ curveS 2 (t/5))
      wait 5
      tweenVar rotX 5 (\t v -> fromToS v (pi/5) $ curveS 2 (t/5))
    wait (15-5)
    tweenVar bend 5 (\t v -> fromToS 1 0 $ curveS 2 (t/5))
    tweenVar rotX 5 (\t v -> fromToS v 0 $ curveS 2 (t/5))
    tweenVar trans 5 (\t v -> fromToS v 0 $ curveS 2 (t/5))
    wait 4
    -- tweenVar trans 1 (\t v -> fromToS v 0 $ curveS 2 t)
    wait 1
  where
    bg = animate $ const $ mkBackground "grey"

texture :: FilePath
texture = svgAsPngFile (mkGroup
  [ checker 10 10
  , withFillColor "red" $
    scale 2 $ center $
    latexAlign "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"])

script :: Double -> Double -> Double -> Double -> T.Text
script bend transZ rotX rotY = [iTrim|
import os
import math

import bpy

cam = bpy.data.objects['Camera']
cam.location = (0,0,22.25 + ${transZ})
cam.rotation_euler = (0, 0, 0)
bpy.ops.object.empty_add(location=(0.0, 0, 0))
focus_target = bpy.context.object
bpy.ops.object.select_all(action='DESELECT')
cam.select_set(True)
focus_target.select_set(True)
bpy.ops.object.parent_set()

focus_target.rotation_euler = (${rotX}, 0, 0)


origin = bpy.data.objects['Cube']
bpy.ops.object.select_all(action='DESELECT')
origin.select_set(True)
bpy.ops.object.delete()

x = ${bend}
bpy.ops.mesh.primitive_plane_add()
plane = bpy.context.object
plane.scale = (16/2,${fromToS (9/2) 4 bend},1)
bpy.ops.object.shade_smooth()

bpy.context.object.active_material = bpy.data.materials['Material']
mat = bpy.context.object.active_material
image_node = mat.node_tree.nodes.new('ShaderNodeTexImage')
texture = mat.node_tree.nodes['Principled BSDF']
texture.inputs['Roughness'].default_value = 1
mat.node_tree.links.new(image_node.outputs['Color'], texture.inputs['Base Color'])

image_node.image = bpy.data.images.load('${T.pack texture}')


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
print(bpy.ops.object.modifier_apply(modifier='Subsurf'))
print(bpy.ops.object.modifier_apply(modifier='Bend up'))
print(bpy.ops.object.modifier_apply(modifier='Bend around'))

bpy.ops.object.select_all(action='DESELECT')
plane.select_set(True);
#bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
bpy.ops.object.origin_clear()
bpy.ops.object.origin_set(type='GEOMETRY_ORIGIN')

plane.rotation_euler = (0, ${rotY}, 0)

scn = bpy.context.scene
# scn.render.engine = 'CYCLES'
scn.render.film_transparent = True

bpy.ops.render.render( write_still=True )
|]


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
