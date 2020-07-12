#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import qualified Data.Text         as T
import           NeatInterpolation
import           Reanimate

main :: IO ()
main = reanimate $ mkAnimation 5 $ \t ->
    let s = t * pi * 2 in
    mkGroup
    [ mkBackground "black"
    , blender (script $ T.pack $ show s)
    ]

texture :: T.Text
texture = T.pack $ svgAsPngFile (mkGroup
  [ checker 10 10
  , withFillColor "red" $
    scale 2 $ center $
    latexAlign "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"])

script :: T.Text -> T.Text
script s = [text|
import os

import bpy

origin = bpy.data.objects['Cube']

bpy.ops.object.select_all(action='DESELECT')
origin.select_set(True)
bpy.ops.object.delete()

bpy.ops.mesh.primitive_uv_sphere_add(segments=100,ring_count=100)
sphere = bpy.context.object
sphere.rotation_euler = (0,0,${s})
bpy.ops.object.shade_smooth()

# bpy.data.materials.new('Prog mat')
bpy.context.object.active_material = bpy.data.materials['Material']
mat = bpy.context.object.active_material
image_node = mat.node_tree.nodes.new('ShaderNodeTexImage')
texture = mat.node_tree.nodes['Principled BSDF']
texture.inputs['Roughness'].default_value = 1
mat.node_tree.links.new(image_node.outputs['Color'], texture.inputs['Base Color'])

image_node.image = bpy.data.images.load('${texture}')

scn = bpy.context.scene
#scn.render.engine = 'CYCLES'
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
