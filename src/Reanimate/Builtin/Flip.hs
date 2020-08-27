{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{- HLINT ignore -}
{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Reanimate.Builtin.Flip
  ( FlipSprite(..)
  , flipSprite
  , flipTransition
  ) where

import           Reanimate.Animation        (Animation, duration, frameAt, setDuration)
import           Reanimate.Blender          (blender)
import           Reanimate.Ease             (fromToS, oscillateS)
import           Reanimate.Raster           (svgAsPngFile)
import           Reanimate.Scene            (Scene, Sprite, Var, fork, newSprite, newVar, scene,
                                             spriteDuration, spriteT, tweenVar, unVar)
import           Reanimate.Svg.Constructors (flipXAxis)
import           Reanimate.Transition       (Transition)

import qualified Data.Text                  as T
import           NeatInterpolation          (text)

-- | Control structure with parameters for the blender script.
data FlipSprite s = FlipSprite
  { fsSprite :: Sprite s
  , fsBend   :: Var s Double
  , fsZoom   :: Var s Double
  , fsWobble :: Var s Double
  }

-- | Project two animations on each side of a plane and flip the plane
--   upside down.
flipSprite :: Animation -> Animation -> Scene s (FlipSprite s)
flipSprite front back = do
    bend <- newVar 0
    trans <- newVar 0
    rotX <- newVar 0
    sprite <- newSprite $ do
      getBend <- unVar bend
      getTrans <- unVar trans
      getRotX <- unVar rotX
      time <- spriteT
      dur <- spriteDuration
      return $
        let rotY = fromToS 0 pi (time/dur)
            frontTexture = svgAsPngFile (frameAt time $ setDuration dur front)
            backTexture = svgAsPngFile (flipXAxis $ frameAt time $ setDuration dur back)
           -- seq'ing frontTexture and backTexture is required to avoid segfaults. :(
        in frontTexture `seq` backTexture `seq`
           blender (script frontTexture backTexture getBend getTrans getRotX rotY)
    return FlipSprite
      { fsSprite = sprite
      , fsBend = bend
      , fsZoom = trans
      , fsWobble = rotX }

flipTransitionOpts :: Double -> Double -> Double -> Transition
flipTransitionOpts bend zoom wobble a b = scene $ do
    FlipSprite{..} <- flipSprite a b
    fork $ tweenVar fsZoom dur   $ \v -> fromToS v zoom . oscillateS
    fork $ tweenVar fsBend dur   $ \v -> fromToS v bend . oscillateS
    fork $ tweenVar fsWobble dur $ \v -> fromToS v wobble . oscillateS
  where
    dur = max (duration a) (duration b)

-- | 3D flip transition.
flipTransition :: Transition
flipTransition = flipTransitionOpts bend zoom wobble
  where
    bend = 1/3
    zoom = 3
    wobble = -pi*0.10

script :: FilePath -> FilePath -> Double -> Double -> Double -> Double -> T.Text
script frontImage backImage bend transZ rotX rotY =
  let transZ_ = T.pack (show transZ)
      rotX_ = T.pack (show rotX)
      bend_ = T.pack (show bend)
      yScale_ = T.pack (show $ fromToS (9/2) 4 bend)
      frontImage_ = T.pack frontImage
      backImage_ = T.pack backImage
      rotY_ = T.pack (show rotY)
  in [text|
import os
import math

import bpy

light = bpy.data.objects['Light']
bpy.ops.object.select_all(action='DESELECT')
light.select_set(True)
bpy.ops.object.delete()


cam = bpy.data.objects['Camera']
cam.location = (0,0,22.22 + $transZ_)
cam.rotation_euler = (0, 0, 0)
bpy.ops.object.empty_add(location=(0.0, 0, 0))
focus_target = bpy.context.object
bpy.ops.object.select_all(action='DESELECT')
cam.select_set(True)
focus_target.select_set(True)
bpy.ops.object.parent_set()

focus_target.rotation_euler = ($rotX_, 0, 0)


origin = bpy.data.objects['Cube']
bpy.ops.object.select_all(action='DESELECT')
origin.select_set(True)
bpy.ops.object.delete()

x = $bend_
bpy.ops.mesh.primitive_plane_add()
plane = bpy.context.object
plane.scale = (16/2,$yScale_,1)
bpy.ops.object.shade_smooth()

bpy.context.object.active_material = bpy.data.materials['Material']
mat = bpy.context.object.active_material
mix = mat.node_tree.nodes.new('ShaderNodeMixShader')
geo = mat.node_tree.nodes.new('ShaderNodeNewGeometry')

mat.blend_method = 'HASHED'

image_node = mat.node_tree.nodes.new('ShaderNodeTexImage')
gh_node = mat.node_tree.nodes.new('ShaderNodeTexImage')
output = mat.node_tree.nodes['Material Output']

gh_mix = mat.node_tree.nodes.new('ShaderNodeMixShader')
transparent = mat.node_tree.nodes.new('ShaderNodeBsdfTransparent')

mat.node_tree.links.new(geo.outputs['Backfacing'], mix.inputs['Fac'])
mat.node_tree.links.new(mix.outputs['Shader'], output.inputs['Surface'])
mat.node_tree.links.new(image_node.outputs['Color'], mix.inputs[1])

#mat.node_tree.links.new(gh_node.outputs['Color'], mix.inputs[2])
mat.node_tree.links.new(gh_node.outputs['Color'], gh_mix.inputs[2])
mat.node_tree.links.new(gh_node.outputs['Alpha'], gh_mix.inputs['Fac'])
mat.node_tree.links.new(transparent.outputs['BSDF'], gh_mix.inputs[1])
mat.node_tree.links.new(gh_mix.outputs['Shader'], mix.inputs[2])

image_node.image = bpy.data.images.load('${frontImage_}')
image_node.interpolation = 'Closest'

gh_node.image = bpy.data.images.load('${backImage_}')
gh_node.interpolation = 'Closest'


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
bpy.ops.object.origin_clear()
bpy.ops.object.origin_set(type='GEOMETRY_ORIGIN')

plane.rotation_euler = (0, $rotY_, 0)

scn = bpy.context.scene

#scn.render.engine = 'CYCLES'
#scn.render.resolution_percentage = 10

scn.view_settings.view_transform = 'Standard'


scn.render.resolution_x = 2560
scn.render.resolution_y = 1440

scn.render.film_transparent = True

bpy.ops.render.render( write_still=True )
|]
