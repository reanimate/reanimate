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
  let s = t * pi in
  mkGroup
  [ mkBackground "black"
  , blender (script $ T.pack $ show s)
  , withFillColor "white" $
    translate 0 2 $
    mkText "default cube"
  ]
  where
    script s = [text|
import bpy

if __name__ == "__main__":
    # Args
    resolution_percentage = 40

    # Setting
    default_scene = bpy.context.scene
    default_scene.render.resolution_percentage = resolution_percentage
    default_scene.render.film_transparent = True

    default_cube = bpy.data.objects["Cube"]
    default_cube.rotation_euler = (0,0,${s})

    # Rendering
    bpy.ops.render.render(animation=False, write_still=True)
|]
