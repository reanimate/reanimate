#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import qualified Data.Text         as T
import           NeatInterpolation
import           Reanimate
import           Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ mkAnimation 5 $ \t -> mkGroup
  [ blender (script $ T.pack $ show $ t*pi)
  , withFillOpacity 1 $ withFillColor "black" $ withStrokeWidth 0 $
    translate 0 3 $ scale 2 $ center $
    latex "\\textbf{default cube}"
  ]
  where
    script s = [text|
    import bpy

    bpy.context.scene.render.film_transparent = True
    bpy.data.objects["Cube"].rotation_euler = (0,0,${s})

    bpy.ops.render.render(animation=False, write_still=True)
|]
