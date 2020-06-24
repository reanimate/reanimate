#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate hiding (raster, hsv)
import Reanimate.Builtin.Documentation
import Reanimate.Builtin.CirclePlot
import Reanimate.ColorComponents
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Codec.Picture.Types

main :: IO ()
main = reanimate $ docEnv $ animate $ const $ circlePlot 500 $ \ang r ->
  promotePixel $ toRGB8 $ uncurryRGB sRGB $ hsv (ang/pi*180) r 1
