module Reanimate.Builtin.CirclePlot where

import           Codec.Picture
import           Graphics.SvgTree (Tree)
import           Reanimate.Raster
import           Reanimate.Svg
import           Reanimate.Constants

circlePlot :: Int -- ^ Pixels in the X-axis.
           -> (Double -> Double -> PixelRGBA8)
              -- ^ Angle and radius in radians percent respectively.
           -> Tree
circlePlot density fn =
    scaleToHeight screenHeight $ flipYAxis $
    embedImage $ generateImage gen density density
  where
    cN = fromIntegral $ density `div` 2 - 1
    gen x y =
      let radius = sqrt ((fromIntegral x-cN)**2 + (fromIntegral y-cN)**2)
          ang = atan2 (fromIntegral y-cN) (fromIntegral x-cN)
      in if radius > cN
        then PixelRGBA8 0 0 0 0
        else fn ang (radius / cN)
