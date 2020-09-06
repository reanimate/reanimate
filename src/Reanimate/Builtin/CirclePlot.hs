{-|
Module      : Reanimate.Builtin.CirclePlot
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

Convenience module for rendering circle plots.

-}
module Reanimate.Builtin.CirclePlot where

import           Codec.Picture
import           Graphics.SvgTree (Tree)
import           Reanimate.Raster
import           Reanimate.Svg
import           Reanimate.Constants

-- | Circle plots are scaled to 'screenHeight'.
--
--   Example:
--
-- @
-- 'circlePlot' 500 $ \\ang r ->
--   'Codec.Picture.Types.promotePixel' $ toRGB8 $ uncurryRGB sRGB $ hsv (ang/pi*180) r 1
-- @
--
--   <<docs/gifs/doc_circlePlot.gif>>
circlePlot :: Int -- ^ Number of diagonal pixels. Only affects quality, not size.
           -> (Double -> Double -> PixelRGBA8)
              -- ^ Angle and radius in radians and percent respectively.
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
