{-|
Module      : Reanimate.Builtin.TernaryPlot
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

Implementation of ternary plots: <https://en.wikipedia.org/wiki/Ternary_plot>

-}
module Reanimate.Builtin.TernaryPlot
  ( ACoord
  , BCoord
  , CCoord
  , ternaryPlot
  -- , atCenter
  -- , radius
  , toCartesianCoords
  , toOffsetCartesianCoords
  , fromCartesianCoords
  ) where

import           Codec.Picture
import           Graphics.SvgTree (Tree)
import           Reanimate.Raster
import           Reanimate.Svg

-- a+b+c=1
-- | Left-most coordinate.
type ACoord = Double
-- | Top-most coordinate.
type BCoord = Double
-- | Right-most coordinate.
type CCoord = Double

-- | Creates a centered ternary plot with a width of 5.
--
--   Example:
--
-- @
-- 'ternaryPlot' 100 $ \\aCoord bCoord cCoord -> 'Codec.Picture.Types.promotePixel' $
--   let red   = round $ aCoord*255
--       green = round $ bCoord*255
--       blue  = round $ cCoord*255
--   in PixelRGB8 red green blue
-- @
--
--   <<docs/gifs/doc_ternaryPlot.gif>>
ternaryPlot :: Int -- ^ Pixels in the X-axis. More pixels => higher quality.
            -> (ACoord -> BCoord -> CCoord -> PixelRGBA8)
            -- ^ a+b+c=1. A=1 is the left-most position,
            --   B=1 is the top-most position, and
            --   C=1 is the right-most position.
            -> Tree
ternaryPlot density fn =
    scaleToWidth stdWidth $
    translate (-cX) (-cY) $
    scaleToWidth 1 $
    flipYAxis $
    translate (fromIntegral density/2) (-fromIntegral height/2) $
    embedImage $ generateImage gen density height
  where
    stdWidth = 5
    (cX, cY) = toCartesianCoords (1/3) (1/3)
    height = round (fromIntegral density * (sqrt 3 / 2) :: Double)
    gen x y =
      let
          x' = (fromIntegral x / fromIntegral density)
          y' = (fromIntegral y / fromIntegral density)
          aCoord = (x'*2-bCoord)/2
          bCoord = y' / (sqrt 3 / 2)
          cCoord = 1 - aCoord - bCoord
      in if aCoord + bCoord > 1 || aCoord < 0 || bCoord < 0 || cCoord < 0
        then PixelRGBA8 0 0 0 0
        else fn aCoord bCoord cCoord

-- atCenter :: Double -> Tree -> Tree
-- atCenter stdWidth = translate (-cX*stdWidth) (-cY*stdWidth)
--   where
--     (cX, cY) = toCartesianCoords (1/3) (1/3)

-- radius :: Double
-- radius = sqrt (cX*cX + cY*cY)
--   where
--     (cX, cY) = toCartesianCoords (1/3) (1/3)

-- | Compute the XY coordinates from ternary coordinates.
--   Note that @CCoord@ is given because @a+b+c=1@.
toCartesianCoords :: ACoord -> BCoord -> (Double, Double)
toCartesianCoords a b = (x, y)
  where
    x = (a+2*b)/2
    y = (sqrt 3 / 2) * a

-- | Compute the XY coordinates relative from the center of the
--   ternary plot.
--   Note that @CCoord@ is given because @a+b+c=1@.
toOffsetCartesianCoords :: ACoord -> BCoord -> (Double, Double)
toOffsetCartesianCoords a b =
    (tx-zx, ty-zy)
  where
    (zx,zy) = toCartesianCoords (1/3) (1/3)
    (tx,ty) = toCartesianCoords a b

-- | Compute ternary coordinates from XY coordinates.
fromCartesianCoords :: Double -> Double -> (ACoord, BCoord, CCoord)
fromCartesianCoords x y = (a,b,1-a-b)
  where
    a = (x*2-b)/2
    b = y / (sqrt 3 / 2)
