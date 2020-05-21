module Main where

import           Control.Lens                    ((&), (.~))

import           Codec.Picture
import           Control.Monad
import           Data.Colour
import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV (hsvView)
import           Data.Colour.SRGB
import           Data.Colour.SRGB.Linear
import           Data.List
import           Data.Maybe
import qualified Data.Map                        as Map
import           Data.Ord
import           Data.Text                       (Text)
import           Graphics.SvgTree                hiding (Text)
import           Linear.V2
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Builtin.CirclePlot
import           Reanimate.Builtin.Documentation
import qualified Reanimate.Builtin.TernaryPlot   as Ternary
import           Reanimate.ColorMap
import           Reanimate.ColorSpace
import           Reanimate.Constants
import           Reanimate.Driver                (reanimate)
import           Reanimate.Effect
import           Reanimate.Interpolate
import           Reanimate.LaTeX
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Ease
import           Reanimate.Svg
import           Reanimate.Svg.BoundingBox
import Control.Parallel.Strategies

labScaleX = 110 -- 100 -- 128
labScaleY = 110 -- 100 -- 128


main :: IO ()
main = writePng "lab.png" (cieLABImage_' 1000)

cieLABImage_' dim = generateImage gen dim dim
  where
    gen x y =
      let
          aStar = (fromIntegral x / fromIntegral dim) * labScaleX*2 - labScaleX
          bStar = (1-(fromIntegral y / fromIntegral dim)) * labScaleY*2 - labScaleY
          -- lStar = 50 -- findLStar aStar bStar
          colors = {-withStrategy (parList rpar)-} [ toSRGBBounded color
                   | lStar <- reverse [0, 0.1 .. 100]
                   , let color = cieLAB d65 lStar aStar bStar
                   , inGamut sRGBGamut color ]
      in case listToMaybe colors of
           Nothing -> PixelRGBA8 0xFF 0xFF 0xFF 0x00
           Just (RGB r g b) -> PixelRGBA8 r g b 0xFF
