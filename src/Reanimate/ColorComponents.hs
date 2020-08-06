{-# LANGUAGE RecordWildCards #-}
{- |
  Colors are three dimensional and can be projected into many color spaces
  with different properties.

  Interpolating directly in the RGB color space is unintuitive and rarely useful.
  If you want to transition through color, you most likely want either the XYZ space
  (for physically accurate color transitions) or the LAB space (for esthetically
  pleasing colors).
-}
module Reanimate.ColorComponents
  ( ColorComponents(..)
  , rgbComponents
  , hsvComponents
  , labComponents
  , xyzComponents
  , lchComponents
  , interpolate
  , interpolateRGB8
  , interpolateRGBA8
  , toRGB8
  , fromRGB8
  ) where

import           Codec.Picture
import           Codec.Picture.Types
import           Data.Colour
import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant (d65)
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.SRGB
import           Data.Fixed
import           Reanimate.Ease

-- | Constructor and destructor for color's three components.
data ColorComponents = ColorComponents
  { colorUnpack :: Colour Double -> (Double, Double, Double)
    -- ^ Unpack a color into its three components.
  , colorPack   :: Double -> Double -> Double -> Colour Double
    -- ^ Restore a color from three coordinates.
  }

-- | > interpolate rgbComponents yellow blue
--
--   <<docs/gifs/doc_rgbComponents.gif>>
rgbComponents :: ColorComponents
rgbComponents = ColorComponents rgbUnpack sRGB
  where
    rgbUnpack :: Colour Double -> (Double, Double, Double)
    rgbUnpack c =
      case toSRGB c of
        RGB r g b -> (r,g,b)

-- | > interpolate hsvComponents yellow blue
--
--   <<docs/gifs/doc_hsvComponents.gif>>
hsvComponents :: ColorComponents
hsvComponents = ColorComponents unpack pack
  where
    unpack = hsvView.toSRGB
    pack a b c = uncurryRGB sRGB $ hsv a b c

-- | > interpolate labComponents yellow blue
--
--   <<docs/gifs/doc_labComponents.gif>>
labComponents :: ColorComponents
labComponents = ColorComponents unpack pack
  where
    unpack = cieLABView d65
    pack = cieLAB d65

-- | > interpolate xyzComponents yellow blue
--
--   <<docs/gifs/doc_xyzComponents.gif>>
xyzComponents :: ColorComponents
xyzComponents = ColorComponents cieXYZView cieXYZ

-- | > interpolate lchComponents yellow blue
--
--   <<docs/gifs/doc_lchComponents.gif>>
lchComponents :: ColorComponents
lchComponents = ColorComponents unpack pack
  where
    toDeg,toRad :: Double -> Double
    toRad deg = deg/180 * pi
    toDeg rad = rad/pi * 180
    unpack :: Colour Double -> (Double, Double, Double)
    unpack color =
      let (l,a,b) = cieLABView d65 color
          c = sqrt (a*a + b*b)
          h :: Double
          h = (toDeg(atan2 b a) + 360) `mod'` 360
          isZero = round (c*10000) == (0::Integer)
      in (l, c, if isZero then 0/0 else h)
    pack l c h =
      cieLAB d65 l (cos (toRad h) * c) (sin (toRad h) * c)

-- | Smoothly interpolate between two colors using the given color components.
interpolate :: ColorComponents -> Colour Double -> Colour Double -> (Double -> Colour Double)
interpolate ColorComponents{..} from to = \d ->
    colorPack (a1 + (a2-a1)*d) (b1 + (b2-b1)*d) (c1 + (c2-c1)*d)
  where
    (a1,b1,c1) = colorUnpack from
    (a2,b2,c2) = colorUnpack to

-- | Convenience interpolation function for RGB8 values.
interpolateRGB8 :: ColorComponents -> PixelRGB8 -> PixelRGB8 -> (Double -> PixelRGB8)
interpolateRGB8 comps from to = toRGB8 . interpolate comps (fromRGB8 from) (fromRGB8 to)

-- | Convenience interpolation function for RGBA8 values.
interpolateRGBA8 :: ColorComponents -> PixelRGBA8 -> PixelRGBA8 -> (Double -> PixelRGBA8)
interpolateRGBA8 comps from to = \t ->
  case interp t of
    PixelRGB8 r g b ->
      let alpha = fromToS (fromIntegral $ pixelOpacity from) (fromIntegral $ pixelOpacity to) t
      in PixelRGBA8 r g b (round alpha)
  where
    interp = interpolateRGB8 comps (dropTransparency from) (dropTransparency to)

-- | Convenience function for expressing a color as an RGB8 value.
toRGB8 :: Colour Double -> PixelRGB8
toRGB8 c = PixelRGB8 r g b
  where
    RGB r g b = toSRGBBounded c

-- | Convenience function for expressing an RGB8 value as a color.
fromRGB8 :: PixelRGB8 -> Colour Double
fromRGB8 (PixelRGB8 r g b) = sRGB24 r g b
