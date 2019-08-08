module Reanimate.Raster
  ( embedImage
  ) where

import Control.Lens
import Codec.Picture
import Codec.Picture.Png
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy.Char8 as LBS
import Graphics.SvgTree (Tree(..), defaultSvg)
import qualified Graphics.SvgTree as Svg

-- XXX: Use Px instead of Num for width and height?
{-# INLINE embedImage #-}
embedImage :: PngSavable a => Image a -> Tree
embedImage img =
  ImageTree $ defaultSvg
    & Svg.imageWidth .~ Svg.Num (fromIntegral $ imageWidth img)
    & Svg.imageHeight .~ Svg.Num (fromIntegral $ imageHeight img)
    & Svg.imageHref .~ ("data:image/png;base64," ++ imgData)
  where
    imgData = LBS.unpack $ Base64.encode (encodePng img)

