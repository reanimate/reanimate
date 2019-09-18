module Reanimate.Raster
  ( embedImage
  , embedDynamicImage
  ) where

import           Codec.Picture
import           Codec.Picture.Types         (dynamicMap)
import           Control.Lens
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Graphics.SvgTree            (Tree (..), defaultSvg)
import qualified Graphics.SvgTree            as Svg
import           Reanimate.Svg

{-# INLINE embedImage #-}
embedImage :: PngSavable a => Image a -> Tree
embedImage img = center $ flipYAxis $
  ImageTree $ defaultSvg
    & Svg.imageWidth .~ Svg.Num (fromIntegral $ imageWidth img)
    & Svg.imageHeight .~ Svg.Num (fromIntegral $ imageHeight img)
    & Svg.imageHref .~ ("data:image/png;base64," ++ imgData)
  where
    imgData = LBS.unpack $ Base64.encode (encodePng img)

{-# INLINE embedDynamicImage #-}
embedDynamicImage :: DynamicImage -> Tree
embedDynamicImage img = center $ flipYAxis $
  ImageTree $ defaultSvg
    & Svg.imageWidth .~ Svg.Num (fromIntegral $ dynamicMap imageWidth img)
    & Svg.imageHeight .~ Svg.Num (fromIntegral $ dynamicMap imageHeight img)
    & Svg.imageHref .~ ("data:image/png;base64," ++ imgData)
  where
    imgData =
      case encodeDynamicPng img of
        Left err  -> error err
        Right dat -> LBS.unpack $ Base64.encode dat
