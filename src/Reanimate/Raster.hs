module Reanimate.Raster
  ( embedImage
  , embedDynamicImage
  , embedPng
  , raster
  ) where

import Control.Lens
import Codec.Picture
import Codec.Picture.Types (dynamicMap)
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy.Char8 as LBS
import Graphics.SvgTree (Tree(..), defaultSvg,Number(..))
import qualified Graphics.SvgTree as Svg
import           System.IO.Temp
import qualified Data.ByteString   as B
import Reanimate.Monad
import System.IO
import System.IO.Unsafe
import System.FilePath
import Reanimate.Misc


-- XXX: Use Px instead of Num for width and height?
{-# INLINE embedImage #-}
embedImage :: PngSavable a => Image a -> Tree
embedImage img = embedPng width height (encodePng img)
  where
    width  = fromIntegral $ imageWidth img
    height = fromIntegral $ imageHeight img

embedPng :: Double -> Double -> LBS.ByteString -> Tree
embedPng w h png =
  ImageTree $ defaultSvg
    & Svg.imageWidth .~ Svg.Num w
    & Svg.imageHeight .~ Svg.Num h
    & Svg.imageHref .~ ("data:image/png;base64," ++ imgData)
  where
    imgData = LBS.unpack $ Base64.encode png


{-# INLINE embedDynamicImage #-}
embedDynamicImage :: DynamicImage -> Tree
embedDynamicImage img = embedPng width height imgData
  where
    width   = fromIntegral $ dynamicMap imageWidth img
    height  = fromIntegral $ dynamicMap imageHeight img
    imgData =
      case encodeDynamicPng img of
        Left err -> error err
        Right dat -> dat

raster :: Tree -> DynamicImage
raster svg = unsafePerformIO $
    withSystemTempFile "reanimate.svg" $ \tmpFile handle -> do
      let target = replaceExtension tmpFile "png"
      -- ffmpeg <- requireExecutable "ffmpeg"
      convert <- requireExecutable "convert"
      hPutStr handle $ renderSvg (Just $ Num width) (Just $ Num height) svg
      hClose handle
      runCmd convert [ tmpFile, target ]
      png <- B.readFile target
      case decodePng png of
        Left{}    -> error "bad image"
        Right img -> return img
  where
    width = 2560
    height = width * 9 / 16
