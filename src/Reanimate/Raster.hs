module Reanimate.Raster
  ( embedImage
  , embedDynamicImage
  , embedPng
  , raster
  , svgAsPngFile
  ) where

import           Codec.Picture.Types
import           Codec.Picture
import           Control.Lens ((.~),(&))
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Graphics.SvgTree            (Number (..), Tree (..),
                                              defaultSvg)
import qualified Graphics.SvgTree            as Svg
import           Reanimate.Misc
import           Reanimate.Animation
import           Reanimate.Svg.Constructors
import           System.FilePath
import           System.Directory
import           Data.Hashable
import           System.IO.Unsafe


{-# INLINE embedImage #-}
embedImage :: PngSavable a => Image a -> Tree
embedImage img = embedPng width height (encodePng img)
  where
    width  = fromIntegral $ imageWidth img
    height = fromIntegral $ imageHeight img

embedPng :: Double -> Double -> LBS.ByteString -> Tree
embedPng w h png = flipYAxis $
  ImageTree $ defaultSvg
    & Svg.imageCornerUpperLeft .~ (Svg.Num (-w/2), Svg.Num (-h/2))
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
        Left err  -> error err
        Right dat -> dat

raster :: Tree -> DynamicImage
raster svg = unsafePerformIO $ do
    png <- B.readFile (svgAsPngFile svg)
    case decodePng png of
      Left{}    -> error "bad image"
      Right img -> return img

-- imageAsFile :: DynamicImage -> FilePath
-- imageAsFile img

svgAsPngFile :: Tree -> FilePath
svgAsPngFile svg = unsafePerformIO $ do
    root <- getXdgDirectory XdgCache "reanimate"
    createDirectoryIfMissing True root
    let svgPath = root </> show (hash rendered) <.> "svg"
        pngPath = replaceExtension svgPath "png"
    hit <- doesFileExist pngPath
    if hit
      then return pngPath
      else do
        -- ffmpeg <- requireExecutable "ffmpeg"
        -- convert <- requireExecutable "convert"
        inkscape <- requireExecutable "inkscape"
        writeFile svgPath rendered
        -- runCmd convert [ "-background", "none", "-antialias", svgPath, pngPath ]
        runCmd inkscape [ svgPath, "--export-png=" ++ pngPath, "--without-gui" ]
        return pngPath
  where
    rendered = renderSvg (Just $ Num width) (Just $ Num height) svg
    width = 2560
    height = width * 9 / 16
