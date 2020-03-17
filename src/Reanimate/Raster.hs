module Reanimate.Raster
  ( mkImage
  , cacheImage
  , embedImage
  , embedDynamicImage
  , embedPng
  , raster
  , rasterSized
  , vectorize
  , vectorize_
  , svgAsPngFile
  , svgAsPngFile'
  ) where

import           Codec.Picture
import           Control.Lens                ((&), (.~))
import           Control.Monad
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Hashable
import qualified Data.Text                   as T
import           Graphics.SvgTree            (Number (..), Tree (..),
                                              defaultSvg, parseSvgFile)
import qualified Graphics.SvgTree            as Svg
import           Reanimate.Animation
import           Reanimate.Cache
import           Reanimate.Misc
import           Reanimate.Render
import           Reanimate.Parameters
import           Reanimate.Svg.Constructors
import           Reanimate.Svg.Unuse
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.IO.Unsafe

-- FIXME: Embed the image data as inline base64 iff no raster engine is specified.
mkImage :: Double -> Double -> FilePath -> SVG
mkImage width height path = unsafePerformIO $ do
    exists <- doesFileExist target
    unless exists $ copyFile path target
    return $ flipYAxis $ ImageTree $ defaultSvg
      & Svg.imageWidth .~ Svg.Num width
      & Svg.imageHeight .~ Svg.Num height
      & Svg.imageHref .~ ("file://"++target)
      & Svg.imageCornerUpperLeft .~ (Svg.Num (-width/2), Svg.Num (-height/2))
      & Svg.imageAspectRatio .~ Svg.PreserveAspectRatio False Svg.AlignNone Nothing
  where
    target = pRootDirectory </> show hashPath <.> takeExtension path
    hashPath = hash path

cacheImage :: (PngSavable pixel, Hashable a) => a -> Image pixel -> FilePath
cacheImage key gen = unsafePerformIO $ cacheFile template $ \path ->
    writePng path gen
  where
    template = show (hash key) <.> "png"

{-# INLINE embedImage #-}
embedImage :: PngSavable a => Image a -> Tree
embedImage img = embedPng width height (encodePng img)
  where
    width  = fromIntegral $ imageWidth img
    height = fromIntegral $ imageHeight img

embedPng :: Double -> Double -> LBS.ByteString -> Tree
-- embedPng w h png = unsafePerformIO $ do
--     LBS.writeFile path png
--     return $ ImageTree $ defaultSvg
--       & Svg.imageCornerUpperLeft .~ (Svg.Num (-w/2), Svg.Num (-h/2))
--       & Svg.imageWidth .~ Svg.Num w
--       & Svg.imageHeight .~ Svg.Num h
--       & Svg.imageHref .~ ("file://"++path)
--   where
--     path = "/tmp" </> show (hash png) <.> "png"
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

-- embedImageFile :: FilePath -> Tree
-- embedImageFile path = unsafePerformIO $ do
--     png <- B.readFile path
--     case decodePng png of
--       Left{}    -> error "bad image"
--       Right img -> return $
--         let width   = fromIntegral $ dynamicMap imageWidth img
--             height  = fromIntegral $ dynamicMap imageHeight img in
--         ImageTree $ defaultSvg
--           & Svg.imageCornerUpperLeft .~ (Svg.Num (-width/2), Svg.Num (-height/2))
--           & Svg.imageWidth .~ Svg.Num width
--           & Svg.imageHeight .~ Svg.Num height
--           & Svg.imageHref .~ ("file://" ++ path)



raster :: Tree -> DynamicImage
raster = rasterSized 2560 1440

rasterSized :: Int -> Int -> Tree -> DynamicImage
rasterSized w h svg = unsafePerformIO $ do
    png <- B.readFile (svgAsPngFile' w h svg)
    case decodePng png of
      Left{}    -> error "bad image"
      Right img -> return img

vectorize :: FilePath -> Tree
vectorize = vectorize_ []

vectorize_ :: [String] -> FilePath -> Tree
vectorize_ _ path | pNoExternals = mkText $ T.pack path
vectorize_ args path = unsafePerformIO $ do
    root <- getXdgDirectory XdgCache "reanimate"
    createDirectoryIfMissing True root
    let svgPath = root </> show key <.> "svg"
    hit <- doesFileExist svgPath
    unless hit $
      withSystemTempFile "file.svg" $ \tmpSvgPath svgH ->
      withSystemTempFile "file.bmp" $ \tmpBmpPath bmpH -> do
        hClose svgH
        hClose bmpH
        potrace <- requireExecutable "potrace"
        convert <- requireExecutable "convert"
        runCmd convert [ path, "-flatten", tmpBmpPath ]
        runCmd potrace (args ++ ["--svg", "--output", tmpSvgPath, tmpBmpPath])
        renameFile tmpSvgPath svgPath
    svg_data <- B.readFile svgPath
    case parseSvgFile svgPath svg_data of
      Nothing  -> do
        removeFile svgPath
        error "Malformed svg"
      Just svg -> return $ unbox $ replaceUses svg
  where
    key = hash (path, args)

-- imageAsFile :: DynamicImage -> FilePath
-- imageAsFile img

svgAsPngFile :: Tree -> FilePath
svgAsPngFile = svgAsPngFile' width height
  where
    width = 2560
    height = width * 9 `div` 16

svgAsPngFile' :: Int -> Int -> Tree -> FilePath
svgAsPngFile' _ _ _ | pNoExternals = "/svgAsPngFile/has/been/disabled"
svgAsPngFile' width height svg = unsafePerformIO $ cacheFile template $ \pngPath -> do
    let svgPath = replaceExtension pngPath "svg"
    -- ffmpeg <- requireExecutable "ffmpeg"
    -- convert <- requireExecutable "convert"
    -- inkscape <- requireExecutable "inkscape"
    writeFile svgPath rendered
    -- FIXME: raster should be configurable.
    applyRaster RasterRSvg svgPath
  where
    template = show (hash rendered) <.> "png"
    rendered = renderSvg (Just $ Px $ fromIntegral width) (Just $ Px $ fromIntegral height) svg
