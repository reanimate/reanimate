{-|
Module      : Reanimate.Raster
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

Tools for generating, manipulating, and embedding raster images.

-}
module Reanimate.Raster
  ( mkImage           -- :: Double -> Double -> FilePath -> SVG
  , cacheImage        -- :: (PngSavable pixel, Hashable a) => a -> Image pixel -> FilePath
  , prerenderSvg      -- :: Hashable a => a -> SVG -> SVG
  , prerenderSvgFile  -- :: Hashable a => a -> Width -> Height -> SVG -> FilePath
  , embedImage        -- :: PngSavable a => Image a -> SVG
  , embedDynamicImage -- :: DynamicImage -> SVG
  , embedPng          -- :: Double -> Double -> LBS.ByteString -> SVG
  , raster            -- :: SVG -> DynamicImage
  , rasterSized       -- :: Width -> Height -> SVG -> DynamicImage
  , vectorize         -- :: FilePath -> SVG
  , vectorize_        -- :: [String] -> FilePath -> SVG
  , svgAsPngFile      -- :: SVG -> FilePath
  , svgAsPngFile'     -- :: Width -> Height -> SVG -> FilePath
  )
where

import           Codec.Picture
import           Control.Lens                             ( (&)
                                                          , (.~)
                                                          )
import           Control.Monad
import qualified Data.ByteString               as B
import qualified Data.ByteString.Base64.Lazy   as Base64
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Hashable
import qualified Data.Text                     as T
import           Graphics.SvgTree                         ( Number(..)
                                                          , defaultSvg
                                                          , parseSvgFile
                                                          )
import qualified Graphics.SvgTree              as Svg
import           Reanimate.Animation
import           Reanimate.Cache
import           Reanimate.Driver.Magick
import           Reanimate.Misc
import           Reanimate.Render
import           Reanimate.Parameters
import           Reanimate.Constants
import           Reanimate.Svg.Constructors
import           Reanimate.Svg.Unuse
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.IO.Unsafe

-- | Load an external image. Width and height must be specified,
--   ignoring the image's aspect ratio. The center of the image is
--   placed at position (0,0).
--
--   For security reasons, must SVG renderer do not allow arbitrary
--   image links. For some renderers, we can get around this by placing
--   the images in the same root directory as the parent SVG file. Other
--   renderers (like Chrome and ffmpeg) requires that the image is inlined
--   as base64 data. External SVG files are an exception, though, as must
--   always be inlined directly. `mkImage` attempts to hide all the complexity
--   but edge-cases may exist.
--
--   Example:
--
-- @
-- 'mkImage' 'screenWidth' 'screenHeight' \"..\/data\/haskell.svg\"
-- @
--
--   <<docs/gifs/doc_mkImage.gif>>
mkImage
  :: Double -- ^ Desired image width.
  -> Double -- ^ Desired image height.
  -> FilePath -- ^ Path to external image file.
  -> SVG
mkImage width height path | takeExtension path == ".svg" = unsafePerformIO $ do
  svg_data <- B.readFile path
  case parseSvgFile path svg_data of
    Nothing -> error "Malformed svg"
    Just svg ->
      return
        $ scaleXY (width / screenWidth) (height / screenHeight)
        $ embedDocument svg
mkImage width height path | pRaster == RasterNone = unsafePerformIO $ do
  inp <- LBS.readFile path
  let imgData = LBS.unpack $ Base64.encode inp
  return
    $  flipYAxis
    $  Svg.imageTree
    $  defaultSvg
    &  Svg.imageWidth
    .~ Svg.Num width
    &  Svg.imageHeight
    .~ Svg.Num height
    &  Svg.imageHref
    .~ ("data:" ++ mimeType ++ ";base64," ++ imgData)
    &  Svg.imageCornerUpperLeft
    .~ (Svg.Num (-width / 2), Svg.Num (-height / 2))
    &  Svg.imageAspectRatio
    .~ Svg.PreserveAspectRatio False Svg.AlignNone Nothing
 where
    -- FIXME: Is there a better way to do this?
  mimeType = case takeExtension path of
    ".jpg" -> "image/jpeg"
    ext    -> "image/" ++ drop 1 ext
mkImage width height path = unsafePerformIO $ do
  exists <- doesFileExist target
  unless exists $ copyFile path target
  return
    $  flipYAxis
    $  Svg.imageTree
    $  defaultSvg
    &  Svg.imageWidth
    .~ Svg.Num width
    &  Svg.imageHeight
    .~ Svg.Num height
    &  Svg.imageHref
    .~ ("file://" ++ target)
    &  Svg.imageCornerUpperLeft
    .~ (Svg.Num (-width / 2), Svg.Num (-height / 2))
    &  Svg.imageAspectRatio
    .~ Svg.PreserveAspectRatio False Svg.AlignNone Nothing
 where
  target   = pRootDirectory </> encodeInt hashPath <.> takeExtension path
  hashPath = hash path

-- | Write in-memory image to cache file if (and only if) such cache file doesn't
--   already exist.
cacheImage :: (PngSavable pixel, Hashable a) => a -> Image pixel -> FilePath
cacheImage key gen = unsafePerformIO $ cacheFile template $ \path ->
  writePng path gen
  where template = encodeInt (hash key) <.> "png"

-- Warning: Caching svg elements with links to external objects does
--          not work. 2020-06-01
-- | Same as 'prerenderSvg' but returns the location of the rendered image
--   as a FilePath.
prerenderSvgFile :: Hashable a => a -> Width -> Height -> SVG -> FilePath
prerenderSvgFile key width height svg =
  unsafePerformIO $ cacheFile template $ \path -> do
    let svgPath = replaceExtension path "svg"
    writeFile svgPath rendered
    engine <- requireRaster pRaster
    applyRaster engine svgPath
 where
  template = encodeInt (hash (key, width, height)) <.> "png"
  rendered = renderSvg (Just $ Px $ fromIntegral width)
                       (Just $ Px $ fromIntegral height)
                       svg

-- | Render SVG node to a PNG file and return a new node containing
--   that image. For static SVG nodes, this can hugely improve performance.
--   The first argument is the key that determines SVG uniqueness. It
--   is entirely your responsibility to ensure that all keys are unique.
--   If they are not, you will be served stale results from the cache.
prerenderSvg :: Hashable a => a -> SVG -> SVG
prerenderSvg key =
  mkImage screenWidth screenHeight . prerenderSvgFile key pWidth pHeight


{-# INLINE embedImage #-}
-- | Embed an in-memory PNG image. Note, the pixel size of the image
--   is used as the dimensions. As such, embedding a 100x100 PNG will
--   result in an image 100 units wide and 100 units high. Consider
--   using with 'scaleToSize'.
embedImage :: PngSavable a => Image a -> SVG
embedImage img = embedPng width height (encodePng img)
 where
  width  = fromIntegral $ imageWidth img
  height = fromIntegral $ imageHeight img

-- | Embed in-memory PNG bytestring without parsing it.
embedPng
  :: Double -- ^ Width
  -> Double -- ^ Height
  -> LBS.ByteString -- ^ Raw PNG data
  -> SVG
-- embedPng w h png = unsafePerformIO $ do
--     LBS.writeFile path png
--     return $ ImageTree $ defaultSvg
--       & Svg.imageCornerUpperLeft .~ (Svg.Num (-w/2), Svg.Num (-h/2))
--       & Svg.imageWidth .~ Svg.Num w
--       & Svg.imageHeight .~ Svg.Num h
--       & Svg.imageHref .~ ("file://"++path)
--   where
--     path = "/tmp" </> show (hash png) <.> "png"
embedPng w h png =
  flipYAxis
    $  Svg.imageTree
    $  defaultSvg
    &  Svg.imageCornerUpperLeft
    .~ (Svg.Num (-w / 2), Svg.Num (-h / 2))
    &  Svg.imageWidth
    .~ Svg.Num w
    &  Svg.imageHeight
    .~ Svg.Num h
    &  Svg.imageHref
    .~ ("data:image/png;base64," ++ imgData)
  where imgData = LBS.unpack $ Base64.encode png


{-# INLINE embedDynamicImage #-}
-- | Embed an in-memory image. Note, the pixel size of the image
--   is used as the dimensions. As such, embedding a 100x100 image will
--   result in an image 100 units wide and 100 units high. Consider
--   using with 'scaleToSize'.
embedDynamicImage :: DynamicImage -> SVG
embedDynamicImage img = embedPng width height imgData
 where
  width   = fromIntegral $ dynamicMap imageWidth img
  height  = fromIntegral $ dynamicMap imageHeight img
  imgData = case encodeDynamicPng img of
    Left  err -> error err
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


-- | Convert an SVG object to a pixel-based image. The default resolution
--   is 2560x1440. See also 'rasterSized'. Multiple raster engines are supported
--   and are selected using the '--raster' flag in the driver.
raster :: SVG -> DynamicImage
raster = rasterSized 2560 1440

-- | Convert an SVG object to a pixel-based image.
rasterSized
  :: Width  -- ^ X resolution in pixels
  -> Height -- ^ Y resolution in pixels
  -> SVG    -- ^ SVG object
  -> DynamicImage
rasterSized w h svg = unsafePerformIO $ do
  png <- B.readFile (svgAsPngFile' w h svg)
  case decodePng png of
    Left{}    -> error "bad image"
    Right img -> return img

-- | Use \'potrace\' to trace edges in a raster image and convert them to SVG polygons.
vectorize :: FilePath -> SVG
vectorize = vectorize_ []

-- | Same as 'vectorize' but takes a list of arguments for \'potrace\'.
vectorize_ :: [String] -> FilePath -> SVG
vectorize_ _ path | pNoExternals = mkText $ T.pack path
vectorize_ args path             = unsafePerformIO $ do
  root <- getXdgDirectory XdgCache "reanimate"
  createDirectoryIfMissing True root
  let svgPath = root </> encodeInt key <.> "svg"
  hit <- doesFileExist svgPath
  unless hit $ withSystemTempFile "file.svg" $ \tmpSvgPath svgH ->
    withSystemTempFile "file.bmp" $ \tmpBmpPath bmpH -> do
      hClose svgH
      hClose bmpH
      potrace <- requireExecutable "potrace"
      magick <- requireExecutable magickCmd
      runCmd magick [path, "-flatten", tmpBmpPath]
      runCmd potrace (args ++ ["--svg", "--output", tmpSvgPath, tmpBmpPath])
      renameOrCopyFile tmpSvgPath svgPath
  svg_data <- B.readFile svgPath
  case parseSvgFile svgPath svg_data of
    Nothing -> do
      removeFile svgPath
      error "Malformed svg"
    Just svg -> return $ unbox $ replaceUses svg
  where key = hash (path, args)

-- imageAsFile :: DynamicImage -> FilePath
-- imageAsFile img

-- | Convert an SVG object to a pixel-based image and save it to disk, returning
--   the filepath. The default resolution is 2560x1440. See also 'svgAsPngFile''.
--   Multiple raster engines are supported and are selected using the '--raster'
--   flag in the driver.
svgAsPngFile :: SVG -> FilePath
svgAsPngFile = svgAsPngFile' width height
 where
  width  = 2560
  height = width * 9 `div` 16

-- | Convert an SVG object to a pixel-based image and save it to disk, returning
--   the filepath.
svgAsPngFile'
  :: Width  -- ^ Width
  -> Height -- ^ Height
  -> SVG    -- ^ SVG object
  -> FilePath
svgAsPngFile' _ _ _ | pNoExternals = "/svgAsPngFile/has/been/disabled"
svgAsPngFile' width height svg =
  unsafePerformIO $ cacheFile template $ \pngPath -> do
    let svgPath = replaceExtension pngPath "svg"
    writeFile svgPath rendered
    engine <- requireRaster pRaster
    applyRaster engine svgPath
 where
  template = encodeInt (hash rendered) <.> "png"
  rendered = renderSvg (Just $ Px $ fromIntegral width)
                       (Just $ Px $ fromIntegral height)
                       svg
