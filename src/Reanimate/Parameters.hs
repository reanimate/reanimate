{- |
  Parameters define the global context of an animation. They are set once
  before an animation is rendered and may not change during rendering.
-}
module Reanimate.Parameters
  ( Raster(..)
  , Width
  , Height
  , FPS
  , pRaster
  , pFPS
  , pWidth
  , pHeight
  , pNoExternals
  , pRootDirectory
  , setRaster
  , setFPS
  , setWidth
  , setHeight
  , setNoExternals
  , setRootDirectory
  ) where

import System.IO.Unsafe
import Data.IORef

-- | Width of animation in pixels.
type Width = Int
-- | Height of animation in pixels.
type Height = Int
-- | Framerate of animation in frames per second.
type FPS = Int

-- | Raster engines turn SVG images into pixels.
data Raster
  = RasterNone     -- ^ Do not use any external raster engine. Rely on the browser or ffmpeg.
  | RasterAuto     -- ^ Scan for installed raster engines and pick the fastest one.
  | RasterInkscape -- ^ Use Inkscape to raster SVG images.
  | RasterRSvg     -- ^ Use rsvg-convert to raster SVG images.
  | RasterMagick   -- ^ Use imagemagick to raster SVG images.
  deriving (Show, Eq)

{-# NOINLINE pRasterRef #-}
pRasterRef :: IORef Raster
pRasterRef = unsafePerformIO (newIORef RasterNone)

{-# NOINLINE pRaster #-}
-- | Selected raster engine.
pRaster :: Raster
pRaster = unsafePerformIO (readIORef pRasterRef)

-- | Set raster engine.
setRaster :: Raster -> IO ()
setRaster = writeIORef pRasterRef

{-# NOINLINE pFPSRef #-}
pFPSRef :: IORef FPS
pFPSRef = unsafePerformIO (newIORef 0)

{-# NOINLINE pFPS #-}
-- | Selected framerate.
pFPS :: FPS
pFPS = unsafePerformIO (readIORef pFPSRef)

-- | Set desired framerate.
setFPS :: FPS -> IO ()
setFPS = writeIORef pFPSRef

{-# NOINLINE pWidthRef #-}
pWidthRef :: IORef FPS
pWidthRef = unsafePerformIO (newIORef 0)

{-# NOINLINE pWidth #-}
-- | Width of animation in pixel.
pWidth :: Width
pWidth = unsafePerformIO (readIORef pWidthRef)

-- | Set desired width of animation in pixel.
setWidth :: Width -> IO ()
setWidth = writeIORef pWidthRef

{-# NOINLINE pHeightRef #-}
pHeightRef :: IORef FPS
pHeightRef = unsafePerformIO (newIORef 0)

{-# NOINLINE pHeight #-}
-- | Height of animation in pixel.
pHeight :: Height
pHeight = unsafePerformIO (readIORef pHeightRef)

-- | Set desired height of animation in pixel.
setHeight :: Height -> IO ()
setHeight = writeIORef pHeightRef

{-# NOINLINE pNoExternalsRef #-}
pNoExternalsRef :: IORef Bool
pNoExternalsRef = unsafePerformIO (newIORef False)

{-# NOINLINE pNoExternals #-}
-- | This parameter determined whether or not external tools are allowed.
--   If this flag is True then tools such as 'Reanimate.LaTeX.latex' and
--   'Reanimate.Blender.blender' will not be invoked.
pNoExternals :: Bool
pNoExternals = unsafePerformIO (readIORef pNoExternalsRef)

-- | Set whether external tools are allowed.
setNoExternals :: Bool -> IO ()
setNoExternals = writeIORef pNoExternalsRef

{-# NOINLINE pRootDirectoryRef #-}
pRootDirectoryRef :: IORef FilePath
pRootDirectoryRef = unsafePerformIO (newIORef (error "root directory not set"))

{-# NOINLINE pRootDirectory #-}
-- | Root directory of animation. Images and other data has to be placed
--   here if they are referenced in an SVG image.
pRootDirectory :: FilePath
pRootDirectory = unsafePerformIO (readIORef pRootDirectoryRef)

-- | Set the root animation directory.
setRootDirectory :: FilePath -> IO ()
setRootDirectory = writeIORef pRootDirectoryRef
