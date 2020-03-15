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

type Width = Int
type Height = Int
type FPS = Int

data Raster
  = RasterNone
  | RasterAuto
  | RasterInkscape
  | RasterRSvg
  | RasterConvert
  deriving (Show)

{-# NOINLINE pRasterRef #-}
pRasterRef :: IORef Raster
pRasterRef = unsafePerformIO (newIORef RasterNone)

{-# NOINLINE pRaster #-}
pRaster :: Raster
pRaster = unsafePerformIO (readIORef pRasterRef)

setRaster :: Raster -> IO ()
setRaster = writeIORef pRasterRef

{-# NOINLINE pFPSRef #-}
pFPSRef :: IORef FPS
pFPSRef = unsafePerformIO (newIORef 0)

{-# NOINLINE pFPS #-}
pFPS :: FPS
pFPS = unsafePerformIO (readIORef pFPSRef)

setFPS :: FPS -> IO ()
setFPS = writeIORef pFPSRef

{-# NOINLINE pWidthRef #-}
pWidthRef :: IORef FPS
pWidthRef = unsafePerformIO (newIORef 0)

{-# NOINLINE pWidth #-}
pWidth :: Width
pWidth = unsafePerformIO (readIORef pWidthRef)

setWidth :: Width -> IO ()
setWidth = writeIORef pWidthRef


{-# NOINLINE pHeightRef #-}
pHeightRef :: IORef FPS
pHeightRef = unsafePerformIO (newIORef 0)

{-# NOINLINE pHeight #-}
pHeight :: Height
pHeight = unsafePerformIO (readIORef pHeightRef)

setHeight :: Height -> IO ()
setHeight = writeIORef pHeightRef

{-# NOINLINE pNoExternalsRef #-}
pNoExternalsRef :: IORef Bool
pNoExternalsRef = unsafePerformIO (newIORef False)

{-# NOINLINE pNoExternals #-}
pNoExternals :: Bool
pNoExternals = unsafePerformIO (readIORef pNoExternalsRef)

setNoExternals :: Bool -> IO ()
setNoExternals = writeIORef pNoExternalsRef

{-# NOINLINE pRootDirectoryRef #-}
pRootDirectoryRef :: IORef FilePath
pRootDirectoryRef = unsafePerformIO (newIORef (error "root directory not set"))

{-# NOINLINE pRootDirectory #-}
pRootDirectory :: FilePath
pRootDirectory = unsafePerformIO (readIORef pRootDirectoryRef)

setRootDirectory :: FilePath -> IO ()
setRootDirectory = writeIORef pRootDirectoryRef
