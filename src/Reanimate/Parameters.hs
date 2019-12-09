module Reanimate.Parameters
  ( pFPS
  , pWidth
  , pHeight
  , pNoExternals
  , setFPS
  , setWidth
  , setHeight
  , setNoExternals
  ) where

import System.IO.Unsafe
import Data.IORef
import Reanimate.Render

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

