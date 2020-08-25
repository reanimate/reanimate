{-|
Module      : Reanimate.Builtin.Documentation
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

This module contains convenience functions used in documention
GIFs for a consistent look and feel.

-}
module Reanimate.Builtin.Documentation where

import Reanimate.Animation
import Reanimate.Svg
import Reanimate.Raster
import Reanimate.Constants
import Codec.Picture

-- | Default environment for API documentation GIFs.
docEnv :: Animation -> Animation
docEnv = mapA $ \svg -> mkGroup
  [ mkBackground "white"
  , withFillOpacity 0 $
    withStrokeWidth 0.1 $
    withStrokeColor "black" (mkGroup [svg]) ]

-- | <<docs/gifs/doc_drawBox.gif>>
drawBox :: Animation
drawBox = mkAnimation 2 $ \t ->
  partialSvg t $ pathify $
  mkRect (screenWidth/2) (screenHeight/2)

-- | <<docs/gifs/doc_drawCircle.gif>>
drawCircle :: Animation
drawCircle = mkAnimation 2 $ \t ->
  partialSvg t $ pathify $
  mkCircle (screenHeight/3)

-- | <<docs/gifs/doc_drawProgress.gif>>
drawProgress :: Animation
drawProgress = mkAnimation 2 $ \t ->
  mkGroup
  [ mkLine (-screenWidth/2*widthP,0)
           (screenWidth/2*widthP,0)
  , translate (-screenWidth/2*widthP + screenWidth*widthP*t) 0 $
    withFillOpacity 1 $ mkCircle 0.5 ]
  where
    widthP = 0.8

-- | Render a full-screen view of a color-map.
showColorMap :: (Double -> PixelRGB8) -> SVG
showColorMap f = center $ scaleToSize screenWidth screenHeight $ embedImage img
  where
    width = 256
    height = 1
    img = generateImage pixelRenderer width height
    pixelRenderer x _y = f (fromIntegral x / fromIntegral (width-1))

-- | Default background color for videos on reanimate.rtfd.io
rtfdBackgroundColor :: PixelRGBA8
rtfdBackgroundColor = PixelRGBA8 252 252 252 0xFF
