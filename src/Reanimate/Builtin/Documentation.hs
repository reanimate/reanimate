module Reanimate.Builtin.Documentation where

import Reanimate.Animation
import Reanimate.Svg
import Reanimate.Constants

docEnv :: Animation -> Animation
docEnv = mapA $ \svg -> mkGroup
  [ mkBackground "white"
  , withFillOpacity 0 $
    withStrokeWidth 0.1 $
    withStrokeColor "black" svg ]

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
