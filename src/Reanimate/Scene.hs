-- |
-- Module      : Reanimate.Scene
-- Copyright   : Written by David Himmelstrup
-- License     : Unlicense
-- Maintainer  : lemmih@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Scenes are an imperative way of defining animations.
module Reanimate.Scene
  ( -- * Scenes
    Scene,
    ZIndex,
    scene, -- :: (forall s. Scene s a) -> Animation
    play, -- :: Animation -> Scene s ()
    fork, -- :: Scene s a -> Scene s a
    queryNow, -- :: Scene s Time
    wait, -- :: Duration -> Scene s ()
    waitUntil, -- :: Time -> Scene s ()
    waitOn, -- :: Scene s a -> Scene s a
    adjustZ, -- :: (ZIndex -> ZIndex) -> Scene s a -> Scene s a
    withSceneDuration, -- :: Scene s () -> Scene s Duration

    -- * Variables
    Var,
    newVar, -- :: a -> Scene s (Var s a)
    readVar, -- :: Var s a -> Scene s a
    writeVar, -- :: Var s a -> a -> Scene s ()
    modifyVar, -- :: Var s a -> (a -> a) -> Scene s ()
    tweenVar, -- :: Var s a -> Duration -> (a -> Time -> a) -> Scene s ()
    simpleVar, -- :: (a -> SVG) -> a -> Scene s (Var s a)
    findVar, -- :: (a -> Bool) -> [Var s a] -> Scene s (Var s a)

    -- * Sprites
    Sprite,
    Frame,
    unVar, -- :: Var s a -> Frame s a
    spriteT, -- :: Frame s Time
    spriteDuration, -- :: Frame s Duration
    newSprite, -- :: Frame s SVG -> Scene s (Sprite s)
    newSprite_, -- :: Frame s SVG -> Scene s ()
    newSpriteA, -- :: Animation -> Scene s (Sprite s)
    newSpriteA', -- :: Sync -> Animation -> Scene s (Sprite s)
    newSpriteSVG, -- :: SVG -> Scene s (Sprite s)
    newSpriteSVG_, -- :: SVG -> Scene s ()
    destroySprite, -- :: Sprite s -> Scene s ()
    applyVar, -- :: Var s a -> Sprite s -> (a -> SVG -> SVG) -> Scene s ()
    spriteModify, -- :: Sprite s -> Frame s ((SVG,ZIndex) -> (SVG, ZIndex)) -> Scene s ()
    spriteMap, -- :: Sprite s -> (SVG -> SVG) -> Scene s ()
    spriteTween, -- :: Sprite s -> Duration -> (Double -> SVG -> SVG) -> Scene s ()
    spriteVar, -- :: Sprite s -> a -> (a -> SVG -> SVG) -> Scene s (Var s a)
    spriteE, -- :: Sprite s -> Effect -> Scene s ()
    spriteZ, -- :: Sprite s -> ZIndex -> Scene s ()
    spriteScope, -- :: Scene s a -> Scene s a

    -- * Object API
    Object,
    ObjectData,
    oNew,
    newObject,
    oModify,
    oModifyS,
    oRead,
    oTween,
    oTweenS,
    oTweenV,
    oTweenVS,
    Renderable (..),

    -- ** Object Properties
    oTranslate,
    oTranslateX,
    oTranslateY,
    oSVG,
    oContext,
    oMargin,
    oMarginTop,
    oMarginRight,
    oMarginBottom,
    oMarginLeft,
    oBB,
    oBBMinX,
    oBBMinY,
    oBBWidth,
    oBBHeight,
    oOpacity,
    oShown,
    oZIndex,
    oEasing,
    oScale,
    oScaleOrigin,
    oTopY,
    oBottomY,
    oLeftX,
    oRightX,
    oCenterXY,
    oCenterX,
    oCenterY,
    oValue,

    -- ** Graphics object methods
    oShow,
    oHide,
    oShowWith,
    oHideWith,
    oFadeIn,
    oFadeOut,
    oGrow,
    oShrink,
    oTransform,
    Origin,
    oScaleIn,
    oScaleIn',
    oScaleOut,
    oScaleOut',
    oDraw,
    oSim,
    oStagger,
    oStaggerRev,
    oStagger',
    oStaggerRev',
    -- , oBalloon

    -- ** Pre-defined objects
    Circle (..),
    circleRadius,
    Rectangle (..),
    rectWidth,
    rectHeight,
    Morph (..),
    morphDelta,
    morphSrc,
    morphDst,
    Camera (..),
    cameraAttach,
    cameraFocus,
    cameraSetZoom,
    cameraZoom,
    cameraSetPan,
    cameraPan,

    -- * ST internals
    liftST,
    transitionO,
    evalScene,
  )
where

import Reanimate.Scene.Core
import Reanimate.Scene.Object
import Reanimate.Scene.Sprite
import Reanimate.Scene.Var
