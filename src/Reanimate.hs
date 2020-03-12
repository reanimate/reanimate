{-|
Module      : Reanimate
Description : SVG-base animation library.
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

Reanimate is an animation library based on SVGs. It is designed to act like glue
between external components such as 'latex', 'ffmpeg', 'gnuplot', 'diagrams',
and 'povray'.

-}
module Reanimate
  ( -- * Driver
    --
    -- | Reanimate features a web-based viewer which is opened by default if
    --   no other parameters are given. Key features:
    --
    --   * This viewer listens for changes to the source file and recompiles the
    --     code automatically as needed.
    --   * Animations are rendered with increasing fidelity until the frame
    --     rate reaches 60 fps.
    --   * Key commands for pausing, frame stepping, forward/rewind.
    --     To pause press SPACE, to move -1\/+1\/-10\/+10 frames use LEFT\/RIGHT\/DOWN\/UP arrow keys.
    reanimate,
    -- * Animations
    SVG,
    Time,
    Animation(..),
    mkAnimation,
    animate,
    staticFrame,
    duration,
    frameAt,
    -- ** Composition
    seqA,
    parA,
    parLoopA,
    parDropA,
    pause,
    andThen,
    mapA,
    pauseAtEnd,
    pauseAtBeginning,
    pauseAround,
    adjustDuration,
    setDuration,
    reverseA,
    playThenReverseA,
    repeatA,
    freezeAtPercentage,
    addStatic,
    signalA,
    -- ** Signals
    Signal,
    constantS,
    fromToS,
    reverseS,
    curveS,
    bellS,
    oscillateS,
    fromListS,
    cubicBezierS,
    -- ** Scenes
    (#),
    Scene
  , ZIndex
  , sceneAnimation    -- :: (forall s. Scene s a) -> Animation
  , play              -- :: Animation -> Scene s ()
  , fork              -- :: Scene s a -> Scene s a
  , queryNow          -- :: Scene s Time
  , wait              -- :: Duration -> Scene s ()
  , waitUntil         -- :: Time -> Scene s ()
  , waitOn            -- :: Scene s a -> Scene s a
  , adjustZ           -- :: (ZIndex -> ZIndex) -> Scene s a -> Scene s a
  , withSceneDuration -- :: Scene s () -> Scene s Duration
  -- *** Variables
  , Var
  , newVar            -- :: a -> Scene s (Var s a)
  , readVar           -- :: Var s a -> Scene s a
  , writeVar          -- :: Var s a -> a -> Scene s ()
  , modifyVar         -- :: Var s a -> (a -> a) -> Scene s ()
  , tweenVar          -- :: Var s a -> Duration -> (a -> Time -> a) -> Scene s ()
  , tweenVarUnclamped -- :: Var s a -> Duration -> (a -> Time -> a) -> Scene s ()
  , simpleVar         -- :: (a -> SVG) -> a -> Scene s (Var s a)
  , findVar           -- :: (a -> Bool) -> [Var s a] -> Scene s (Var s a)
  -- *** Sprites
  , Sprite
  , Frame
  , unVar             -- :: Var s a -> Frame s a
  , spriteT           -- :: Frame s Time
  , spriteDuration    -- :: Frame s Duration
  , newSprite         -- :: Frame s SVG -> Scene s (Sprite s)
  , newSpriteA        -- :: Animation -> Scene s (Sprite s)
  , newSpriteA'       -- :: Sync -> Animation -> Scene s (Sprite s)
  , newSpriteSVG      -- :: SVG -> Scene s (Sprite s)
  , destroySprite     -- :: Sprite s -> Scene s ()
  , applyVar          -- :: Var s a -> Sprite s -> (a -> SVG -> SVG) -> Scene s ()
  , spriteModify      -- :: Sprite s -> Frame s ((SVG,ZIndex) -> (SVG, ZIndex)) -> Scene s ()
  , spriteMap         -- :: Sprite s -> (SVG -> SVG) -> Scene s ()
  , spriteTween       -- :: Sprite s -> Duration -> (Double -> SVG -> SVG) -> Scene s ()
  , spriteVar         -- :: Sprite s -> a -> (a -> SVG -> SVG) -> Scene s (Var s a)
  , spriteE           -- :: Sprite s -> Effect -> Scene s ()
  , spriteZ,          -- :: Sprite s -> ZIndex -> Scene s ()

    -- ** Effects
    Effect,
    overBeginning,
    overEnding,
    overInterval,
    reverseE,
    delayE,
    applyE,
    constE,
    fadeInE,
    fadeOutE,
    fadeLineInE,
    fadeLineOutE,
    drawInE,
    drawOutE,
    fillInE,
    scaleE,
    translateE,
    aroundCenterE,

    -- * SVG
    module Reanimate.Svg.Constructors,
    module Reanimate.Svg.LineCommand,
    module Reanimate.Svg.BoundingBox,
    module Reanimate.Svg,
    -- ** Raster data
    embedImage,
    embedDynamicImage,
    embedPng,
    raster,
    svgAsPngFile,
    vectorize,
    vectorize_,
    -- ** External SVG providers
    latex,
    latexAlign,
    xelatex,
    -- * External 3D renderers
    povray,
    povray',
    blender,
    blender',
    -- * Colormaps
    turbo,
    viridis,
    magma,
    inferno,
    plasma,
    sinebow,
    parula,
    cividis,
    jet,
    hsv,
    hsvMatlab,
    greyscale,
    -- * Constants
    screenWidth,
    screenHeight,
    defaultDPI,
    defaultStrokeWidth,
    -- * Parameters
    pFPS,
    pHeight,
    pWidth
  ) where

import           Reanimate.Animation
import           Reanimate.Blender
import           Reanimate.ColorMap
import           Reanimate.Constants
import           Reanimate.Driver
import           Reanimate.LaTeX
import           Reanimate.Parameters
import           Reanimate.Povray
import           Reanimate.Raster
import           Reanimate.Effect
import           Reanimate.Scene
import           Reanimate.Signal
import           Reanimate.Svg
import           Reanimate.Svg.BoundingBox
import           Reanimate.Svg.Constructors
import           Reanimate.Svg.LineCommand
