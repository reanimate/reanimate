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
    --     rate eaches 60 fps.
    --   * Key commands for pausing, frame stepping, forward/rewind.
    --     To pause press SPACE, to move -1\/+1\/-10\/+10 frames use LEFT\/RIGHT\/DOWN\/UP arrow keys.
    reanimate,
    -- * Animations
    SVG,
    Time,
    Animation(..),
    mkAnimation,
    animate,
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
    -- ** Scenes
    (#),
    ZIndex,
    Scene,
    sceneAnimation,
    fork,
    play,
    queryNow,
    waitAll,
    waitUntil,
    wait,
    adjustZ ,
    withSceneDuration,
    newSprite,
    newSpriteA,
    newSpriteSVG,
    destroySprite,
    spriteE,
    newVar,
    tweenVar,
    unVar,
    spriteT,
    spriteDuration,

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
    transitions,

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
