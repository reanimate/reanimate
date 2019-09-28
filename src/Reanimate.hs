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
    reanimate,
    -- * Animations
    Animation(..),
    mkAnimation,
    animate,
    duration,
    -- ** Composition
    before,
    sim,
    simLoop,
    simDrop,
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
    -- ** Signals
    Signal,
    signalFlat,
    signalLinear,
    signalFromTo,
    signalReverse,
    signalCurve,
    signalBell,
    signalOscillate,
    signalFromList,
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
    -- ** External SVG providers
    latex,
    latexAlign,
    xelatex,
    povray,
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
    defaultStrokeWidth
  ) where

import           Reanimate.Animation
import           Reanimate.ColorMap
import           Reanimate.Constants
import           Reanimate.Driver
import           Reanimate.LaTeX
import           Reanimate.Povray
import           Reanimate.Raster
import           Reanimate.Signal
import           Reanimate.Svg
import           Reanimate.Svg.BoundingBox
import           Reanimate.Svg.Constructors
import           Reanimate.Svg.LineCommand
