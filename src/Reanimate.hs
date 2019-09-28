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
    -- * Signals
    Signal,
    signalFlat,
    signalLinear,
    signalFromTo,
    signalReverse,
    signalCurve,
    signalBell,
    signalOscillate,
    signalFromList,
    -- * External tools
    latex,
    latexAlign,
    xelatex
  ) where

import           Reanimate.Animation
import           Reanimate.Driver
import           Reanimate.LaTeX
import           Reanimate.Signal
-- import           Reanimate.Svg
