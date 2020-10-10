{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Reanimate.Debug
  ( traceSVG
  , traceA
  , playTraces
  )
where

import           Control.Exception   (evaluate)
import           Data.IORef          (IORef, atomicModifyIORef', modifyIORef', newIORef)
import qualified Data.Text           as T
import           Reanimate.Animation (Animation, SVG, duration, parA, pause, seqA, staticFrame)
import           Reanimate.Constants (defaultStrokeWidth)
import           Reanimate.LaTeX     (latex)
import           Reanimate.Svg       (center, mkGroup, translate, withFillColor, withStrokeColor,
                                      withStrokeWidth)
import           System.IO.Unsafe    (unsafePerformIO)
import           Text.Printf         (printf)

{-# NOINLINE traceBuffer #-}
traceBuffer :: IORef [Animation]
traceBuffer = unsafePerformIO (newIORef [])

{-# NOINLINE traceSVG #-}
-- | Add SVG image to trace stack.
traceSVG :: SVG -> a -> a
traceSVG = traceA . staticFrame (recip 60)

{-# NOINLINE traceA #-}
-- | Add animation to trace stack.
traceA :: Animation -> a -> a
traceA a v = unsafePerformIO $ do
  modifyIORef' traceBuffer (a :)
  evaluate v

{-# NOINLINE playTraces #-}
-- | Evaluate argument and play back the trace stack.
playTraces :: a -> Animation
playTraces v = unsafePerformIO $ do
  _   <- evaluate v
  lst <- atomicModifyIORef' traceBuffer (\x -> ([], reverse x))
  let n = length lst :: Int
  return $ foldr
    seqA
    (pause 0)
    [ f `parA` staticFrame (duration f) (counter i n) | (i, f) <- zip [1 :: Int ..] lst ]
 where
  counter a b = mkGroup
    [ withStrokeWidth defaultStrokeWidth
    $ withStrokeColor "black"
    $ translate 6.5 4
    $ center
    $ latex
    $ T.pack
    $ printf "%d/%d" a b
    , withStrokeWidth 0
    $ withFillColor "white"
    $ translate 6.5 4
    $ center
    $ latex
    $ T.pack
    $ printf "%d/%d" a b
    ]
