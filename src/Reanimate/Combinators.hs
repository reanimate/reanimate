{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Reanimate.Combinators where

import           Control.Arrow
import           Data.Fixed      (mod')
import           Data.Monoid     ((<>))
import           Data.Text       (Text, pack)
import qualified Data.Text       as T

-- import           Reanimate.Arrow

type Path = [(Double, Double)]

approxFnData :: Int -> (Double -> (Double, Double)) -> Path
approxFnData steps fn =
  fn 0 : [ fn (fromIntegral n/fromIntegral steps) | n <- [0..steps] ]

morphPath :: Path -> Path -> Double -> Path
morphPath src dst idx = zipWith worker src dst
  where
    worker (x1, y1) (x2, y2) =
      (x1 + (x2-x1)*idx
      ,y1 + (y2-y1)*idx)

-- signalSigmoid :: Double -> Double -> Double -> Ani Double
-- signalSigmoid steepness from to = proc () -> do
--   s <- signal 0 1 -< ()
--   let s' = (s-0.5)*steepness
--   let sigmoid = exp s' / (exp s'+1)
--       ret = (from + (to-from)*sigmoid)
--   returnA -< ret
--
-- signalSCurve :: Double -> Double -> Double -> Ani Double
-- signalSCurve steepness from to = proc () -> do
--   s <- signal 0 1 -< ()
--   let s' = if s < 0.5
--               then 0.5 * (2*s)**steepness
--               else 1-0.5 * (2 - 2*s)**steepness
--   returnA -< from + (to-from)*s'
