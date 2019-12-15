#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import qualified Data.Text          as T
import           Graphics.SvgTree
import           Reanimate
import           System.IO.Unsafe

main :: IO ()
main = reanimate $ mkAnimation dur $ \t -> unsafePerformIO $ do
    let frame = round (t*dur*fromIntegral pFPS) :: Integer
    threadDelay (10^(6::Integer))
    evaluate $ mkGroup
      [ mkBackground "black"
      , translate (-4) 0 $
        withFillColor "white" $
        mkText (T.pack $ "Frame: " ++ show frame)
          & textAnchor .~ pure TextAnchorStart
      ]
  where
    dur = 2
