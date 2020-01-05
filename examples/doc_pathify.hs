#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Reanimate
import           Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv pathifyExample

pathifyExample :: Animation
pathifyExample = animate $ \t -> gridLayout
    [ [ partialSvg t $ pathify $ mkCircle 1
      , partialSvg t $ pathify $ mkRect 2 2
      ]
    , [ partialSvg t $ pathify $ mkEllipse 1 0.5
      , partialSvg t $ pathify $ mkLine (-1, -1) (1, 1)
      ]
    ]
