#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Reanimate
import           Reanimate.LaTeX
import           Reanimate.Builtin.Documentation

main :: IO ()
main =
  reanimate
    $ docEnv
    $ staticFrame 1
    $ withFillColor "black"
    $ withFillOpacity 1
    $ withStrokeWidth 0
    $ mkGroup
        [ translate (-8) 3 $ latex "Hello Computer Modern $\\frac{a}{b}$"
        , translate (-8) 2
          $ latexCfg calligra "Hello Calligra $\\frac{a}{b}$"
        , translate (-8) 1
          $ latexCfg noto "Hello Noto $\\frac{a}{b}$"
        , translate (-8) 0
          $ latexCfg helvet "Hello Helvetica $\\frac{a}{b}$"
        , translate (-8) (-1)
          $ latexCfg libertine "Hello Libertine $\\frac{a}{b}$"
        , translate (-8) (-2)
          $ latexCfg chalkduster "Hello world"
        , translate (-8) (-3)
          $ latexCfg artemisia "Hello Artemisia $\\frac{H}{b}$"
        ]

artemisia :: TexConfig
artemisia = TexConfig LaTeX
  [ "\\usepackage{gfsartemisia}"
  , "\\usepackage[T1]{fontenc}"
  ] []
