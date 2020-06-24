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
    $ mkGroup
        [ latex "Computer Modern"
        , translate 0    (-2)
          $ latexWithHeaders ["\\usepackage{calligra}"] "\\calligra\nGalligra"
        , translate (-6) (-2)
          $ latexWithHeaders ["\\usepackage{noto}"] "\\normalfont\nNoto"
        , translate (-6) 0
          $ latexWithHeaders ["\\usepackage{helvet}"] "Helvetica"
        ]

