#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens           ((&))
import           Graphics.SvgTree       (LineJoin (..))
import           Reanimate
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

main :: IO ()
main = reanimate $
  addStatic (mkBackgroundPixel bgColor) $
  mapA (withStrokeWidth defaultStrokeWidth) $
  mapA (withStrokeColor "black") $
  mapA (withStrokeLineJoin JoinRound) $
  mapA (withFillOpacity 1) $
    scene $
      showPair (stages ++ take 1 stages)
  where
    showPair (from:to:rest) =
      waitOn $ do
        toS <- newSpriteSVG $
          translate (4) 0 to
        spriteE toS $ overBeginning 0.2 fadeInE
        spriteE toS $ overEnding 0.2 fadeOutE
        m <- fork $ newSpriteA $ animate (morph linear from to)
          & signalA (curveS 4)
          & mapA (translate (-4) 0)
        wait 4
        destroySprite m
        destroySprite toS
        showPair (to:rest)
        return ()
    showPair _ = return ()

    stages = map (lowerTransformations . scale 8 . pathify . center) $ colorize
      [ latex "X"
      , latex "$\\aleph$"
      , latex "Y"
      , latex "$\\infty$"
      , latex "I"
      , latex "$\\pi$"
      , latex "1"
      , latex "S"
      , mkRect 0.5 0.5
      ]

colorize :: [SVG] -> [SVG]
colorize lst =
  [ withFillColorPixel (promotePixel $ parula (n/fromIntegral (length lst-1))) elt
  | elt <- lst
  | n <- [0..]
  ]
