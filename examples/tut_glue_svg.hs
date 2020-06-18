#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reanimate
import           Reanimate.Transition
import           Reanimate.Builtin.Images

import           Codec.Picture
import           Control.Lens
import           Data.Text                (Text)
import           Graphics.SvgTree         hiding (Text)

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

framePause :: Double
framePause = 3

transitionTime :: Double
transitionTime = 0.5

main :: IO ()
main = reanimate $ bg `parA` chainT (overlapT transitionTime fadeT)
      [comp1, comp2, comp3, comp4, comp5, comp6, comp7, setDuration transitionTime comp1]
  where
    bg = animate $ const $ mkBackgroundPixel bgColor
    comp1 = svgComponent "Circles" (mkCircle 2)
    comp2 = svgComponent "Rects" (mkRect 4 3)
    comp3 = svgComponent "Lines" (mkLine (-2,0) (2,0))
    comp4 = svgComponent "Images" (scale 0.5 svgLogo)
    comp5 = svgComponent "Paths" $
      withFillOpacity 0 $
      scale 8 $ withStrokeWidth defaultStrokeWidth $
      center $ latex "$\\pi$"
    comp6 = svgComponent "Blurs" mkBlur
    comp7 = svgComponent "Blobs" mkBlob

svgComponent :: Text -> SVG -> Animation
svgComponent txt svg = mkAnimation framePause $ const $
  mkGroup
  [ translate 0 (-1) $
    withStrokeWidth (defaultStrokeWidth*2) $
    withStrokeColor "red" $ withFillColor "black" svg
  , translate 0 3 $
    withFillColor "black" $ scale 2 $ center $ latex txt
  ]

mkBlur :: SVG
mkBlur = mkGroup
    [ FilterTree $ mkFilter "blur"
      [FEGaussianBlur $ defaultSvg
        & gaussianBlurStdDeviationX .~ Num dev
        & filterResult ?~ "blur"
      ] & filterWidth .~ pure (Percent 3)
        & filterX .~ pure (Percent (-1))
        & filterHeight .~ pure (Percent 3)
        & filterY .~ pure (Percent (-1))
    , circ
      & filterRef .~ pure (Ref "blur")
    ]
  where
    dev = 0.2
    radius = 2
    circ = mkCircle radius

mkBlob :: SVG
mkBlob =
    mkGroup
    [ FilterTree $ mkFilter "goo"
      [FEGaussianBlur $ defaultSvg
        & gaussianBlurStdDeviationX .~ Num dev
        & filterResult ?~ "blur"
      ,FEColorMatrix $ defaultSvg
        & colorMatrixType .~ Matrix
        & colorMatrixValues .~ "1 0 0 0 0 \
                               \0 1 0 0 0 \
                               \0 0 1 0 0 \
                               \0 0 0 " ++ show (sharpness*2) ++ " -" ++ show sharpness
        & filterResult .~ pure "goo"
      ,FEComposite $ defaultSvg
        & compositeIn .~ pure SourceGraphic
        & compositeIn2 .~ pure (SourceRef "goo")
        & compositeOperator .~ CompositeAtop
      ] & filterWidth .~ pure (Percent 3)
        & filterX .~ pure (Percent (-1))
        & filterHeight .~ pure (Percent 3)
        & filterY .~ pure (Percent (-1))
    , withStrokeWidth 0 $ withFillColor "red" $ mkGroup
      [ translate (0.9*(-radius)) 0 circ
      , translate (0.9*radius) 0 circ
      ] & filterRef .~ pure (Ref "goo")
    ]
  where
    sharpness = 10 :: Integer
    dev = 0.2
    radius = 2
    circ = mkCircle radius


mkFilter :: String -> [FilterElement] -> Filter
mkFilter ident fe = defaultSvg & filterChildren .~ fe & attrId ?~ ident
