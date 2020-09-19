#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

import           Text.Printf

{- FE articles/demos
scatter: https://codepen.io/mullany/pen/JmgbRB
distortion: https://codepen.io/mullany/pen/BWKePz
variable stroke width: https://codepen.io/mullany/pen/qaONQm
variable stroke gradient: https://codepen.io/mullany/pen/XXBMJd
heart: https://codepen.io/yoksel/pen/MLVjoB
elastic stroke: https://codepen.io/yoksel/pen/XJbzrO
-}

import           Codec.Picture.Types
import           Control.Lens                    hiding (magma)
import           Graphics.SvgTree
import           Reanimate

main :: IO ()
main = reanimate $ scene $ do
    newSpriteSVG_ $ mkBackground "white"
    let circles =
          [ (900, 0.3)
          , (760, 0.5)
          , (670, 0.4)
          , (600, 0.6)
          , (450, 0.7) ]
    play $ pauseAtEnd 2 $ mkAnimation 10 $ \t ->
      mkGroup
      [ mkGooeyFilter 0.2 -- fromToS 0.1 0.2 $ oscillateS t
      , mkGroup
        [ lowerTransformations $ center $ withFillColor "black" $ latex "Text"
        , mkGroup
          [rotate (t*rot) $ translate (fromToS 0 3.5 $ bellS 2 $ t) 0 $
            withFillColorPixel (promotePixel c) $ mkCircle (size*1.5)
          | (n,(rot, size)) <- zip [0..] circles
          , let c = turbo (n/fromIntegral (length circles-1))
          ]
        ] & filterRef .~ pure (Ref "gooey")

      ]
    -- wait 1

mkGooeyFilter :: Double -> SVG
mkGooeyFilter blur =
  FilterTree $ mkFilter ("gooey")
      [ FEGaussianBlur $ defaultSvg
        & gaussianBlurStdDeviationX .~ Num blur
        & gaussianBlurEdgeMode      .~ EdgeNone
        & filterResult              .~ Just "blur"
      , FEColorMatrix $ defaultSvg
        & colorMatrixType           .~ Matrix
        & colorMatrixValues         .~ printf
          "1 0 0 0 0 \
          \0 1 0 0 0 \
          \0 0 1 0 0 \
          \0 0 0 %f %f" mul sub
        & colorMatrixIn             .~ pure (SourceRef "blur")
        & filterResult              .~ Just "colormatrix"
      ]
  where
    -- Increase alpha contrast. Turns alphaMin to 0x00 and alphaMax to 0xFF.
    -- Increasing alphaDiff makes the shapes softer. Decreasing it gives more
    -- contrast.
    -- alphaMin*mul + sub*255 = 0
    -- alphaMax*mul + sub*255 = 255
    -- alphaDiff*mul = 255
    -- mul = 255/alphaDiff
    -- sub = -alphaMin*mul/255
    mul = 255/alphaDiff
    sub = -alphaMin*mul/255
    alphaCenter = 255/2 :: Double
    alphaDiff   = 20 :: Double
    alphaMin = alphaCenter - alphaDiff/2
    -- alphaMax = alphaCenter + alphaDiff/2

mkFilter :: String -> [FilterElement] -> Filter
mkFilter ident fe = defaultSvg & filterChildren .~ fe & attrId ?~ ident
