#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

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
import           Control.Monad
import           Data.Fixed
import           Data.Monoid
import           Graphics.SvgTree
import           Linear.V2
import           Linear.Vector
import           Reanimate
import           Reanimate.Builtin.Images
import           System.Random

seed1,seed2,seed3,seed4,seed5,seed6,seed7 :: StdGen
(seed1:seed2:seed3:seed4:seed5:seed6:seed7:_) = map mkStdGen (randoms (mkStdGen 23958))

spokes :: [(PixelRGB8, Double, Double, Double)]
spokes = take 30
  [ (turbo color, position, destination, offset)
  | color <- randomRs (0,1) seed1
  | position <- randomRs (0,360) seed2
  | destination <- randomRs (-90,90) seed3
  | offset <- randomRs (0,1) seed4
  ]

balls :: [(Double, Double, Double)]
balls = take 20
  [ (ang, offset, len)
  | ang <- randomRs (0,pi*2) seed5
  , let degrees = ang/pi*180 :: Double
  , degrees < (270-25) || degrees > (270+25)
  | offset <- randomRs (0,1) seed6
  | len <- randomRs (1,3) seed7
  ]

ctx :: SVG -> SVG
ctx = scale 0.7

-- = CoordUserSpace   -- ^ `userSpaceOnUse` value
--     | CoordBoundingBox -- ^ `objectBoundingBox` value

whiteGithub :: SVG
whiteGithub = withFillOpacity 1 $ withFillColor "white" $
  mkPath $
  extractPath $
  scaleToHeight screenHeight $ center $ unpackImage githubIcon

main :: IO ()
main = reanimate $ scene $ do
    newSpriteSVG_ $ mkBackground "white"
    -- newSpriteSVG_ $
    --   mkClipPath "clip" $
    --   removeGroups $ scaleToHeight screenHeight $ center $ unpackImage githubIcon
    -- newSpriteSVG_ $
    --   withId "circleMask" $ MaskTree $ defaultSvg
    --   & maskContent .~
    --     [ mkBackground "white"
    --     , translate 8 4.5 $ withFillOpacity 1 $ withFillColor "black" $
    --       mkCircle screenTop ]
    --   & maskWidth .~ Percent 2
    --   & maskHeight .~ Percent 2
    newSpriteSVG_ $
      FilterTree $ mkFilter ("gooey")
      [ FEGaussianBlur $ defaultSvg
        & gaussianBlurStdDeviationX .~ Num 0.2
        & gaussianBlurEdgeMode .~ EdgeNone
        & filterResult .~ Just "blur"
      , FEColorMatrix $ defaultSvg
        & colorMatrixType .~ Matrix
        & colorMatrixValues .~
          "1 0 0 0 0 \
          \0 1 0 0 0 \
          \0 0 1 0 0 \
          \0 0 0 19 -10"
        & colorMatrixIn .~ pure (SourceRef "blur")
        & filterResult .~ Just "colormatrix"
      ]
    newSprite_ $ do
      t <- spriteT
      d <- spriteDuration
      pure $
        withId "clip" $ MaskTree $ defaultSvg
        -- & maskContent .~ [translate 8 4.5 $ withFillOpacity 1 $ withFillColor "white" $ mkCircle 1]
        & maskContent .~
          [ mkGroup
            [ whiteGithub
            , withFillColor "white" $ mkGroup
              [(translate x y $ mkCircle (r*2))
              | (ang, offset, len) <- balls
              , let dir = angle ang
                    r = 1 - (mod' (t/d+offset) 1)
                    V2 x y = dir ^* (screenTop * len * (mod' (t/d+offset) 1)) ]
            ] & filterRef .~ pure (Ref "gooey")
          , withFillColor "black" $ mkCircle (screenTop*0.99)
          , whiteGithub
          ]
        & maskWidth .~ Percent 2
        & maskHeight .~ Percent 2
      -- & maskPosition .~ (Num 0,Num 0)
      -- & maskUnits .~ CoordBoundingBox
      -- & maskContentUnits .~ CoordBoundingBox
    newSpriteSVG_ $
      ctx $
      mkGroup [scale 2 $ mkBackground "white"]
      & maskRef .~ (pure $ Ref "clip")
    newSpriteSVG_ $
      FilterTree $ mkFilter ("blur") $
      [ FEGaussianBlur $ defaultSvg
        & gaussianBlurStdDeviationX .~ Num 0.1
        & gaussianBlurEdgeMode .~ EdgeNone
        & filterX .~ pure (Percent 0)
        & filterY .~ pure (Percent 0)
        & filterResult .~ Just "blur"
        & filterWidth .~ pure (Percent 1)
        & filterHeight .~ pure (Percent 1)
      , FETurbulence $ defaultSvg
        & turbulenceBaseFrequency .~ (2, Last Nothing)
        & turbulenceType .~ FractalNoiseType
        & turbulenceNumOctaves .~ 1
        & turbulenceSeed .~ 1
        & turbulenceStitchTiles .~ Stitch
        & filterResult .~ Just "turbulence"
      , FEDisplacementMap $ defaultSvg
        & displacementMapScale .~ pure 2
        & displacementMapIn .~ pure (SourceRef "blur")
        & displacementMapIn2 .~ pure (SourceRef "turbulence")
        & displacementMapXChannelSelector .~ ChannelR
        & displacementMapYChannelSelector .~ ChannelA
        & filterResult .~ Just "displacementMap"
      ]

    -- Color wheels:
    forM_ spokes $ \(color, pos, dst, offset) ->
      fork $ play $ mkAnimation 5 $ \t' ->
        let t = (t' + offset) `mod'` 1
        in ctx $ mkGroup
        [ mkGroup [(rotate (fromToS 0 dst $ curveS 2 $ oscillateS $ t) $
          rotate pos $
          withStrokeColorPixel (promotePixel color) slice)]
          & filterRef .~ pure (Ref "blur")
        ] & maskRef .~ (pure $ Ref "clip")
    wait 5

slice :: SVG
slice =
  withFillOpacity 0 $ withStrokeWidth 16 $ mkCircle 8
  & strokeDashArray .~ pure
  [ Percent 0.10, Percent 1.90 ]

-- colorWheel :: Int -> Int -> (Double -> PixelRGB8) -> SVG
-- colorWheel buckets repeats cm =
--   scale 1.1 $ circlePlot 1000
--   (\ang _r ->
--     let idx = round (((ang+pi)/(2*pi)) * buckets' * repeats') `mod` buckets
--     in promotePixel $ cm (fromIntegral idx * recip buckets'))
--   where
--     buckets' = fromIntegral buckets
--     repeats' = fromIntegral repeats

unpackImage :: SVG -> SVG
unpackImage (SvgTree doc) = unbox doc
unpackImage (GroupTree g) = GroupTree $ g & groupChildren %~ map unpackImage
unpackImage svg           = svg

mkFilter :: String -> [FilterElement] -> Filter
mkFilter ident fe = defaultSvg & filterChildren .~ fe & attrId ?~ ident
