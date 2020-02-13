#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Control.Lens            ((^.))
import           Control.Monad
import           Control.Monad.ST
import           Data.Aeson
import qualified Data.ByteString         as BS
import           Data.Foldable
import           Data.Geospatial         hiding (LonLat)
import           Data.LinearRing
import qualified Data.LineString         as Line
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe
import qualified Data.Text               as T
import           Graphics.SvgTree        (PathCommand (..), Tree (None))
import           Reanimate
import           Reanimate.Animation
import           Reanimate.GeoProjection
import           Reanimate.Scene
import           System.IO.Unsafe


{-
  Show the earth.
  Rotate
-}

main :: IO ()
main = seq equirectangular $ reanimate $ sceneAnimation $ do
    bg <- newSpriteSVG $ mkBackground "white"
    spriteZ bg (-1)
    -- newSpriteSVG $ grid (orthoP 0 0)
    lam <- newVar (-pi)
    phi <- newVar 1
    -- newSprite $
    --   grid <$> (orthoP <$> unVar lam <*> unVar phi)
    --tweenVar lam (3) $ \v -> fromToS v (pi) . curveS 2
    -- tweenVar phi 3 $ \v -> fromToS v halfPi . curveS 2
    -- tweenVar phi 3 $ \v -> fromToS v (-halfPi) . curveS 2
    -- tweenVar phi 3 $ \v -> fromToS v halfPi . curveS 2
    -- tweenVar phi 3 $ \v -> fromToS v (-halfPi) . curveS 2
    -- play $ staticFrame 1 $
    --   grid equirectangularP
    play $ pauseAtEnd 1 $ setDuration 2 $ animate $ \t ->
      grid $ (orthoP 0 0)
    --wait 1
  where
    src = equirectangular
    waitT = 2
    morphT = 2

renderLabel label =
  let ref = scale 1.5 $ latex "\\texttt{Tygv123}"
      glyphs = scale 1.5 $ latex ("\\texttt{" <> label <> "}")
      svgTxt = mkGroup
        [ withStrokeColor "black" $ withFillColor "white" $
          glyphs
        , withFillColor "white" $
          glyphs ]
  in
    translate (screenWidth*0.01) (screenHeight*0.02) $
    translate (-screenWidth/2) (-screenHeight/2) $
    translate 0 (svgHeight ref) svgTxt

equirectangular :: Image PixelRGB8
equirectangular = unsafePerformIO $ do
  dat <- BS.readFile "earth.jpg"
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGB8 img

toRads :: Double -> Double
toRads dec = dec/180 * pi


grid :: Projection -> SVG
grid p =
  lowerTransformations $
  scaleXY
    (screenWidth)
    (screenHeight)
   $
  translate (-1/2) (-1/2) $
  withStrokeWidth strokeWidth $

  withFillOpacity 0 $
  -- withFillColor "black" $
  mkGroup
  [ mkGroup []
  , withStrokeColor "black" $
    applyProjection p $
    svgPointsToRadians $
    pathify $ gen annotate
  -- , withStrokeColorPixel (PixelRGBA8 0x90 0x90 0x90 0x0) $
  --   withStrokeColor "black" $
  --   withFillOpacity 0 $ mkGroup
  --   [ geometryToSVG p geo
  --   | (_,geo) <- landBorders
  --   ]
  , withStrokeColorPixel (PixelRGBA8 0x30 0x30 0x30 0x0) $
    withStrokeColor "black" $
    applyProjection p $ pathify $
    mkGroup $ map mkLinePath (gridLines 15 15)
  ]
  where
    gen = loadFeatureCollection "countries.json"
    annotate :: Map String Value -> SVG -> SVG
    annotate _ svg = svg
    strokeWidth = defaultStrokeWidth * 0.3

worldLine :: Projection -> SVG
worldLine p =
  mkLinePath $
    map apply
    [ (-pi, -halfPi)
    , (-pi, halfPi)
    , (pi, halfPi)
    , (pi, -halfPi)
    , (-pi, -halfPi) ]
  where
    apply (lam, phi) =
      let XYCoord x y = projectionForward p $ LonLat lam phi
      in (x, y)

gridLines :: Int -> Int -> [[(Double, Double)]]
gridLines latLines lonLines =
    map longitudeLine (stepper (-halfPi) halfPi lonLines) ++
    map latitudeLine (stepper (-pi) pi latLines)
  where
    segments = 2
    stepper from to nMax =
      [ fromToS from to (fromIntegral n / fromIntegral (nMax+1))
      | n <- [1 .. nMax] ]
    maxLat = halfPi -- atan (sinh pi)
    latitudeLine lam =
      [ (lam, phi)
      | n <- [0..segments]
      , let phi = fromToS (-maxLat) maxLat (n/segments) ]
    longitudeLine phi =
      [ (lam, phi)
      | n <- [0..segments]
      , let lam = fromToS (-pi) pi (n/segments) ]


halfPi :: Double
halfPi = pi/2
