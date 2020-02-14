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
main = seq equirectangular $ reanimate $ pauseAtEnd 2 $
  mapA (withStrokeColor "black") $ sceneAnimation $ do
    bg <- newSpriteSVG $ mkBackground "white"
    spriteZ bg (-1)
    -- fork $ do
    --   wait 2
    --   play $ pauseAtEnd 6 $ setDuration 1 $ signalA (curveS 2) $ animate $ \t ->
    --     translate (fromToS 0 (-6) t)
    --               (fromToS 0 (3) t) $
    --     america (orthoP usaLonLat)
    -- play $ pauseAtEnd 1 $ setDuration 2 $ signalA (curveS 2) $ animate $ \t ->
    --   grid $ orthoP $ fromToLonLat (LonLat 0 0) usaLonLat t
    -- fork $ do
    --   wait 2
    --   play $ pauseAtEnd 3 $ setDuration 1 $ signalA (curveS 2) $ animate $ \t ->
    --     translate (fromToS 0 (6) t)
    --               (fromToS 0 (3) t) $
    --     uk
    -- play $ pauseAtEnd 1 $ setDuration 2 $ signalA (curveS 2) $ animate $ \t ->
    --   grid $ orthoP $ fromToLonLat usaLonLat ukLonLat t
    -- fork $ do
    --   wait 2
    --   play $ pauseAtEnd 0 $ setDuration 1 $ signalA (curveS 2) $ animate $ \t ->
    --     translate (fromToS 0 (6) t)
    --               (fromToS 0 (-2.5) t) $
    --     australia
    -- play $ pauseAtEnd 1 $ setDuration 2 $ signalA (curveS 2) $ animate $ \t ->
    --   grid $ orthoP $ fromToLonLat ukLonLat ausLonLat t

    let s = scaleP 0.75 0.75
    play $ pauseAtEnd 1 $ animate $ \t ->
      mkGroup
      [ center $ withStrokeColor "red" $
        america (mergeP (s $ orthoP usaLonLat) lambertP t)
      , center $ america (s $ orthoP usaLonLat)
      ]
    play $ pauseAtEnd 1 $ animate $ \t ->
      mkGroup
      [ center $ withStrokeColor "red" $
        america (mergeP lambertP mollweideP t)
      , center $ america (s $ orthoP usaLonLat)
      ]
    play $ pauseAtEnd 1 $ animate $ \t ->
      mkGroup
      [ center $ withStrokeColor "red" $
        america (mergeP mollweideP collignonP t)
      , center $ america (s $ orthoP usaLonLat)
      ]

    -- play $ animate $ \t ->
    --   grid $ orthoP $ LonLat 0 (halfPi+0.1)
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

usaLonLat = svgToLonLat americaE
ukLonLat = svgToLonLat ukE
ausLonLat = svgToLonLat australiaE

svgToLonLat :: SVG -> LonLat
svgToLonLat svg =
    LonLat (cx / (screenWidth/2) * pi)
           (cy / (screenHeight/2) * halfPi)
  where
    cx = x + w/2
    cy = y + h/2
    (x, y, w, h) = boundingBox svg

fromToLonLat (LonLat lam1 phi1) (LonLat lam2 phi2) t =
  LonLat (fromToS lam1 lam2 t) (fromToS phi1 phi2 t)

toRads :: Double -> Double
toRads dec = dec/180 * pi

fetchCountry :: Projection -> (Map String Value -> SVG -> Maybe SVG) -> SVG
fetchCountry p checker =
    lowerTransformations $
    scaleXY
      (screenWidth)
      (screenHeight)
     $
    translate (-1/2) (-1/2) $
    withStrokeWidth strokeWidth $

    withFillOpacity 0 $
    mkGroup
    [ mkGroup []
    , applyProjection p $
      svgPointsToRadians $
      pathify $ gen annotate
    ]
  where
    gen = loadFeatureCollection "countries.json"
    annotate :: Map String Value -> SVG -> SVG
    annotate props svg = fromMaybe None (checker props svg)
    strokeWidth = defaultStrokeWidth * 0.3

america :: Projection -> SVG
america p = fetchCountry p $ \props svg -> do
  "United States of America" <- Map.lookup "NAME" props
  return $ snd $ splitGlyphs [75] svg

americaE :: SVG
americaE = fetchCountry equirectangularP $ \props svg -> do
  "United States of America" <- Map.lookup "NAME" props
  return $ snd $ splitGlyphs [75] svg

uk :: SVG
uk = fetchCountry (orthoP ukLonLat) $ \props svg -> do
  name <- Map.lookup "NAME" props
  guard (name `elem` ["United Kingdom"])
  return svg

ukE :: SVG
ukE = fetchCountry equirectangularP $ \props svg -> do
  "United Kingdom" <- Map.lookup "NAME" props
  return svg

australia :: SVG
australia = fetchCountry (orthoP ausLonLat) $ \props svg -> do
  "Australia" <- Map.lookup "NAME" props
  return $ snd $ splitGlyphs [0] svg

australiaE :: SVG
australiaE = fetchCountry equirectangularP $ \props svg -> do
  "Australia" <- Map.lookup "NAME" props
  return $ snd $ splitGlyphs [0] svg

-- Alaska: 16
-- Continent: 75
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
  , withStrokeColor "black" $
    applyProjection p $ pathify $
    mkGroup $ map mkLinePath (gridLines 8 4)
  ]
  where
    gen = loadFeatureCollection "countries.json"
    annotate :: Map String Value -> SVG -> SVG
    annotate props svg =
      case Map.lookup "NAME" props of
        Nothing -> None
        Just name
          | name `elem` [ "United Kingdom", "France", "Germany", "Denmark" ] ->
            svg
          | name `elem` [ "Australia" ] ->
            svg -- snd $ splitGlyphs [0] svg
          | name `elem` [ "United States of America" ] ->
            snd $ splitGlyphs [75] svg
          | otherwise -> None
    strokeWidth = defaultStrokeWidth * 0.3

gridLines :: Int -> Int -> [[(Double, Double)]]
gridLines latLines lonLines =
    map longitudeLine (stepper (-halfPi) halfPi (lonLines+1)) ++
    map latitudeLine (stepper (-pi) pi (latLines))
  where
    segments = 2
    stepper from to nMax =
      [ fromToS from to (fromIntegral n / fromIntegral (nMax))
      | n <- [0 .. nMax-1] ]
    maxLat = halfPi -- atan (sinh pi)
    latitudeLine lam =
      [ (lam + pi/fromIntegral latLines, phi)
      | n <- [0..segments]
      , let phi = fromToS (-maxLat) maxLat (n/segments) ]
    longitudeLine phi =
      [ (lam, phi)
      | n <- [0..segments]
      , let lam = fromToS (-pi) pi (n/segments) ]


halfPi :: Double
halfPi = pi/2
