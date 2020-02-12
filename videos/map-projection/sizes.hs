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
import Data.Maybe
import qualified Data.ByteString         as BS
import           Data.Foldable
import           Data.Geospatial         hiding (LonLat)
import           Data.LinearRing
import qualified Data.LineString         as Line
import           Data.Map                (Map)
import qualified Data.Map                as Map
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
    play $ setDuration 2 $ animate $ \t ->
      grid (orthoP (fromToS (-pi) pi t) 1)
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
  mkGroup
  [ mkGroup []
  , withStrokeColorPixel (PixelRGBA8 0x90 0x90 0x90 0x0) $
    withStrokeColor "black" $
    withFillOpacity 0 $ mkGroup
    [ geometryToSVG p geo
    | (_,geo) <- landBorders
    ]
  -- , withStrokeColorPixel (PixelRGBA8 0x30 0x30 0x30 0x0) $
  --   withStrokeColor "black" $
  --   mkGroup $ map mkLinePath (latitudeLines p ++ longitudeLines p)
  ]
  where
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

gridLines :: Projection -> Int -> Int -> [[(Double, Double)]]
gridLines p latLines lonLines =
    longitudeLine maxLat :
    longitudeLine (-maxLat) :
    map longitudeLine (stepper (-halfPi) halfPi lonLines) ++ 
    map latitudeLine (stepper (-pi) pi latLines)
  where
    segments = 100
    stepper from to nMax =
      [ fromToS from to (fromIntegral n / fromIntegral nMax)
      | n <- [0 .. nMax] ]
    maxLat = atan (sinh pi)
    latitudeLine lam =
      [ (x, y)
      | n <- [0..segments]
      , let phi = fromToS (-maxLat) maxLat (n/segments)
      , let XYCoord x y = projectionForward p $ LonLat lam phi
      , not (isNaN x || isNaN y) ]
    longitudeLine phi =
      [ (x, y)
      | n <- [0..segments]
      , let lam = fromToS (-pi) pi (n/segments)
      , let XYCoord x y = projectionForward p $ LonLat lam phi
      , not (isNaN x || isNaN y) ]


halfPi :: Double
halfPi = pi/2

landBorders :: [([String], GeospatialGeometry)]
landBorders = unsafePerformIO $ do
  -- Just geo <- decodeFileStrict "land.geojson"
  Just geo <- decodeFileStrict "countries.json"
  Just geoStates <- decodeFileStrict "states.json"
  return
    [ (Map.keys p, feature ^. geometry)
    | feature <- toList (geo ^. geofeatures) ++ toList (geoStates ^. geofeatures)
    , let p = feature ^. properties :: Map String Value
    -- , Map.lookup "admin" p == Just "United States of America"
    -- , Map.lookup "adm0_name" p == Just "United States of America"
    -- , Map.lookup "name" p /= Just "Alaska"
    -- , Map.lookup "name" p /= Just "Hawaii"
    -- , Map.lookup "TYPE" p == Just "Dependency"
    ]

-- drawFeatureCollection :: GeoFeatureCollection a -> (a -> SVG -> SVG) -> SVG
-- loadFeatureColection :: FromJSON a => FilePath -> (a -> SVG -> SVG) -> SVG
-- modifyPoints :: ((Double,Double) -> (Double, Double)) -> SVG -> SVG
-- pointsToRadians :: SVG -> SVG
-- applyProjection :: Projection -> SVG -> SVG

geometryToSVG :: Projection -> GeospatialGeometry -> SVG
geometryToSVG p geometry =
  case geometry of
    MultiPolygon mpolygon ->
      mkGroup $ map (geometryToSVG p . Polygon) $ toList (splitGeoMultiPolygon mpolygon)
    Polygon poly ->
      mkGroup
      [ mkLinePath section
      | section <- pure
          [ (x', y')
          | PointXY x y <- map retrieveXY (fromLinearRing (head (toList (poly^.unGeoPolygon))))
          , let XYCoord x' y' = projectionForward p $ LonLat (x/180*pi) (y/180*pi)
          ]
      ]
    Line line ->
      mkLinePath
      [ (x', y')
      | PointXY x y <- map retrieveXY (Line.fromLineString (line ^. unGeoLine))
      , let XYCoord x' y' = projectionForward p $ LonLat (x/180*pi) (y/180*pi)
      ]
    MultiLine ml ->
      mkGroup $ map (geometryToSVG p . Line) $ toList (splitGeoMultiLine ml)
    _ -> None
