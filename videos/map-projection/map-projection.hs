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
import           Data.Text               (Text)
import           Graphics.SvgTree        (PathCommand (..), Tree (None))
import           Reanimate
import           Reanimate.Animation
import           Reanimate.GeoProjection
import           Reanimate.Scene
import           System.IO.Unsafe

main :: IO ()
main = reanimate $ animate $ \t ->
  grid (mergeP sinusoidalP wernerP t)

-- p t = projectionForward (mergeP sinusoidalP wernerP t)

grid :: Projection -> SVG
grid p =

  scaleXY
    (screenWidth)
    (screenHeight)
   $
  translate (-1/2) (-1/2) $
  withStrokeWidth strokeWidth $
  withStrokeColor "black" $
  withFillOpacity 0 $
  mkGroup
  [ mkGroup []
  , mkGroup $ map mkLinePath $ latitudeLines p
  , mkGroup $ map mkLinePath $ longitudeLines p
  , withFillOpacity 0 $ mkGroup
    [ geometryToSVG p geo
    | geo <- landBorders
    ]
  ]
  where
    latLines = 5
    lonLines = 5
    maxLat = atan (sinh pi)
    strokeWidth = defaultStrokeWidth * 0.01

latitudeLines :: Projection -> [[(Double, Double)]]
latitudeLines p =
    [ latitudeLine (fromToS (-pi) pi (n/(latLines*2)))
    | n <- [0 .. latLines*2]]
  where
    latLines = 5
    segments = 100
    maxLat = atan (sinh pi)
    latitudeLine lam =
      [ (x, y)
      | n <- [0..segments]
      , let phi = fromToS (-maxLat) maxLat (n/segments)
      , let XYCoord x y = projectionForward p $ LonLat lam phi ]

longitudeLines :: Projection -> [[(Double, Double)]]
longitudeLines p =
    longitudeLine maxLat :
    longitudeLine (-maxLat) :
    [ longitudeLine (fromToS (-halfPi) halfPi (n/(lonLines*2)))
    | n <- [0 .. lonLines*2]]
  where
    lonLines = 5
    segments = 100
    maxLat = atan (sinh pi)
    longitudeLine phi =
      [ (x, y)
      | n <- [0..segments]
      , let lam = fromToS (-pi) pi (n/segments)
      , let XYCoord x y = projectionForward p $ LonLat lam phi ]

halfPi :: Double
halfPi = pi/2

landBorders :: [(GeospatialGeometry)]
landBorders = unsafePerformIO $ do
  Just geo <- decodeFileStrict "land.geojson"
  return
    [ (feature ^. geometry)
    | feature <- toList $ geo ^. geofeatures
    , let p = feature ^. properties :: Map String Value
    ]

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
