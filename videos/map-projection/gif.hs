#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo #-}
module Main(main) where

import           Control.Lens                    ((^.))
import           Data.Aeson
import           Data.Foldable                   (toList)
import           Data.Geospatial                 (GeoFeature (..),
                                                  GeospatialGeometry (..),
                                                  PointXY (..), geofeatures,
                                                  geometry, retrieveXY,
                                                  splitGeoMultiLine,
                                                  splitGeoMultiPolygon,
                                                  unGeoLine, unGeoPolygon)
import           Data.LinearRing                 (fromLinearRing)
import           Data.LineString                 (fromLineString)
import           Graphics.SvgTree                (Tree (None))
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.GeoProjection
import           System.IO.Unsafe


main :: IO ()
main = reanimate $ sceneAnimation $ do
    -- Set the background to 'rtfdBackgroundColor'
    newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor

    -- We'll be cycling through projections so let's create a variable
    -- containing the current projection.
    prevProj <- newVar equirectangularP
    -- Now we can define a function that animates smoothly from the
    -- current projection to a new projection.
    let push _label proj = do
          prev <- readVar prevProj
          play $ animate (\t -> grid $ mergeP prev proj t)
            # setDuration morphT -- Set the length of the animation
            # signalA (curveS 2) -- Ease in and ease out.
            # pauseAtEnd waitT   -- Then wait on the last frame.
          -- The morph from one projection to another has finished so
          -- update the variable with new projection.
          writeVar prevProj proj

    -- Cycle from 'equirectangularP' through 5 projections and then
    -- back to 'equirectangularP'.
    push "Mollweide" mollweideP
    push "Bottomley 30\\degree" (bottomleyP (toRads 30))
    push "Werner" wernerP
    push "Foucaut" foucautP
    push "Lagrange" lagrangeP

    prev <- readVar prevProj
    play $ animate (\t -> grid $ mergeP prev equirectangularP t)
      # setDuration morphT -- Set the length of the animation
      # signalA (curveS 2) -- Ease in and ease out.
      # pauseAtEnd waitT   -- Then wait on the last frame.
  where
    waitT = 0  -- Seconds to wait between transformations
    morphT = 1 -- Duration (in seconds) of each transformation

-- Draw grid lines and land borders.
grid :: Projection -> SVG
grid p =
  withStrokeWidth strokeWidth $
  lowerTransformations $
  scaleXY screenWidth screenHeight $
  translate (-1/2) (-1/2) $
  withFillOpacity 0 $ withStrokeColor "black" $
  mkGroup
  [ mkGroup
    [ geometryToSVG p geo
    | geo <- landBorders
    ]
  , mkGroup $ map mkLinePath (latitudeLines p ++ longitudeLines p)
  ]
  where
    strokeWidth = defaultStrokeWidth * 0.5

latitudeLines :: Projection -> [[(Double, Double)]]
latitudeLines p =
    [ latitudeLine (fromToS (-pi) pi (n/(latLines*2)))
    | n <- [0 .. latLines*2]]
  where
    latLines = 2
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
    | n <- [1 .. lonLines*2-1] ]
  where
    lonLines = 2
    segments = 100
    maxLat = atan (sinh pi)
    longitudeLine phi =
      [ (x, y)
      | n <- [0..segments]
      , let lam = fromToS (-pi) pi (n/segments)
      , let XYCoord x y = projectionForward p $ LonLat lam phi ]

landBorders :: [(GeospatialGeometry)]
landBorders = unsafePerformIO $ do
  Just geo <- decodeFileStrict "countries.json"
  return
    [ (feature ^. geometry)
    | feature <- toList $ geo ^. geofeatures :: [GeoFeature Value]
    ]

geometryToSVG :: Projection -> GeospatialGeometry -> SVG
geometryToSVG p geo =
  case geo of
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
      | PointXY x y <- map retrieveXY (fromLineString (line ^. unGeoLine))
      , let XYCoord x' y' = projectionForward p $ LonLat (x/180*pi) (y/180*pi)
      ]
    MultiLine ml ->
      mkGroup $ map (geometryToSVG p . Line) $ toList (splitGeoMultiLine ml)
    _ -> None

-- Convert degrees to radians
toRads :: Double -> Double
toRads dec = dec/180 * pi

halfPi :: Double
halfPi = pi/2
