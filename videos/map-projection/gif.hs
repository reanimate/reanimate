#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
module Main(main) where

import qualified Data.Text as T
import           Codec.Picture
import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Control.Monad.ST
import           Control.Monad
import qualified Data.ByteString     as BS
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Scene
import           Reanimate.GeoProjection
import           System.IO.Unsafe
import           Data.Geospatial         hiding (LonLat)
import           Data.LinearRing
import qualified Data.LineString         as Line
import           Data.Aeson
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Graphics.SvgTree        (PathCommand (..), Tree (None))
import           Data.Foldable
import           Control.Lens            ((^.))


main :: IO ()
main = seq equirectangular $ reanimate $ sceneAnimation $ do
    newSpriteSVG $ mkBackground "white"
    prevProj <- newVar equirectangularP
    let push label proj = do
          prev <- readVar prevProj
          play $ pauseAtEnd waitT $ signalA (curveS 2) $
            mkAnimation morphT $ \t ->
              mkGroup $
              [ grid $ mergeP prev proj t ]
          writeVar prevProj proj

    -- play $ staticFrame morphT $
    --   mkGroup
    --   [ grid equirectangularP ]

    -- push "Lambert" lambertP
    push "Web Mercator" mercatorP
    push "Mollweide" mollweideP
    -- push "Bottomley 30\\degree" (bottomleyP (toRads 30))
    -- 4
    -- pushInterp "Werner" wernerP
    -- 5
    -- pushInterp "Bonne 45\\degree" (bonneP (toRads 45))
    -- pushT
    --   (\t -> "Bonne " <> T.pack (show $ round $ fromToS 45 0 t) <> "\\degree")
    --   (bonneP . toRads . fromToS 45 0)
    -- 6
    -- pushInterp "Eckert I" eckert1P
    -- pushInterp "Eckert III" eckert3P
    -- pushInterp "Eckert IV" eckert5P
    -- 7
    -- push "Fahey" faheyP
    -- 8
    push "August" augustP
    -- 9
    push "Foucaut" foucautP
    -- 10
    push "Lagrange" lagrangeP

    prev <- readVar prevProj
    play $ signalA (curveS 2) $
      mkAnimation morphT $ grid . mergeP prev equirectangularP
  where
    src = equirectangular
    waitT = 0
    morphT = 1

equirectangular :: Image PixelRGB8
equirectangular = unsafePerformIO $ do
  dat <- BS.readFile "earth.jpg"
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGB8 img

toRads :: Double -> Double
toRads dec = dec/180 * pi


grid :: Projection -> SVG
-- grid p = None
grid p =
  withStrokeWidth strokeWidth $
  lowerTransformations $
  scaleXY
    (screenWidth)
    (screenHeight)
   $
  translate (-1/2) (-1/2) $

  withFillOpacity 0 $
  mkGroup
  [ mkGroup []
  , withStrokeColor "black" $
    withFillOpacity 0 $ mkGroup
    [ geometryToSVG p geo
    | geo <- landBorders
    ]
  , withStrokeColor "black" $
    mkGroup $ map mkLinePath (latitudeLines p ++ longitudeLines p)
  ]
  where
    strokeWidth = defaultStrokeWidth * 0.5

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

halfPi :: Double
halfPi = pi/2

landBorders :: [(GeospatialGeometry)]
landBorders = unsafePerformIO $ do
  Just geo <- decodeFileStrict "countries.json"
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
