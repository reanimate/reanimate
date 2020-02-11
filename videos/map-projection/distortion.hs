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
    prevProj <- newVar equirectangularP
    txtVar <- newVar "Equirectangular"
    txtS <- newSprite $ renderLabel <$> unVar txtVar
    txtFade <- spriteVar txtS 1 withGroupOpacity
    spriteZ txtS 2
    let pushInterp = pushMerge' (\a b t -> interpP src a b t)
        pushMerge' fn label proj = do
          fork $ do
            tweenVar txtFade 0.2 $ \v -> fromToS v 0 . curveS 2
            writeVar txtVar label
            tweenVar txtFade 0.2 $ \v -> fromToS v 1 . curveS 2
          prev <- readVar prevProj
          play $ pauseAtEnd waitT $ signalA (curveS 2) $
            mkAnimation morphT $ \t ->
              mkGroup $
              [ scaleToSize screenWidth screenHeight $
                embedImage $ fn prev proj t
              , grid $ mergeP prev proj t ]
          writeVar prevProj proj
        pushT mkLabel mkProj = do
          fork $ tweenVar txtVar morphT $ \v t -> if t > 0 then mkLabel t else v
          play $ pauseAtEnd waitT $ signalA (curveS 2) $
            mkAnimation morphT $ \t ->
              mkGroup $
              [ scaleToSize screenWidth screenHeight $
                embedImage $ project src $ mkProj t
              , grid $ mkProj t ]
          writeVar prevProj (mkProj 1)

    play $ staticFrame (waitT/2) $
      mkGroup
      [ scaleToSize screenWidth screenHeight $
        embedImage $ project src equirectangularP
      , grid equirectangularP ]

    -- pushInterp "Lambert" lambertP
    -- 1
    pushInterp "Web Mercator" mercatorP
    -- 2
    pushInterp "Mollweide" mollweideP
    -- 3
    pushInterp "Bottomley 30\\degree" (bottomleyP (toRads 30))
    -- 4
    pushInterp "Werner" wernerP
    -- 5
    pushInterp "Bonne 45\\degree" (bonneP (toRads 45))
    pushT
      (\t -> "Bonne " <> T.pack (show $ round $ fromToS 45 0 t) <> "\\degree")
      (bonneP . toRads . fromToS 45 0)
    -- 6
    pushInterp "Eckert I" eckert1P
    eckert <- newSpriteSVG $ renderLabel "Eckert"
    spriteZ eckert 2
    pushInterp "Eckert III" eckert3P
    pushInterp "Eckert IV" eckert5P
    destroySprite eckert
    -- 7
    pushInterp "Fahey" faheyP
    -- 8
    pushInterp "August" augustP
    -- 9
    pushInterp "Foucaut" foucautP
    -- 10
    pushInterp "Lagrange" lagrangeP
    pushInterp "Equirectangular" equirectangularP
  where
    src = equirectangular
    waitT = 1
    morphT = 1

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
-- grid p = None
grid p =

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
    withFillOpacity 0 $ mkGroup
    [ geometryToSVG p geo
    | geo <- landBorders
    ]
  , withStrokeColorPixel (PixelRGBA8 0x30 0x30 0x30 0x0) $
    mkGroup $ map mkLinePath (latitudeLines p ++ longitudeLines p)
  ]
  where
    strokeWidth = defaultStrokeWidth * 0.02

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
    latLines = 4
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
    lonLines = 4
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
