#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
module Main(main) where

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


{-
  1. equirectangular
  2. lambert
  3. mercator
  4. mollweide
  5. hammer
  6. sinusoidal
-}
main :: IO ()
main = seq equirectangular $ reanimate $ sceneAnimation $ do
    prevProj <- newVar equirectangularP
    txtVar <- newVar "Equirectangular"
    txtS <- newSprite $ do
      txt <- unVar txtVar
      pure $
        let ref = scale 1.5 $ latex "\\texttt{Tyg}"
            glyphs = scale 1.5 $ latex ("\\texttt{" <> txt <> "}")
            svgTxt = mkGroup
              [ withStrokeColor "black" $ withFillColor "white" $
                glyphs
              , withFillColor "white" $
                glyphs ]
        in
          translate (screenWidth*0.01) (screenHeight*0.02) $
          translate (-screenWidth/2) (-screenHeight/2) $
          translate 0 (svgHeight ref) svgTxt
    txtFade <- spriteVar txtS 1 withGroupOpacity
    spriteZ txtS 2
    let pushMerge = pushMerge' (\a b t -> project src $ mergeP a b t)
        pushInterp = pushMerge' (\a b t -> interpP src a b t)
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

    play $ staticFrame (waitT/2) $
      mkGroup
      [ scaleToSize screenWidth screenHeight $
        embedImage $ project src equirectangularP
      , grid equirectangularP ]

    pushInterp "Lambert" lambertP
    pushInterp "Web Mercator" mercatorP
    pushInterp "Mollweide" mollweideP
    pushInterp "Hammer" hammerP
    pushInterp "Bottomley 30\\degree" (bottomleyP (toRads 30))
    pushInterp "Sinusoidal" sinusoidalP
    pushInterp "Werner" wernerP
    pushInterp "Bonne 45\\degree" (bonneP (toRads 45))
    pushInterp "Eckert v1" eckert1P
    pushInterp "Eckert v3" eckert3P
    pushInterp "Eckert v5" eckert5P
    pushInterp "Collignon" collignonP
    pushInterp "Fahey" faheyP
    pushInterp "Equirectangular" equirectangularP
    -- pushInterp "Orthographic" (orthoP orthoStart 0)
    --
    -- play $ pauseAtEnd 1 $ signalA (curveS 2) $ mkAnimation orthoRotT $ \t ->
    --   mkGroup
    --   [ scaleToSize screenWidth screenHeight $
    --     embedImage $ project src $ orthoP (fromToS orthoStart orthoEnd t) 0
    --   , grid $ orthoP (fromToS orthoStart orthoEnd t) 0
    --   ]

    -- let toFahey = animate $ \t ->
    --       mkGroup
    --       [ scaleToSize screenWidth screenHeight $
    --         embedImage $ project src $ mergeP (orthoP orthoEnd 0) faheyP t
    --       , grid $ mergeP (orthoP orthoEnd 0) faheyP t ]
    --     toEqui = animate $ \t ->
    --       scaleToSize screenWidth screenHeight $
    --       embedImage $ project src $ mergeP faheyP equirectangularP t
    -- play $ setDuration orthoOutT $ signalA (curveS 2) (toFahey `seqA` toEqui)
  where
    src = equirectangular
    waitT = 1
    morphT = 1
    orthoOutT = 3
    orthoStart = -halfPi
    orthoEnd = quartPi
    orthoRotT = 4
    quartPi = pi/4
    halfPi = pi/2

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
  withStrokeColorPixel (PixelRGBA8 0x50 0x50 0x50 0x0) $
  withFillOpacity 0 $
  mkGroup
  [ mkGroup []
  , withFillOpacity 0 $ mkGroup
    [ geometryToSVG p geo
    | geo <- landBorders
    ]
  , withStrokeColor "grey" $ mkGroup $ map mkLinePath $ latitudeLines p
  , withStrokeColor "grey" $ mkGroup $ map mkLinePath $ longitudeLines p
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
    latLines = 7
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
    lonLines = 7
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
