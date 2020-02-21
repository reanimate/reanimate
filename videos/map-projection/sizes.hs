#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE BangPatterns   #-}
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Control.Lens            ((^.), (%~), (&))
import           Control.Monad
import           Control.Monad.ST
import           Data.Aeson
import qualified Data.ByteString         as BS
import qualified Data.Sequence as Seq
import           Data.Char
import           Data.Foldable
import           Data.Geospatial         hiding (LonLat)
import           Data.LinearRing
import qualified Data.LineString         as Line
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.String.Here
import qualified Data.Text               as T
import           Graphics.SvgTree        (ElementRef (..), PathCommand (..),
                                          Tree (None))
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Blender
import           Reanimate.GeoProjection
import           Reanimate.Scene
import           Reanimate.Raster
import           System.IO.Unsafe


{-
  Show the earth.
  Rotate
-}

earth :: FilePath
-- earth = "earth-low.jpg"
-- earth = "earth-mid.jpg"
earth = "earth-high.jpg"
-- earth = "earth-extreme.jpg"

main :: IO ()
main = reanimate mainScene

testScene :: Animation
testScene = sceneAnimation $ do
    bg <- newSpriteSVG $ mkBackground "white"
    spriteZ bg (-1)
    -- play $ animate $ const $ scale (1/halfPi * (4/4.5)) $ scaleToSize screenWidth screenHeight $
    --   embedImage $ project src (orthoP $ LonLat 0 0)
    bend <- newVar 1
    let LonLat lam phi = newzealandLonLat
    rotX <- newVar 0
    rotY <- newVar 0
    draw <- newVar 0
    _ <- newSprite $ do
      getBend <- unVar bend
      getRotX <- unVar rotX
      getRotY <- unVar rotY
      getDraw <- unVar draw
      return $
        scale (fromToS 1 ((9*pi)/16) getBend) $
        mkGroup
        [ blender (script earth getBend getRotX getRotY)
        -- , lowerTransformations $ scale (1/halfPi * (4/4.5)) $
        --   grid $ orthoP $ LonLat (getRotY) (getRotX)
        ]
    -- wait 1
    -- tweenVar bend 1 $ \v -> fromToS v 0 . curveS 2
    fork $ tweenVar rotY 1 $ \v -> fromToS v (lam) . curveS 2
    tweenVar rotX 1 $ \v -> fromToS v (phi) . curveS 2
    -- wait 1
    fork $ tweenVar rotY 2 $ \v -> fromToS v 0 . curveS 2
    fork $ tweenVar rotX 2 $ \v -> fromToS v 0 . curveS 2
    wait 0.3
    tweenVar bend 2 $ \v -> fromToS v 0 . curveS 2
  where
    src = equirectangular

mainScene :: Animation
mainScene = seq equirectangular $ -- takeA 5 $ dropA 19 $
  mapA (withStrokeColor "black") $ sceneAnimation $ do
    bg <- newSpriteSVG $ mkBackground "white"
    spriteZ bg (-1)


    let offset = translate 0 (-screenHeight/2 * 0.25)
        orthoScale = 0.35
        largeScale = 0.50
    Globe{..} <- newGlobe

    morph <- newVar 0
    mapScale <- newVar orthoScale
    projs <- newVar (orthoP, equirectangularP)

    spriteModify globeSprite $ do
      s <- unVar mapScale
      pure $ \(svg, z) -> (scale s svg, z)
    spriteMap globeSprite (offset)
    -- destroySprite globeSprite

    let addRegion x y s proj lonlat@(LonLat lam phi) = do
          let idName = filter isAlphaNum $ show lonlat
              outline = lowerTransformations $ scale orthoScale $
                        proj (orthoP lonlat)
          region1 <- newSpriteSVG outline
          destroySprite region1

          spriteZ region1 2
          move <- newVar 0
          region1Shadow <- newSprite $ do
            ~(from, to) <- unVar projs
            m <- unVar morph
            relScale <- unVar mapScale

            t <- unVar move

            pure $
              let srcWidth = imageWidth equirectangularExtreme
                  srcHeight = imageHeight equirectangularExtreme
                  !subImg = convertRGBA8 $ rasterSized srcWidth srcHeight $ mkGroup
                        [ mkGroup []
                        , mkClipPath idName $
                          clipSvg
                        , withClipPathRef (Ref idName) $
                          scaleToSize screenWidth screenHeight $
                          embedImage $ project equirectangularExtreme equirectangularP]
                  setPos =
                    translate (fromToS 0 x $ curveS 2 t)
                    (fromToS 0 y $ curveS 2 t) .
                    offset .
                    scale (fromToS 1 s $ curveS 2 t)
                  posSvg =
                    lowerTransformations $ proj (mergeP (from lonlat) to m)
                  clipSvg = removeGroups $
                    lowerTransformations $ proj equirectangularP in
              mkGroup
              [ mkGroup []
              , setPos $ scale relScale $ centerWithDelta 1 posSvg $
                scaleToSize screenWidth screenHeight $
                embedImage $ interpP subImg (from lonlat) to m
              , lowerTransformations $ setPos $ scale orthoScale $ center $
                proj (orthoP lonlat)
              ]
          fork $ tweenVar move 1 $ \v -> fromToS v 1 . curveS 2


    tweenVar globePosition 2 $ \v -> fromToLonLat v usaLonLat . curveS 2
    fork $ addRegion (-5.5) 4 3 america usaLonLat

    tweenVar globePosition 2 $ \v -> fromToLonLat v brazilLonLat . curveS 2
    fork $ addRegion (-6) 0 3 brazil brazilLonLat

    tweenVar globePosition 2 $ \v -> fromToLonLat v ukLonLat . curveS 2
    fork $ addRegion (-1) 4 5 uk ukLonLat

    tweenVar globePosition 2 $ \v -> fromToLonLat v germanyLonLat . curveS 2
    fork $ addRegion 1 4 5 germany germanyLonLat

    tweenVar globePosition 2 $ \v -> fromToLonLat v ausLonLat . curveS 2
    fork $ addRegion 6 4 3 australia ausLonLat

    tweenVar globePosition 2 $ \v -> fromToLonLat v newzealandLonLat . curveS 2
    fork $ addRegion 6 0 4 newzealand newzealandLonLat

    fork $ tweenVar globePosition 3 $ \v -> fromToLonLat v (LonLat 0 0) . curveS 2
    wait 1
    fork $ tweenVar mapScale 2 $ \v -> fromToS v largeScale . curveS 2
    fork $ tweenVar morph 2 $ \v -> fromToS v 1 . curveS 2
    tweenVar globeBend 2 $ \v -> fromToS v 0 . curveS 2
    destroySprite globeSprite


    writeVar projs (const equirectangularP, equirectangularP)
    mapS <- newSprite $ do
      ~(from, to) <- unVar projs
      m <- unVar morph
      relScale <- unVar mapScale
      pure $ lowerTransformations $ scale relScale $ mkGroup
        [ mkGroup []
        , scaleToSize screenWidth screenHeight $
          embedImage $ interpP src (from (LonLat 0 0)) to m
        , grid $ mergeP (from (LonLat 0 0)) to m
        ]
    spriteMap mapS offset

    wait 1

    let push proj = do
          (_, prev) <- readVar projs
          writeVar projs (const $ prev, proj)
          writeVar morph 0
          tweenVar morph 1 $ \v -> fromToS v 1 . curveS 2
          wait 1

    push lambertP
    push mercatorP
    push mollweideP
    push hammerP
    push (bottomleyP $ 30/180*pi)
    push sinusoidalP
    push wernerP
    push (bonneP $ 45/180*pi)
    push augustP
    push collignonP
    push eckert1P
    push eckert3P
    push eckert5P
    push faheyP
    push foucautP
    push lagrangeP

  where
    src = equirectangular
    waitT = 2
    morphT = 2

centerDelta :: Double -> Tree -> Tree
centerDelta d t = translate ((-x-w/2)*d) ((-y-h/2)*d) t
  where
    (x, y, w, h) = boundingBox t

centerWithDelta :: Double -> Tree -> Tree -> Tree
centerWithDelta d orig t = translate ((-x-w/2)*d) ((-y-h/2)*d) t
  where
    (x, y, w, h) = boundingBox orig

centerXWithDelta :: Double -> Tree -> Tree -> Tree
centerXWithDelta d orig t = translate ((-x-w/2)*d) 0 t
  where
    (x, y, w, h) = boundingBox orig


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

equirectangular :: Image PixelRGBA8
equirectangular = unsafePerformIO $ do
  dat <- BS.readFile earth
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGBA8 img

equirectangularExtreme :: Image PixelRGBA8
equirectangularExtreme = unsafePerformIO $ do
  dat <- BS.readFile "earth-extreme.jpg"
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGBA8 img

usaLonLat = svgToLonLat americaE
ukLonLat = svgToLonLat ukE
germanyLonLat = svgToLonLat $ germany equirectangularP
newzealandLonLat = svgToLonLat $ newzealand equirectangularP
ausLonLat = svgToLonLat australiaE
brazilLonLat = svgToLonLat brazilE

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
      pathify $ countriesGeo annotate
    ]
  where
    annotate :: Map String Value -> SVG -> SVG
    annotate props svg = fromMaybe None (checker props svg)
    strokeWidth = defaultStrokeWidth * 0.3

countriesGeo :: (Map String Value -> SVG -> SVG) -> SVG
countriesGeo = loadFeatureCollection "countries-limited.json"

filterCountries :: IO ()
filterCountries = do
  mbGeo <- decodeFileStrict "countries.json"
  case mbGeo of
    Nothing  -> return ()
    Just geo -> do
      let geo' :: GeoFeatureCollection (Map String Value)
          geo' = geo & geofeatures %~ Seq.filter fn
          fn feat =
            case Map.lookup "NAME" (feat ^. properties) of
              Nothing -> False
              Just name -> name `elem` goodNames
      encodeFile "countries-limited.json" geo'
  where
    goodNames =
      [ "United States of America"
      , "United Kingdom"
      , "Germany"
      , "New Zealand"
      , "Australia"
      , "Brazil" ]

america :: Projection -> SVG
america p = fetchCountry p $ \props svg -> do
  "United States of America" <- Map.lookup "NAME" props
  return $ snd $ splitGlyphs [75] svg

americaE :: SVG
americaE = america equirectangularP

uk :: Projection -> SVG
uk p = fetchCountry p $ \props svg -> do
  name <- Map.lookup "NAME" props
  guard (name `elem` ["United Kingdom"])
  return svg

ukE :: SVG
ukE = uk equirectangularP

germany :: Projection -> SVG
germany p = fetchCountry p $ \props svg -> do
  "Germany" <- Map.lookup "NAME" props
  return svg

australia :: Projection -> SVG
australia p = fetchCountry p $ \props svg -> do
  "Australia" <- Map.lookup "NAME" props
  return $ snd $ splitGlyphs [0] svg

australiaE :: SVG
australiaE = australia equirectangularP

newzealand :: Projection -> SVG
newzealand p = fetchCountry p $ \props svg -> do
  "New Zealand" <- Map.lookup "NAME" props
  return $ snd $ splitGlyphs [0,1,2,3,4,5,6] svg

brazil :: Projection -> SVG
brazil p = fetchCountry p $ \props svg -> do
  "Brazil" <- Map.lookup "NAME" props
  return $ snd $ splitGlyphs [0] svg

brazilE :: SVG
brazilE = brazil equirectangularP

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
    pathify $ landGeo annotate
  , withStrokeColor "black" $
    applyProjection p $ pathify $
    gridLines 7 4
  ]
  where
    annotate :: Map String Value -> SVG -> SVG
    annotate props svg = svg
    strokeWidth = defaultStrokeWidth * 0.3

landGeo :: (Map String Value -> SVG -> SVG) -> SVG
landGeo = loadFeatureCollection "land.geojson"

gridLines :: Int -> Int -> SVG
gridLines latLines lonLines = mkGroup $ map mkLinePath $
    map longitudeLine (stepper (-halfPi) halfPi (lonLines+1)) ++
    map latitudeLine (stepper (-pi) pi (latLines))
  where
    segments = 100
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







data Globe s = Globe
  { globeSprite   :: Sprite s
  , globePosition :: Var s LonLat
  , globeBend     :: Var s Double
  }

newGlobe :: Scene s (Globe s)
newGlobe = do
  bend <- newVar 1
  pos <- newVar $ LonLat 0 0
  globe <- newSprite $ do
    getBend <- unVar bend
    ~(LonLat lam phi) <- unVar pos
    pure $
      scale (fromToS 1 ((9*pi)/16) getBend) $
      blender $ script earth getBend phi lam
  return $ Globe globe pos bend

script :: FilePath -> Double -> Double -> Double -> T.Text
script img bend rotX rotY = [iTrim|
import os
import math

import bpy

light = bpy.data.objects['Light']
bpy.ops.object.select_all(action='DESELECT')
light.select_set(True)
bpy.ops.object.delete()

cam = bpy.data.objects['Camera']
cam.data.type = 'ORTHO'
cam.data.ortho_scale = 16
cam.location = (0,0,5)
cam.rotation_euler = (0, 0, 0)
bpy.ops.object.empty_add(location=(0.0, 0, 0))
focus_target = bpy.context.object
bpy.ops.object.select_all(action='DESELECT')
cam.select_set(True)
focus_target.select_set(True)
bpy.ops.object.parent_set()

focus_target.rotation_euler = (${negate rotX}, 0, 0)


origin = bpy.data.objects['Cube']
bpy.ops.object.select_all(action='DESELECT')
origin.select_set(True)
bpy.ops.object.delete()

x = ${bend}
bpy.ops.mesh.primitive_plane_add()
plane = bpy.context.object
plane.scale = (16/2,${fromToS (9/2) 4 bend},1)
bpy.ops.object.shade_smooth()

bpy.context.object.active_material = bpy.data.materials['Material']
mat = bpy.context.object.active_material
image_node = mat.node_tree.nodes.new('ShaderNodeTexImage')
output = mat.node_tree.nodes['Material Output']
mat.node_tree.links.new(image_node.outputs['Color'], output.inputs['Surface'])

image_node.image = bpy.data.images.load('${T.pack img}')


modifier = plane.modifiers.new(name='Subsurf', type='SUBSURF')
modifier.levels = 7
modifier.render_levels = 7
modifier.subdivision_type = 'SIMPLE'

bpy.ops.object.empty_add(type='ARROWS',rotation=(math.pi/2,0,0))
empty = bpy.context.object

bendUp = plane.modifiers.new(name='Bend up', type='SIMPLE_DEFORM')
bendUp.deform_method = 'BEND'
bendUp.origin = empty
bendUp.deform_axis = 'X'
bendUp.factor = -math.pi*x

bendAround = plane.modifiers.new(name='Bend around', type='SIMPLE_DEFORM')
bendAround.deform_method = 'BEND'
bendAround.origin = empty
bendAround.deform_axis = 'Z'
bendAround.factor = -math.pi*2*x

bpy.context.view_layer.objects.active = plane
bpy.ops.object.modifier_apply(modifier='Subsurf')
bpy.ops.object.modifier_apply(modifier='Bend up')
bpy.ops.object.modifier_apply(modifier='Bend around')

bpy.ops.object.select_all(action='DESELECT')
plane.select_set(True);
#bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
bpy.ops.object.origin_clear()
bpy.ops.object.origin_set(type='GEOMETRY_ORIGIN')

plane.rotation_euler = (0, ${negate rotY}, 0)

scn = bpy.context.scene

#scn.render.engine = 'CYCLES'
#scn.render.resolution_percentage = 10

scn.view_settings.view_transform = 'Standard'

scn.render.film_transparent = True

bpy.ops.render.render( write_still=True )
|]
