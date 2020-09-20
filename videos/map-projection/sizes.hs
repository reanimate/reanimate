#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Main(main) where

import           Codec.Picture
import           Control.Lens            ((%~), (&), (^.))
import           Control.Monad           (guard)
import           Data.Aeson              (Value, decodeFileStrict, encodeFile)
import qualified Data.ByteString         as BS
import           Data.Char               (isAlphaNum)
import           Data.Geospatial         hiding (LonLat)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import qualified Data.Sequence           as Seq
import           Data.String.Here        (iTrim)
import qualified Data.Text               as T
import           Graphics.SvgTree        (ElementRef (..), Tree (None))
import           Reanimate
import           Reanimate.Builtin.Flip  (flipTransition)
import           Reanimate.GeoProjection
import           Reanimate.Raster        (cacheImage, mkImage, rasterSized)
import           Reanimate.Transition    (overlapT, signalT)
import           System.IO.Unsafe        (unsafePerformIO)

import           EndScene                (endScene)

earth :: FilePath
-- earth = "earth-low.jpg"
-- earth = "earth-mid.jpg"
-- earth = "earth-high.jpg"
-- earth = "earth-extreme.jpg"
earth = "earth-1440.png"

earthMax :: FilePath
earthMax = "earth-max.jpg"

main :: IO ()
main = reanimate $
    parA (staticFrame 1 $ mkBackground "darkgrey") $
    overlapT 2 (signalT (curveS 2) flipTransition)
      mainScene
      endScene

mainScene :: Animation
mainScene = seq equirectangular $ -- takeA 10 $ dropA 21 $
  mapA (withStrokeColor "black") $ sceneAnimation $ do
    bg <- newSpriteSVG $ mkBackground "white"
    spriteZ bg (-1)


    let offset = translate 0 (-screenHeight/2 * 0.20)
        orthoScale = 0.50
        largeScale = 0.75
    Globe{..} <- newGlobe

    morph <- newVar 0
    mapScale <- newVar orthoScale
    projs <- newVar (orthoP, equirectangularP)

    spriteMap globeSprite (offset . scale orthoScale)

    let addRegion x y s proj lonlat label = do
          let idName = filter isAlphaNum $ show lonlat
              srcWidth = imageWidth equirectangularMax
              srcHeight = imageHeight equirectangularMax
              subImg =
                convertRGBA8 $ rasterSized srcWidth srcHeight $ mkGroup
                    [ mkGroup []
                    , mkClipPath idName $
                      clipSvg
                    , withClipPathRef (Ref idName) $
                      scaleToSize screenWidth screenHeight $
                      embedImage equirectangularMax]
              clipSvg = removeGroups $
                lowerTransformations $ proj equirectangularP

          move <- newVar 0
          _ <- newSprite $ do
            ~(from, to) <- unVar projs
            m <- unVar morph
            relScale <- unVar mapScale

            t <- unVar move

            pure $
              let
                  (bx,by,bw,bh) = boundingBox $ proj (from lonlat)
                  fx = (bx+screenWidth/2)/screenWidth
                  fy = (by+screenHeight/2)/screenHeight
                  fw = bw/screenWidth
                  fh = bh/screenHeight
                  (bx',by',bw',bh') = boundingBox $ proj to
                  fx' = (bx'+screenWidth/2)/screenWidth
                  fy' = (by'+screenHeight/2)/screenHeight
                  fw' = bw'/screenWidth
                  fh' = bh'/screenHeight
                  imgKey = (projectionLabel (from lonlat), projectionLabel to, lonlat, earthMax, m)
                  imgFile = cacheImage imgKey $
                    interpBBP subImg (from lonlat) to (fx,fy,fw,fh) (fx',fy',fw',fh') m
                  setPos =
                    translate (fromToS 0 x $ curveS 2 t)
                    (fromToS 0 y $ curveS 2 t) .
                    offset .
                    scale (fromToS 1 s $ curveS 2 t)
                  posSvg =
                    lowerTransformations $ proj (mergeP (from lonlat) to m)
                  finalSvg =
                    lowerTransformations $ proj to
                  toCenter = if projectionLabel (from lonlat) == "ortho"
                    then centerWithDelta m finalSvg
                    else centerWithDelta 1 posSvg
                  -- targetXY
                  -- translate ((-x-w/2)*d) ((-y-h/2)*d) t
                  in
              mkGroup
              [ mkGroup []
              , setPos $ scale relScale $ toCenter $
                mkImage screenWidth screenHeight imgFile
              , withStrokeWidth (fromToS 0 (defaultStrokeWidth*0.3) t) $
                lowerTransformations $ setPos $ scale orthoScale $
                  proj (orthoP lonlat)
              ]
          -- destroySprite region1Shadow
          fork $ tweenVar move 1 $ \v -> fromToS v 1 . curveS 2
          fork $ play $ (staticFrame 2 $
            withStrokeWidth 0 $
            translate 0 (-screenHeight*0.40) $
            center $ latex label)
            # applyE (overBeginning 0.2 fadeInE)
            # applyE (overEnding 0.2 fadeOutE)



    tweenVar globePosition 2 $ \v -> fromToLonLat v usaLonLat . curveS 2
    fork $ addRegion (-5.5) 3.5 2 america usaLonLat "USA"

    tweenVar globePosition 2 $ \v -> fromToLonLat v brazilLonLat . curveS 2
    fork $ addRegion (-6) (-0.5) 2 brazil brazilLonLat "Brazil"

    tweenVar globePosition 2 $ \v -> fromToLonLat v ukLonLat . curveS 2
    fork $ addRegion (-1) 4 4 uk ukLonLat "UK, scaled 200\\%"

    tweenVar globePosition 2 $ \v -> fromToLonLat v germanyLonLat . curveS 2
    fork $ addRegion 1 4 4 germany germanyLonLat "Germany, scaled 200\\%"

    tweenVar globePosition 2 $ \v -> fromToLonLat v ausLonLat . curveS 2
    fork $ addRegion 6 3.5 2 australia ausLonLat "Australia"

    tweenVar globePosition 2 $ \v -> fromToLonLat v newzealandLonLat . curveS 2
    fork $ addRegion 6 (-0.5) 3 newzealand newzealandLonLat "New Zealand, scaled 150\\%"

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
      -- relScale <- unVar mapScale
      t <- spriteT
      pure $
        let imgKey = (projectionLabel (from (LonLat 0 0)), projectionLabel to, earth, m
                     ,"global"::String)
            imgFile = cacheImage imgKey $
              interpP src (from (LonLat 0 0)) to m
        in lowerTransformations $ scale orthoScale $ mkGroup
          [ mkGroup []
          , mkImage screenWidth screenHeight imgFile
          , withStrokeWidth (fromToS 0 (defaultStrokeWidth*0.2) $ min t 1) $
            grid $ mergeP (from (LonLat 0 0)) to m
          ]
    spriteMap mapS offset

    play $ (staticFrame (projMorphT+projWaitT) $
      withStrokeWidth 0 $
      translate 0 (-screenHeight*0.43) $
      center $ latex "Equirectangular")
      # applyE (overBeginning 0.2 fadeInE)
      # applyE (overEnding 0.2 fadeOutE)

    let push proj label = do
          fork $ play $ (staticFrame (projMorphT+projWaitT) $
            withStrokeWidth 0 $
            translate 0 (-screenHeight*0.43) $
            center $ latex label)
            # applyE (overBeginning 0.2 fadeInE)
            # applyE (overEnding 0.2 fadeOutE)
          (_, prev) <- readVar projs
          writeVar projs (const $ prev, proj)
          writeVar morph 0
          tweenVar morph projMorphT $ \v -> fromToS v 1 . curveS 2
          wait projWaitT

    push lambertP "Lambert"
    push mercatorP "Mercator"
    push mollweideP "Mollweide"
    push hammerP "Hammer"
    push (bottomleyP $ 30/180*pi) "Bottomley"
    push sinusoidalP "Sinusoidal"
    push wernerP "Werner"
    push (bonneP $ 45/180*pi) "Bonne"
    push augustP "August"
    push collignonP "Collignon"
    push eckert1P "Eckert 1"
    push eckert3P "Eckert 3"
    push eckert5P "Eckert 5"
    push faheyP "Fahey"
    push foucautP "Foucaut"
    push lagrangeP "Lagrange"

    wait 5
  where
    src = equirectangular
    projMorphT = 2
    projWaitT = 3

centerWithDelta :: Double -> Tree -> Tree -> Tree
centerWithDelta d orig t = translate ((-x-w/2)*d) ((-y-h/2)*d) t
  where
    (x, y, w, h) = boundingBox orig

equirectangular :: Image PixelRGBA8
equirectangular = unsafePerformIO $ do
  dat <- BS.readFile earth
  case decodeImage dat of
    Left err  -> error err
    Right img -> return $ convertRGBA8 img

equirectangularMax :: Image PixelRGBA8
equirectangularMax = unsafePerformIO $ do
  dat <- BS.readFile earthMax
  case decodeImage dat of
    Left err  -> error err
    Right img -> return $ convertRGBA8 img

usaLonLat, ukLonLat, germanyLonLat, newzealandLonLat, ausLonLat, brazilLonLat ::
  LonLat

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

fromToLonLat :: LonLat -> LonLat -> Double -> LonLat
fromToLonLat (LonLat lam1 phi1) (LonLat lam2 phi2) t =
  LonLat (fromToS lam1 lam2 t) (fromToS phi1 phi2 t)

fetchCountry :: Projection -> (Map String Value -> SVG -> Maybe SVG) -> SVG
fetchCountry p checker =
    lowerTransformations $
    scaleXY
      screenWidth
      screenHeight
     $
    translate (-1/2) (-1/2) $

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

countriesGeo :: (Map String Value -> SVG -> SVG) -> SVG
countriesGeo = loadFeatureCollection "countries-limited.json"

_filterCountries :: IO ()
_filterCountries = do
  mbGeo <- decodeFileStrict "countries.json"
  case mbGeo of
    Nothing  -> return ()
    Just geo -> do
      let geo' :: GeoFeatureCollection (Map String Value)
          geo' = geo & geofeatures %~ Seq.filter fn
          fn feat =
            case Map.lookup "NAME" (feat ^. properties) of
              Nothing   -> False
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
    screenWidth
    screenHeight
   $
  translate (-1/2) (-1/2) $

  withFillOpacity 0 $
  mkGroup
  [ mkGroup []
  , withStrokeColor "grey" $
    applyProjection p $
    svgPointsToRadians $
    pathify $ landGeo annotate
  , withStrokeColor "black" $
    applyProjection p $ pathify $
    gridLines 7 4
  ]
  where
    annotate :: Map String Value -> SVG -> SVG
    annotate _ svg = svg

landGeo :: (Map String Value -> SVG -> SVG) -> SVG
landGeo = loadFeatureCollection "land.geojson"

gridLines :: Int -> Int -> SVG
gridLines latLines lonLines = mkGroup $ map mkLinePath $
    map longitudeLine (stepper (-halfPi) halfPi (lonLines+1)) ++
    map latitudeLine (stepper (-pi) pi latLines)
  where
    segments = 100
    stepper from to nMax =
      [ fromToS from to (fromIntegral n / fromIntegral nMax)
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
      mkImage screenWidth screenHeight $
      blender' $ script earthMax getBend phi lam
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

focus_target.rotation_euler = (${negate rotX}, ${rotY}, 0)


origin = bpy.data.objects['Cube']
bpy.ops.object.select_all(action='DESELECT')
origin.select_set(True)
bpy.ops.object.delete()

x = ${bend}
bpy.ops.mesh.primitive_plane_add()
plane = bpy.context.object
plane.scale = (16/2,${fromToS (9/2) 4 bend},1)
#bpy.ops.object.shade_smooth()

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

#plane.rotation_euler = (${negate rotX}, ${negate rotY}, 0)

scn = bpy.context.scene

#scn.render.engine = 'CYCLES'
#scn.render.resolution_percentage = 10

scn.view_settings.view_transform = 'Standard'

scn.render.film_transparent = True
scn.render.resolution_x = ${pWidth} #3200
scn.render.resolution_y = ${pHeight} #1800

bpy.ops.render.render( write_still=True )
|]
