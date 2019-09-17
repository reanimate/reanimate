{-# LANGUAGE OverloadedStrings #-}
module Colorspace (colorSpacesScene) where

import           Control.Lens               ((&), (.~))

import qualified Data.Map as Map
import           Codec.Picture
import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB
import           Data.Colour
import           Data.Colour.SRGB.Linear
import           Graphics.SvgTree
import           Linear.V2
import           Reanimate.ColorMap
import           Reanimate.ColorSpace
import           Reanimate.Driver           (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Raster
import           Reanimate.Signal
import           Reanimate.Svg
import qualified Reanimate.Builtin.TernaryPlot as Ternary

labScaleX = 128
labScaleY = 128

screenScale = 100

colorSpacesScene :: Animation
colorSpacesScene = mkAnimation 2 $ do
    s <- getSignal signalLinear
    let cm = hsv
    emit $ mkGroup
      [ mkClipPath "sRGB"
        [ simplify
          sRGBTriangle
        ]
      , mkClipPath "visible"
        [ simplify
          obsColors
        ]
      ]
    let centerPic = translate (-screenScale/2) (-screenScale/2)
    emit $ mkGroup
      [ mkBackground "black"
      -- , translate (-60) 0 $ mkGroup
      --   [ -- withClipPathRef (Ref "sRGB") $
      --     center $
      --     scaleToWidth 100 $ img1
      --   ]
      , translate (-60) 10 $ mkGroup
        [ -- withClipPathRef (Ref "sRGB") $
          --withClipPathRef (Ref "visible") $
          mkGroup [img1]
        , withStrokeColor "white" $
          sRGBTriangle
        , withStrokeColor "white" $
          obsColors
        ]
      , translate (60) 10 $ mkGroup
        [ -- withClipPathRef (Ref "sRGB") $
          withClipPathRef (Ref "sRGB") $
          mkGroup [img1]
        -- , withFillOpacity 0 $
        --   withStrokeColor "white" $
        --   mkCircle (Num $ Ternary.radius * 100)
        -- , let (zx,zy) = Ternary.toCartesianCoords (1/3) (1/3)
        --       (tx,ty) = Ternary.toCartesianCoords 0 0
        --   in
        --   withFillOpacity 0 $
        --   withStrokeColor "white" $
        --   scaleXY 1 (-1) $
        --   lowerTransformations $
        --   -- scale (100) $
        --   translate ((tx-zx)*100) ((ty-zy)*100) $
        --   pathify $
        --   mkCircle (Num $ 3)
        -- ,
        --   withStrokeColor "white" $
        --   obsColors
        ]
      -- , translate (-60) 0 $ center $ mkGroup
      --   [ center $ scaleToSize 100 100 $ cieXYImageOld 100 100
      --   -- , withStrokeColor "white" $ labColors
      --   , lowerTransformations $
      --     withStrokeColor "white" $
      --     scale 100 $
      --     renderXYZCoordinates
      --   ]
      -- , translate (60) 0 $ scale 0.2 img2
      -- , translate 60 0 $ center $ renderSensitivity
      ]
  where
    imgSize :: Num a => a
    imgSize = 1000
    img1 = cieXYImage imgSize
    img2 = cieLABImage imgSize imgSize
    obsColors =
      lowerTransformations $
      scaleXY 1 (-1) $
      scale (100) $
      renderXYZCoordinatesTernary
    labColors =
      lowerTransformations $
      scale (100/2) $
      renderLABCoordinates



renderXYZCoordinatesTernary :: Tree
renderXYZCoordinatesTernary =
  withFillOpacity 0 $
  mkLinePath $
  [ (x, y)
  | (_nm, (red,green,blue)) <- Map.toList lightXYZCoordinates
  , let (x,y) = Ternary.toOffsetCartesianCoords green red
  ]


cieXYImageOld :: Int -> Int -> Tree
cieXYImageOld width height = embedImage $ generateImage gen width height
  where
    gen x y =
      let
          x' = (fromIntegral x / fromIntegral width)
          y' = 1-(fromIntegral y / fromIntegral height)
          z' = 1 - x' - y'
          RGB r g b = toSRGBBounded (cieXYZ x' y' z')
          -- RGB r g b = toSRGBBounded (cieXYZ xPrim yPrim zPrim)
      in if x' + y' > 1
        then PixelRGB8 0 0 0
        else PixelRGB8 r g b
{-
cieXYImage_ :: Int -> Tree
cieXYImage_ width = embedImage $ generateImage gen width height
  where
    height = round $ fromIntegral width * sin (60/180*pi)
    gen x y =
      let
          x' = (fromIntegral x / fromIntegral width)
          y' = (fromIntegral y / fromIntegral width)
          z' = 1 - x' - y'
          aCoord = (x'*2-bCoord)/2
          bCoord = y' / (sqrt 3 / 2)
          cCoord = 1 - aCoord - bCoord
          -- RGB r g b = toSRGBBounded (cieXYZ x' y' z')
          RGB r g b = toSRGBBounded (cieXYZ aCoord bCoord cCoord)
      in if aCoord + bCoord > 1 || aCoord < 0 || bCoord < 0 || cCoord < 0
        then PixelRGBA8 0 0 0 0
        else PixelRGBA8 r g b 0xFF
-}
cieXYImage :: Int -> Tree
cieXYImage density = Ternary.raster density $ \aCoord bCoord cCoord ->
    let RGB r g b = toSRGBBounded (cieXYZ aCoord bCoord cCoord)
    in PixelRGBA8 r g b 0xFF



cieLABImage :: Int -> Int -> Tree
cieLABImage width height = embedImage $ generateImage gen width height
  where
    gen x y =
      let
          aStar = (fromIntegral x / fromIntegral width) * labScaleX*2 - labScaleX
          bStar = (1-(fromIntegral y / fromIntegral height)) * labScaleY*2 - labScaleY
          lStar = findLStar aStar bStar
          RGB r g b = toSRGBBounded (cieLAB d65 lStar aStar bStar)
          -- RGB r g b = RGB (round $ lStar/100 * 255) (round $ lStar/100 * 255) (round $ lStar/100 * 255)
      in PixelRGB8 r g b

findLStar :: Double -> Double -> Double
findLStar aStar bStar = worker 0 100 10
  where
    -- worker minL maxL n | trace (show (minL, maxL, n)) False = undefined
    worker minL maxL 0 = minL + (maxL-minL)/2
    worker minL maxL n
      | total > 1 = worker minL thisL (n-1)
      | otherwise = worker thisL maxL (n-1)
      where
        thisL = minL + (maxL-minL)/2
        (x,y,z) = cieXYZView (cieLAB d65 thisL aStar bStar)
        total = x+y+z

colorMapToLABCoords :: (Double -> PixelRGB8) -> Tree
colorMapToLABCoords colorMap = withFillOpacity 0 $ mkLinePath
    [ (aStar, 1-bStar)
    | n <- [0 .. steps]
    , let PixelRGB8 r g b = colorMap (n/steps)
          (lStar, aStar, bStar) = cieLABView d65 (sRGB24 r g b)
    ]
  where
    steps = 100

colorMapToXYZCoords :: (Double -> PixelRGB8) -> Tree
colorMapToXYZCoords colorMap = withFillOpacity 0 $ mkLinePath
    [ (x/s, 1-(y/s))
    | n <- [0 .. steps]
    , let PixelRGB8 r g b = colorMap (n/steps)
          (x, y, z) = cieXYZView (sRGB24 r g b)
          s = x+y+z
    ]
  where
    steps = 100

sRGBTriangle :: Tree
sRGBTriangle =
    lowerTransformations $
    scaleXY 1 (-1) $
    scale 100 $
    withFillOpacity 0 $
    mkClosedLinePath
    [ Ternary.toOffsetCartesianCoords rY rX
    , Ternary.toOffsetCartesianCoords gY gX
    , Ternary.toOffsetCartesianCoords bY bX ]
  where
    RGB r g b = primaries sRGBGamut
    (rX, rY, _) = chromaCoords $ chromaConvert r
    (gX, gY, _) = chromaCoords $ chromaConvert g
    (bX, bY, _) = chromaCoords $ chromaConvert b

mkClosedLinePath :: [(Double, Double)] -> Tree
mkClosedLinePath [] = mkGroup []
mkClosedLinePath ((startX, startY):rest) =
    PathTree $ defaultSvg & pathDefinition .~ cmds
  where
    cmds = [ MoveTo OriginAbsolute [V2 startX startY]
           , LineTo OriginAbsolute [ V2 x y | (x, y) <- rest ]
           , EndPath ]
