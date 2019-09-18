module Reanimate.Builtin.TernaryPlot where

import           Codec.Picture
import           Graphics.SvgTree (Tree)
import           Reanimate.Raster
import           Reanimate.Svg

-- (top, left, right)
-- a+b+c=1
type ACoord = Double
type BCoord = Double
type CCoord = Double

-- Creates a centered ternary plot with a width of 5.
raster :: Int -> (ACoord -> BCoord -> CCoord -> PixelRGBA8) -> Tree
raster density fn =
    translate (-cX*stdWidth) (-cY*stdWidth) $
    scaleToWidth stdWidth $
    embedImage $ generateImage gen density height
  where
    stdWidth = 5
    (cX, cY) = toCartesianCoords (1/3) (1/3)
    height = round (fromIntegral density * (sqrt 3 / 2) :: Double)
    gen x y =
      let
          x' = (fromIntegral x / fromIntegral density)
          y' = (fromIntegral y / fromIntegral density)
          aCoord = (x'*2-bCoord)/2
          bCoord = y' / (sqrt 3 / 2)
          cCoord = 1 - aCoord - bCoord
      in if aCoord + bCoord > 1 || aCoord < 0 || bCoord < 0 || cCoord < 0
        then PixelRGBA8 0 0 0 0
        else fn aCoord bCoord cCoord

atCenter :: Double -> Tree -> Tree
atCenter stdWidth = translate (-cX*stdWidth) (-cY*stdWidth)
  where
    (cX, cY) = toCartesianCoords (1/3) (1/3)

radius :: Double
radius = sqrt (cX*cX + cY*cY)
  where
    (cX, cY) = toCartesianCoords (1/3) (1/3)

toCartesianCoords :: ACoord -> BCoord -> (Double, Double)
toCartesianCoords a b = (x, y)
  where
    x = (a+2*b)/2
    y = (sqrt 3 / 2) * a

toOffsetCartesianCoords :: ACoord -> BCoord -> (Double, Double)
toOffsetCartesianCoords a b =
    (tx-zx, ty-zy)
  where
    (zx,zy) = toCartesianCoords (1/3) (1/3)
    (tx,ty) = toCartesianCoords a b

fromCartesianCoords :: Double -> Double -> (ACoord, BCoord, CCoord)
fromCartesianCoords x y = (a,b,1-a-b)
  where
    a = (x*2-b)/2
    b = y / (sqrt 3 / 2)
