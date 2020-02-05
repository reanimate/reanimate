#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Jpg
import qualified Data.ByteString   as BS
import           Reanimate
import           System.IO.Unsafe
import Debug.Trace

main :: IO ()
main = seq equirectangular $ reanimate $ playThenReverseA $ sceneAnimation $ do
    play $ setDuration 5 $ animate $ \t ->
      scaleToSize screenWidth screenHeight $
      --embedImage (bottomley $ fromToS (toRads 1) (toRads 30) $ curveS 2 t) -- (mercator t)
      --embedImage (mollweide $ curveS 2 t)
      --embedImage (mercator 1)
      --embedImage (project mollweideP)
      -- embedImage (project $ mergeP mollweideP (bottomleyP (toRads 30)) $ curveS 2 t)
      --embedImage (project $ bottomleyP $ fromToS (toRads 0.1) (toRads 30) $ curveS 2 t)
      --embedImage (project $ mergeP mollweideP lambertP $ curveS 2 t)
      --embedImage (project $ mergeP mollweideP sinusoidalP $ curveS 2 t)
      embedImage (project $ mergeP equirectangularP mollweideP $ curveS 2 t)

equirectangular :: Image PixelRGB8
equirectangular = unsafePerformIO $ do
  dat <- BS.readFile "earth.jpg"
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGB8 img

-- x y -> (lon, lat)
-- [0,1] [0,1] -> [-pi,+pi] [-pi/2, +pi/2]
type Projection = Double -> Double -> (Double, Double)

project :: Projection -> Image PixelRGB8
project p = generateImage fn w h
  where
    w = imageWidth equirectangular
    h = imageHeight equirectangular
    fn xPx yPx =
      let x = (fromIntegral xPx / fromIntegral (w-1))
          y = (fromIntegral yPx / fromIntegral (h-1))
          (lon, lat) = p x y
          x_e = round $ fromToS 0 (fromIntegral w-1) ((lon+pi) / (2*pi))
          y_e = round $ fromToS 0 (fromIntegral h-1) ((lat+pi/2) / pi)
      in
      if x_e < 0 || x_e >= w || y_e < 0 || y_e >= h
        then PixelRGB8 0 0 0
        else pixelAt equirectangular x_e y_e

mergeP :: Projection -> Projection -> Double -> Projection
mergeP p1 p2 t = \x y ->
    let (lon1, lat1) = p1 x y
        (lon2, lat2) = p2 x y
    in if oob lon1 lat1 && oob lon2 lat2
      then (100, 100)
      else (fromToS lon1 lon2 t, fromToS lat1 lat2 t)
  where
    oob lon lat = lon < (-pi) || lon > pi || lat < (-pi/2) || lat > pi/2

equirectangularP :: Projection
equirectangularP x y = (xPi, yPi)
  where
    xPi = fromToS (-pi) pi x
    yPi = fromToS (-pi/2) (pi/2) y

mercatorP :: Projection
mercatorP x y = (xPi, atan (sinh yPi))
  where
    xPi = fromToS (-pi) pi x
    yPi = fromToS (-pi) pi y

{- Mollweide
phi = latitude
lamba = longitude

x = 2sqrt(2)/pi lambda * cos theta
y = sqrt 2 * sin theta

x [-2sqrt(2), +2sqrt(2)]
y [-sqrt(2), +sqrt(2)]

-}
mollweideP :: Projection
mollweideP x' y' = (lam, phi)
  where
    x = fromToS (-2*sqrt(2)) (2*sqrt(2)) x'
    y = fromToS (-sqrt 2) (sqrt 2) y'
    theta = asin (y/sqrt(2))
    lam = pi*x/(2*sqrt(2)*cos theta)
    phi = asin ((2*theta+sin(2*theta))/pi)


{- Lambert
x = lam
y = sin phi
-}

lambertP :: Projection
lambertP x' y' = (x, asin y)
  where
    x = fromToS (-pi) (pi) x'
    y = fromToS (-1) 1 y'

toRads dec = dec/180 * pi

{- Bottomley
-}

bottomleyP :: Double -> Projection
bottomleyP phi_1 x' y' = (lam, -phi)
  where
    x = fromToS (-pi) (pi) x'
    y = fromToS (pi/2) (-pi/2) y'
    rho = sqrt ((x * sin phi_1)**2 + (pi/2 - y)**2)
    e = atan2 (x*sin phi_1) (pi/2 - y)
    lam = (e*rho)/(sin phi_1*sin rho)
    phi = pi/2 - rho

sinusoidalP :: Projection
sinusoidalP x' y' = (x/cos y, y)
  where
    x = fromToS (-pi) (pi) x'
    y = fromToS (-pi/2) (pi/2) y'

orthoP :: Double -> Double -> Projection
orthoP lam_0 phi_0 x' y' = (lam, phi)
  where
    x = fromToS (-16/9) (16/9) x'
    y = fromToS (-1) (1) y'
    lam = wrap (-pi) (pi) $
      lam_0 + atan2 (x * sin c) (rho * cos c * cos phi_0 - y * sin c * sin phi_0)
    phi = wrap (-pi/2) (pi/2) $
      asin ((cos c * sin phi_0 + y * sin c * cos phi_0)/rho)
    rho = sqrt (x**2 + y**2)
    c = asin rho
    wrap lower upper v
      | v > upper = v-upper+lower
      | v < lower = v+upper-lower
      | otherwise = v

{- Cassini
phi = asin (sin y * cos x)
lam = atan2 (tan x) (cos y)
-}

cassini :: Double -> Image PixelRGB8
cassini t =
    generateImage fn w h
  where
    w = imageWidth equirectangular
    h = imageHeight equirectangular
    fn xPx yPx = -- trace (show (xPx, yPx, lam, phi, x_rect, y_rect)) $
        if x_new < 0 || x_new >= w || y_new < 0 || y_new >= h -- lam < -pi || lam > pi
          then PixelRGB8 0 0 0
          else pixelAt equirectangular x_new y_new
      where
        x = fromToS (-pi) (pi) (fromIntegral xPx / fromIntegral (w-1))
        y = fromToS (-pi/2) (pi/2) (fromIntegral yPx / fromIntegral (h-1))
        lam = atan2 (tan x) (cos y)
        phi = asin (sin y * cos x)
        x_rect = (((lam + pi) / (2*pi))*fromIntegral w)
        y_rect = (((phi + pi/2) / (pi))*fromIntegral h)
        x_new = round (fromToS (fromIntegral xPx) x_rect t)
        y_new = round (fromToS (fromIntegral yPx) y_rect t)
