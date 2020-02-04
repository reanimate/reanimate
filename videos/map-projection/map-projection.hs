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
    play $ setDuration 1 $ animate $ \t ->
      scaleToSize screenWidth screenHeight $
      embedImage (bottomley $ fromToS (toRads 1) (toRads 30) $ curveS 2 t) -- (mercator t)

equirectangular :: Image PixelRGB8
equirectangular = unsafePerformIO $ do
  dat <- BS.readFile "earth.jpg"
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGB8 img

mercator :: Double -> Image PixelRGB8
mercator t =
    generateImage fn w h
  where
    w = imageWidth equirectangular
    h = imageHeight equirectangular
    fn xPx yPx =
      let -- x = fromToS (-pi) pi (fromIntegral xPx / fromIntegral (w-1))
          y = fromToS (-pi) (pi) (fromIntegral yPx / fromIntegral (h-1))
          orig_y = (fromToS 0 (fromIntegral h) ((atan (sinh y) + (pi/2)) / pi))
          mid = fromToS (fromIntegral yPx) orig_y t
      in pixelAt equirectangular xPx (round mid)

{- Mollweide
phi = latitude
lamba = longitude

x = 2sqrt(2)/pi lambda * cos theta
y = sqrt 2 * sin theta

x [-2sqrt(2), +2sqrt(2)]
y [-sqrt(2), +sqrt(2)]

-}
mollweide :: Double -> Image PixelRGB8
mollweide t =
    generateImage fn w h
  where
    w = imageWidth equirectangular
    h = imageHeight equirectangular
    fn xPx yPx = -- trace (show (xPx, yPx, lam, phi, x_rect, y_rect)) $
        if x_new < 0 || x_new >= w || y_new < 0 || y_new >= h -- lam < -pi || lam > pi
          then PixelRGB8 0 0 0
          else pixelAt equirectangular x_new y_new
      where
        x = fromToS (-2*sqrt(2)) (2*sqrt(2)) (fromIntegral xPx / fromIntegral (w-1))
        y = fromToS (-sqrt 2) (sqrt 2) (fromIntegral yPx / fromIntegral (h-1))
        theta = asin (y/sqrt(2))
        lam = (pi*x/(2*sqrt(2)*cos theta))
        phi = asin ((2*theta+sin(2*theta))/pi)
        x_rect = (((lam + pi) / (2*pi))*fromIntegral w)
        y_rect = (((phi + pi/2) / (pi))*fromIntegral h)
        x_new = round (fromToS (fromIntegral xPx) x_rect t)
        y_new = round (fromToS (fromIntegral yPx) y_rect t)
    -- lam = fromToS (-pi) pi (fromIntegral xPx / fromIntegral (w-1))
    -- phi = fromToS (-pi/2) (pi/2) (fromIntegral yPx / fromIntegral (h-1))
    -- theta = theta_gen phi 10
    -- x_rect = (2*sqrt(2))/pi * lam * cos theta
    -- y_rect = sqrt(2)*sin theta

theta_gen phi | phi == pi/2 = const (pi/2)
theta_gen phi | phi == -pi/2 = const (-pi/2)
theta_gen phi = worker
  where
    worker 0 = phi
    worker n =
      let sub = worker (n-1) in
      sub - (2*sub + sin (2*sub) - pi*sin phi) / (2+2*cos (2*sub))


{- Lambert
x = lam
y = sin phi
-}

lambert :: Double -> Image PixelRGB8
lambert t =
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
        y = fromToS (-1) 1 (fromIntegral yPx / fromIntegral (h-1))
        lam = x
        phi = asin y
        x_rect = (((lam + pi) / (2*pi))*fromIntegral w)
        y_rect = (((phi + pi/2) / (pi))*fromIntegral h)
        x_new = round (fromToS (fromIntegral xPx) x_rect t)
        y_new = round (fromToS (fromIntegral yPx) y_rect t)

cot = recip . tan
toRads dec = dec/180 * pi

{- Bottomley
-}

bottomley :: Double -> Image PixelRGB8
bottomley phi_1 =
    generateImage fn w h
  where
    w = imageWidth equirectangular
    h = imageHeight equirectangular
    fn xPx yPx = -- trace (show (xPx, yPx, lam, phi, x_rect, y_rect)) $
        if x_new < 0 || x_new >= w || y_new < 0 || y_new >= h -- lam < -pi || lam > pi
          then PixelRGB8 0 0 0
          else pixelAt equirectangular x_new y_new
      where
        rho = sqrt ((x * sin phi_1)**2 + (pi/2 - y)**2)
        e = atan2 (x*sin phi_1) (pi/2 - y)
        x = fromToS (-pi) (pi) (fromIntegral xPx / fromIntegral (w-1))
        y = fromToS (pi/2) (-pi/2) (fromIntegral yPx / fromIntegral (h-1))
        lam = (e*rho)/(sin phi_1*sin rho)
        phi = pi/2 - rho
        x_rect = (((lam + pi) / (2*pi))*fromIntegral w)
        y_rect = ((1-(phi + pi/2) / (pi))*fromIntegral h)
        x_new = round (fromToS (fromIntegral xPx) x_rect 1)
        y_new = round (fromToS (fromIntegral yPx) y_rect 1)

bottomley_toSphere phi_1 x y = (lam, phi)
  where
    rho = sqrt ((x * sin phi_1)**2 + (pi/2 - y)**2)
    e = atan2 (x*sin phi_1) (pi/2 - y)
    lam = (e*rho)/(sin phi_1*sin rho)
    phi = pi/2 - rho

bottomley_coords phi_1 lam phi = (x,y)
  where
    rho = pi/2 - phi
    e = if rho == 0 then 0 else (lam * sin phi_1 * sin rho) / rho
    x = (rho * sin e) / sin phi_1
    y = pi/2 - rho * cos e


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
