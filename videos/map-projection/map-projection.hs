#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE MultiWayIf #-}
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Jpg
import           Control.Monad.ST
import qualified Data.ByteString     as BS
-- import           Data.Vector         ()
-- import           Data.Vector.Unboxed
import           Debug.Trace
import           Reanimate
import           System.IO.Unsafe

{-
Realized projection: UVector Int
(x,y) -> (lam, phi)



-}

{-
  1. equirectangular
  2. lambert
  3. mercator
  4. mollweide
  5. hammer
  6. sinusoidal
-}
main :: IO ()
main = seq equirectangular $ reanimate $ playThenReverseA $ sceneAnimation $ do
    -- play $ pauseAtEnd 1 $ setDuration 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage (project $ mergeP equirectangularP mollweideP $ curveS 2 t)
    -- play $ pauseAtEnd 1 $ setDuration 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage (project $ mergeP mollweideP (bottomleyP (toRads 30)) $ curveS 2 t)
    -- play $ pauseAtEnd 1 $ setDuration 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage (project $ mergeP mollweideP sinusoidalP $ curveS 2 t)
    -- play $ pauseAtEnd 1 $ setDuration 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage (project $ mergeP sinusoidalP equirectangularP $ curveS 2 t)
    -- play $ pauseAtEnd 1 $ setDuration 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage (project $ mergeP sinusoidalP (bottomleyP (toRads 30)) $ curveS 2 t)
    play $ pauseAtEnd 1 $ setDuration 1 $ animate $ \t ->
      scaleToSize screenWidth screenHeight $
      embedImage (project $ mergeP sinusoidalP hammerP $ curveS 2 t)
    play $ pauseAtEnd 1 $ setDuration 1 $ animate $ \t ->
      scaleToSize screenWidth screenHeight $
      embedImage (project $ mergeP hammerP mollweideP $ curveS 2 t)
    play $ pauseAtEnd 1 $ setDuration 1 $ animate $ \t ->
      scaleToSize screenWidth screenHeight $
      embedImage (project $ mergeP mollweideP mercatorP $ curveS 2 t)
    play $ pauseAtEnd 1 $ setDuration 1 $ animate $ \t ->
      scaleToSize screenWidth screenHeight $
      embedImage (project $ mergeP mercatorP lambertP $ curveS 2 t)

-- type RealProj s = MVector s Int

-- realize :: Projection -> ST s (RealProj s)
-- realize = undefined

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

isInverseP :: Projection -> Projection -> Bool
isInverseP p pInv = and
    [ check x y
    | x <- [0..w-1]
    , y <- [0..h-1] ]
  where
    w = imageWidth equirectangular
    h = imageHeight equirectangular
    check xPx yPx =
      let x = (fromIntegral xPx / fromIntegral (w-1))
          y = (fromIntegral yPx / fromIntegral (h-1))
          (lam, phi) = p x y
          (outX, outY) = pInv lam phi
          valid = lam >= -pi && lam <= pi && phi >= -pi/2 && phi <= pi/2
          diff = (x-outX)**2 + (y-outY)**2
      in if not valid || diff < 1.0e-9
          then True
          else trace (show (x,y)) False

moveDownP :: Double -> Projection -> Projection
moveDownP offset p x y = p x ((y+offset)/(1+offset))

mergeP :: Projection -> Projection -> Double -> Projection
mergeP p1 p2 t = \x y ->
    let (lon1, lat1) = p1 x y
        (lon2, lat2) = p2 x y
    in
    if | oob lon1 lat1 && oob lon2 lat2 -> (100, 100)
       -- | oob lon1 lat1 -> (fromToS x lon2 t, fromToS y lat2 t)
       -- | oob lon2 lat2 -> (fromToS lon1 x t, fromToS lat1 y t)
       | otherwise     -> (fromToS lon1 lon2 t, fromToS lat1 lat2 t)
  where
    oob lon lat = lon < (-pi) || lon > pi || lat < (-pi/2) || lat > pi/2

tau = pi*2

equirectangularF :: Projection
equirectangularF lam phi = ((lam+pi)/tau, (phi+pi/2)/pi)

equirectangularP :: Projection
equirectangularP x y = (xPi, yPi)
  where
    xPi = fromToS (-pi) pi x
    yPi = fromToS (-pi/2) (pi/2) y

-- mercatorF :: Projection
-- mercatorF lam phi = ((lam+pi)/(2*pi), pi-log ()
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
mollweideF :: Projection
mollweideF lam phi = ((x+sqrt2*2)/(4*sqrt2), (y+sqrt2)/(2*sqrt2))
  where
    sqrt2 = sqrt 2
    x = (2*sqrt2)/pi * lam * cos theta
    y = sqrt2*sin theta
    theta = find_theta 100
    find_theta 0 = phi
    find_theta n | abs phi == pi/2 = signum phi * pi/2
    find_theta n =
      let sub = find_theta (n-1)
      in sub - (2*sub+sin (2*sub)-pi*sin phi)/(2+2*cos(2*sub))

mollweideP :: Projection
mollweideP x' y' = (lam, phi)
  where
    x = fromToS (-2*sqrt(2)) (2*sqrt(2)) x'
    y = fromToS (-sqrt 2) (sqrt 2) y'
    theta = asin (y/sqrt(2))
    lam = pi*x/(2*sqrt(2)*cos theta)
    phi = asin ((2*theta+sin(2*theta))/pi)


hammerF :: Projection
hammerF lam phi = ((x+sqrt2*2)/(4*sqrt2), (y+sqrt2)/(2*sqrt2))
  where
    sqrt2 = sqrt 2
    x = (2*sqrt2*cos phi*sin (lam/2))/(sqrt (1+cos phi*cos (lam/2)))
    y = (sqrt2*sin phi)/(sqrt (1+cos phi*cos (lam/2)))
hammerP :: Projection
hammerP x' y' = (lam, phi)
  where
    x = fromToS (-2*sqrt(2)) (2*sqrt(2)) x'
    y = fromToS (-sqrt 2) (sqrt 2) y'
    z = sqrt (1 - (x/4)**2 - (y/2)**2)
    lam = 2 * atan2 (z*x) (2*(2*z**2-1))
    phi = asin (z*y)

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

bottomleyF :: Double -> Projection
bottomleyF phi_1 lam phi = ((x+pi)/tau, (y+pi/2)/pi)
  where
    x = (rho * sin e) / sin phi_1
    y = pi/2 - rho * cos e
    rho = pi/2 - phi
    e = lam * sin phi_1 * sin rho / rho

bottomleyP :: Double -> Projection
bottomleyP phi_1 x' y' = (lam, phi)
  where
    x = fromToS (-pi) (pi) x'
    y = fromToS (-pi/2) (pi/2) y'
    x1 = x * sin phi_1
    y1 = pi/2 - y
    rho = sqrt (x1*x1 + y1*y1)
    e = atan2 x1 y1
    lam = rho / sin rho * e/sin phi_1
    phi = pi/2 - rho

sinusoidalF :: Projection
sinusoidalF lam phi = ((x+pi)/tau, (y+pi/2)/pi)
  where
    x = lam * cos phi
    y = phi

sinusoidalP :: Projection
sinusoidalP x' y' = (x/cos y, y)
  where
    x = fromToS (-pi) (pi) x'
    y = fromToS (-pi/2) (pi/2) y'

wernerF :: Projection
wernerF lam phi = ((x+pi)/tau, (y+pi/2)/pi)
  where
    rho = pi/2 - phi
    e = lam * sin rho / rho
    x = rho * sin e
    y = pi/2 - rho * cos e
wernerP :: Projection
wernerP x' y' = (lam, phi)
  where
    x = fromToS (-pi) (pi) x'
    y = fromToS (-pi/2) (pi/2) y'
    rho = sqrt (x**2 + (pi/2 - y)**2)
    e = atan2 x (pi/2 -y)
    phi = pi/2 - rho
    lam = e * rho / sin rho

-- This must be wrong
bonneP :: Double -> Projection
bonneP phi_1 x' y' = (lam, -phi)
  where
    x = fromToS (-pi) (pi) x'
    y = fromToS (pi/2) (-pi/2) y'
    rho = sqrt (x**2 + (cot phi_1 + phi_1 - y)**2)
    e = atan2 x (cot phi_1 + phi_1 + y)
    phi = cot phi_1 + phi_1 - rho
    lam = e * rho / sin rho

cot = recip . tan

orthoF :: Double -> Double -> Projection
orthoF lam_0 phi_0 lam phi = ((x+(16/9))/(16/9*2), (y+1)/2)
  where
    x = cos phi * sin (lam - lam_0)
    y = cos phi_0 * sin phi - sin phi_0 * cos phi * cos (lam - lam_0)
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

cassiniP :: Projection
cassiniP x' y' = (lam, phi)
  where
    x = fromToS (-pi/2) (pi/2) x'
    y = fromToS (-pi) (pi) y'
    lam = atan2 (tan x) (cos y)
    phi = asin (sin y * cos x)
