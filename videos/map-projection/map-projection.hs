#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE MultiWayIf #-}
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Control.Monad.ST
import           Control.Monad
import qualified Data.ByteString     as BS
-- import           Data.Vector         ()
-- import           Data.Vector.Unboxed
import           Debug.Trace
import           Reanimate
import           System.IO.Unsafe

{-
Map between two projections:

for each lam,phi.
  (x1,y1) = p1 lam phi
  (x2,y2) = p2 lam phi
  x3 = fromToS x1 x2 t
  y3 = fromToS y1 y2 t

Pixel at lam,phi should move from (x1,y1) to (x2,y2)

-}

halfPi = pi/2
sqrtPi = sqrt pi

interpP :: Projection -> Projection -> Double -> Image PixelRGB8
interpP p1 p2 t = runST $ do
    img <- newMutableImage w h
    let blank = PixelRGB8 0x00 0x00 0x00
    let isBlank pixel = pixel == blank
    forM_ [0..w-1] $ \x ->
      forM_ [0..h-1] $ \y ->
        writePixel img x y blank

    forM_ [0..w-1] $ \x ->
      forM_ [0..h-1] $ \y -> do
        let x1 = fromIntegral x / wMax
            y1 = fromIntegral y / hMax
            lonlat@(LonLat lam phi) = projectionInverse p1 (XYCoord x1 y1)
            XYCoord x2' y2' = projectionForward p2 lonlat
            lst@(~[(x2,y2)]) = take 1
              [ (x', y')
              | let xi = round $ x2' * wMax
                    yi = round $ y2' * hMax
              , ax <- [xi, xi-1, xi+1]
              , ay <- [yi, yi-1, yi+1]
              , (ax >= 0 && ax < w && ay >= 0 && ay < h)
              , let x' = fromIntegral ax / wMax
                    y' = fromIntegral ay / hMax
              , validLonLat $ projectionInverse p2 (XYCoord x' y')
              ]
            x3 = round $ fromToS x1 x2 t * wMax
            y3 = round $ fromToS y1 y2 t * hMax
            lamX = round $ (lam+pi)/tau * wMax
            phiY = round $ (phi+halfPi)/pi * hMax
        when (validLonLat lonlat && validXYCoord (XYCoord x2' y2')) $ do
          when (x3 >= 0 && x3 < w && y3 >= 0 && y3 < h) $ do
            writePixel img x3 y3 (pixelAt equirectangular lamX phiY)
    forM_ [0..w-1] $ \x ->
      forM_ [0..h-1] $ \y -> do
        let x2 = fromIntegral x / wMax
            y2 = fromIntegral y / hMax
            lonlat@(LonLat lam phi) = projectionInverse p2 (XYCoord x2 y2)
            XYCoord x1' y1' = projectionForward p1 lonlat
            -- (x2,y2) = p2 lam phi
            x3 = round $ fromToS x1 x2 t * wMax
            y3 = round $ fromToS y1 y2 t * hMax
            lamX = round $ (lam+pi)/tau * wMax
            phiY = round $ (phi+halfPi)/pi * hMax
            lst@(~[(x1,y1)]) = take 1
              [ (x', y')
              | let xi = round $ x1' * wMax
                    yi = round $ y1' * hMax
              , ax <- [xi, xi-1, xi+1]
              , ay <- [yi, yi-1, yi+1]
              , (ax >= 0 && ax < w && ay >= 0 && ay < h)
              , let x' = fromIntegral ax / wMax
                    y' = fromIntegral ay / hMax
              , validLonLat $ projectionInverse p1 (XYCoord x' y')
              ]
            --check = projectionInverse p1 (XYCoord x1' y1')
        when (validLonLat lonlat && validXYCoord (XYCoord x1' y1')) $
          when (validLonLat lonlat {-&& validLonLat check && xCheck >= 0-}) $
            when (x3 >= 0 && x3 < w && y3 >= 0 && y3 < h) $
              writePixel img x3 y3 (pixelAt equirectangular lamX phiY)
    forM_ [1..w-2] $ \x ->
      forM_ [0..h-1] $ \y -> do
        this <- readPixel img x y
        when (isBlank this) $ do
          left <- readPixel img (x-1) y
          right <- readPixel img (x+1) y
          unless (isBlank left || isBlank right) $ writePixel img x y left
    forM_ [0..w-1] $ \x ->
      forM_ [1..h-2] $ \y -> do
        this <- readPixel img x y
        when (isBlank this) $ do
          left <- readPixel img x (y-1)
          right <- readPixel img x (y+1)
          unless (isBlank left || isBlank right) $ writePixel img x y left
          -- unless (isBlack right) $ writePixel img x y right
    forM_ [1..w-2] $ \x ->
      forM_ [0..h-1] $ \y -> do
        this <- readPixel img x y
        when (isBlank this) $ do
          left <- readPixel img (x-1) y
          right <- readPixel img (x+1) y
          unless (isBlank left || isBlank right) $ writePixel img x y left
    unsafeFreezeImage img
  where
    w = imageWidth equirectangular
    h = imageHeight equirectangular
    wMax = fromIntegral (w-1)
    hMax = fromIntegral (h-1)


eqLonLat (LonLat x1 y1) (LonLat x2 y2)
  = abs (x1-x2) < epsilon && abs (y1-y2) < epsilon
  where
    epsilon = 1.0e-9

eqCoords (XYCoord x1 y1) (XYCoord x2 y2)
  = abs (x1-x2) < epsilon && abs (y1-y2) < epsilon
  where
    epsilon = 1.0e-9

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

    -- play $ pauseAtEnd 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP equirectangularP lambertP $ curveS 2 t
    -- play $ pauseAtEnd 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ project $ mergeP lambertP hammerP $ curveS 2 t
    -- play $ pauseAtEnd 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ project $ mergeP hammerP sinusoidalP $ curveS 2 t
    -- play $ pauseAtEnd 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP sinusoidalP (bottomleyP (toRads 30)) $ curveS 2 t
    -- play $ pauseAtEnd 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP (bottomleyP (toRads 30)) equirectangularP $ curveS 2 t
    -- play $ pauseAtEnd 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP equirectangularP mercatorP $ curveS 2 t
    -- play $ pauseAtEnd 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP mercatorP equirectangularP $ curveS 2 t
    -- play $ pauseAtEnd 1 $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP equirectangularP (orthoP 0 0) $ curveS 2 t
    -- play $ pauseAtEnd 1 $ reverseA $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP equirectangularP (orthoP 0 0) $ curveS 2 t
    -- play $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP faheyP (orthoP 0 0) $ curveS 2 t

    -- play $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP equirectangularP faheyP $ curveS 2 t
    play $ animate $ \t ->
      scaleToSize screenWidth screenHeight $
      embedImage $ project $ mergeP faheyP (orthoP (-halfPi) 0) $ curveS 2 t
    play $ setDuration 3 $ animate $ \t ->
      scaleToSize screenWidth screenHeight $
      embedImage $ project $ orthoP (fromToS (-halfPi) (halfPi*0.4) $ curveS 2 t) 0
    play $ animate $ \t ->
      scaleToSize screenWidth screenHeight $
      embedImage $ project $ mergeP (orthoP (halfPi*0.4) 0) faheyP  $ curveS 2 t
    -- play $ animate $ \t ->
    --   scaleToSize screenWidth screenHeight $
    --   embedImage $ interpP faheyP equirectangularP  $ curveS 2 t

    return ()

-- type RealProj s = MVector s Int

-- realize :: Projection -> ST s (RealProj s)
-- realize = undefined

equirectangular :: Image PixelRGB8
equirectangular = unsafePerformIO $ do
  dat <- BS.readFile "earth.jpg"
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGB8 img

data XYCoord = XYCoord Double Double -- 0 to 1
  deriving (Read,Show,Eq,Ord)
data LonLat = LonLat Double Double -- -pi to +pi, -halfPi to +halfPi
  deriving (Read,Show,Eq,Ord)
data Projection = Projection
  { projectionForward :: LonLat -> XYCoord
  , projectionInverse :: XYCoord -> LonLat
  }

project :: Projection -> Image PixelRGB8
project (Projection _ pInv) = generateImage fn w h
  where
    w = imageWidth equirectangular
    h = imageHeight equirectangular
    fn xPx yPx =
      let x = (fromIntegral xPx / fromIntegral (w-1))
          y = (fromIntegral yPx / fromIntegral (h-1))
          LonLat lon lat = pInv (XYCoord x y)
          x_e = round $ fromToS 0 (fromIntegral w-1) ((lon+pi) / (2*pi))
          y_e = round $ fromToS 0 (fromIntegral h-1) ((lat+pi/2) / pi)
      in
      if x_e < 0 || x_e >= w || y_e < 0 || y_e >= h
        then PixelRGB8 0 0 0
        else pixelAt equirectangular x_e y_e

validLonLat :: LonLat -> Bool
validLonLat (LonLat lam phi) =
  lam >= -pi && lam <= pi && phi >= -pi/2 && phi <= pi/2

validXYCoord :: XYCoord -> Bool
validXYCoord (XYCoord x y) = x >= 0 && x <= 1 && y >= 0 && y <= 1

isValidP :: Projection -> Bool
isValidP (Projection p pInv) = and
    [ check x y
    | x <- [0..w-1]
    , y <- [0..h-1] ]
  where
    w = 100
    h = 100
    check xPx yPx =
      let x = (fromIntegral xPx / fromIntegral (w-1))
          y = (fromIntegral yPx / fromIntegral (h-1))
          lonlat = pInv (XYCoord x y)
          XYCoord outX outY = p lonlat
          diff = (x-outX)**2 + (y-outY)**2
          epsilon = 1.0e-9
      in if not (validLonLat lonlat) || not (validXYCoord (XYCoord outX outY)) || diff < epsilon
          then True
          else trace (show (x,y)) False

moveDownP :: Double -> Projection -> Projection
moveDownP offset (Projection p pInv) = Projection p' pInv'
  where
    p' (LonLat lon lat) =
      case p (LonLat lon lat) of
        XYCoord x y -> XYCoord x (fromToS offset 1 y)
    pInv' (XYCoord x y) = pInv (XYCoord x ((y-offset)/(1-offset)))

flipYAxisP :: Projection -> Projection
flipYAxisP (Projection p pInv) = Projection p' pInv'
  where
    p' (LonLat lam phi) =
      let XYCoord x y = p (LonLat lam (negate phi))
      in XYCoord x (1-y)
    pInv' (XYCoord x y) =
      let LonLat lam phi = pInv (XYCoord x (1-y))
      in LonLat lam (negate phi)

scaleP :: Double -> Double -> Projection -> Projection
scaleP xScale yScale (Projection p pInv) = Projection forward inverse
  where
    forward lonlat =
      case p lonlat of
        XYCoord x y -> XYCoord ((x-0.5)*xScale+0.5) ((y-0.5)*yScale+0.5)
    inverse (XYCoord x y) =
      let new = XYCoord ((x-0.5)/xScale+0.5) ((y-0.5)/yScale+0.5)
      in pInv new


mergeP :: Projection -> Projection -> Double -> Projection
mergeP p1 p2 t = Projection p pInv
  where
    p lonlat =
      let XYCoord x1 y1 = projectionForward p1 lonlat
          XYCoord x2 y2 = projectionForward p2 lonlat
      in XYCoord (fromToS x1 x2 t) (fromToS y1 y2 t)
    pInv coord =
      let LonLat lon1 lat1 = projectionInverse p1 coord
          LonLat lon2 lat2 = projectionInverse p2 coord
      in
        if | oob lon1 lat1 && oob lon2 lat2 -> LonLat 100 100
           -- | oob lon1 lat1 -> (fromToS x lon2 t, fromToS y lat2 t)
           -- | oob lon2 lat2 -> (fromToS lon1 x t, fromToS lat1 y t)
           | otherwise     -> LonLat (fromToS lon1 lon2 t) (fromToS lat1 lat2 t)
    oob lon lat = lon < (-pi) || lon > pi || lat < (-pi/2) || lat > pi/2

tau = pi*2

equirectangularP :: Projection
equirectangularP = Projection forward inverse
  where
    forward (LonLat lam phi) = XYCoord ((lam+pi)/tau) ((phi+pi/2)/pi)
    inverse (XYCoord x y) = LonLat xPi yPi
      where
        xPi = fromToS (-pi) pi x
        yPi = fromToS (-pi/2) (pi/2) y

mercatorP :: Projection
mercatorP = Projection forward inverse
  where
    forward (LonLat lam phi) =
      XYCoord ((lam+pi)/tau)
        (((log(tan(pi/4+phi/2))) + pi)/tau)
    inverse (XYCoord x y) = LonLat xPi (atan (sinh yPi))
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
mollweideP = Projection forward inverse
  where
    forward (LonLat lam phi) =
        XYCoord ((x+sqrt2*2)/(4*sqrt2)) ((y+sqrt2)/(2*sqrt2))
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
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS (-2*sqrt(2)) (2*sqrt(2)) x'
        y = fromToS (-sqrt 2) (sqrt 2) y'
        theta = asin (y/sqrt(2))
        lam = pi*x/(2*sqrt(2)*cos theta)
        phi = asin ((2*theta+sin(2*theta))/pi)

hammerP :: Projection
hammerP = Projection forward inverse
  where
    forward (LonLat lam phi) =
        XYCoord ((x+sqrt2*2)/(4*sqrt2)) ((y+sqrt2)/(2*sqrt2))
      where
        sqrt2 = sqrt 2
        x = (2*sqrt2*cos phi*sin (lam/2))/(sqrt (1+cos phi*cos (lam/2)))
        y = (sqrt2*sin phi)/(sqrt (1+cos phi*cos (lam/2)))
    inverse (XYCoord x' y') = LonLat lam phi
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
lambertP = Projection forward inverse
  where
    forward (LonLat lam phi) =
      XYCoord ((lam+pi)/tau) ((sin phi+1)/2)
    inverse (XYCoord x' y') = LonLat x (asin y)
      where
        x = fromToS (-pi) (pi) x'
        y = fromToS (-1) 1 y'

toRads dec = dec/180 * pi

bottomleyP :: Double -> Projection
bottomleyP phi_1 = flipYAxisP $ Projection forward inverse
  where
    forward (LonLat lam phi) =
        XYCoord ((x+pi)/tau) ((y+pi/2)/pi)
      where
        x = (rho * sin e) / sin phi_1
        y = pi/2 - rho * cos e
        rho = pi/2 - phi
        e = lam * sin phi_1 * sin rho / rho
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS (-pi) (pi) x'
        y = fromToS (-pi/2) (pi/2) y'
        x1 = x * sin phi_1
        y1 = pi/2 - y
        rho = sqrt (x1*x1 + y1*y1)
        e = atan2 x1 y1
        lam = (if rho == 0 then 1 else rho / sin rho) * e/sin phi_1
        phi = pi/2 - rho

sinusoidalP :: Projection
sinusoidalP = Projection forward inverse
  where
    forward (LonLat lam phi) =
        XYCoord ((x+pi)/tau) ((y+pi/2)/pi)
      where
        x = lam * cos phi
        y = phi
    inverse (XYCoord x' y') = LonLat (x/cos y) y
      where
        x = fromToS (-pi) (pi) x'
        y = fromToS (-pi/2) (pi/2) y'

wernerP :: Projection
wernerP = moveDownP 0.25 $ flipYAxisP $ Projection forward inverse
  where
    forward (LonLat lam phi) =
        XYCoord ((x+pi)/tau) ((y+pi/2)/pi)
      where
        rho = pi/2 - phi
        e = lam * sin rho / rho
        x = rho * sin e
        y = pi/2 - rho * cos e
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS (-pi) (pi) x'
        y = fromToS (-pi/2) (pi/2) y'
        rho = sqrt (x**2 + (pi/2 - y)**2)
        e = atan2 x (pi/2 -y)
        phi = pi/2 - rho
        lam = e * rho / sin rho

-- FIXME: find right scale and position.
bonneP :: Double -> Projection
bonneP 0 = sinusoidalP
bonneP phi_0 = moveDownP (-0.15) $ scaleP sf sf $ flipYAxisP $ Projection forward inverse
  where
    sf = 0.65
    forward (LonLat lam phi ) = XYCoord ((x+pi)/tau) ((y+halfPi)/pi)
      where
        cotPhi0 = cot phi_0
        rho = cotPhi0 + phi_0 - phi
        e = (lam * cos phi) / rho
        x = rho * sin e
        y = cotPhi0 - rho * cos e
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS (-pi) (pi) x'
        y = fromToS (-pi/2) (pi/2) y'
        cotPhi0 = cot phi_0
        rho = sqrt (x*x + (cot phi_0 - y)**2)
        -- e = atan2 x (cot phi_1 + phi_1 + y)
        phi = cotPhi0 + phi_0 - rho
        lam = rho / cos phi * atan2 x (cotPhi0-y)

cot = recip . tan

orthoP :: Double -> Double -> Projection
orthoP lam_0 phi_0 = Projection forward inverse
  where
    forward (LonLat lam phi)
      | (lam+lam_0) < -halfPi || (lam+lam_0) > halfPi ||
        (phi+phi_0) < -halfPi/2 || (phi+phi_0) > halfPi/2
        = XYCoord (0/0) (0/0)
    forward (LonLat lam phi) =
        XYCoord ((x+(16/9))/(16/9*2)) ((y+1)/2)
      where
        x = cos phi * sin (lam - lam_0)
        y = cos phi_0 * sin phi - sin phi_0 * cos phi * cos (lam - lam_0)
    inverse (XYCoord x' y') = LonLat lam phi
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
cassiniP = Projection forward inverse
  where
    forward (LonLat lam phi) =
      XYCoord ((asin (cos phi * sin lam)+halfPi)/pi) ((atan2 (tan phi) (cos lam)+pi)/tau)
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS (-halfPi) halfPi x'
        y = fromToS (-pi) (pi) y'
        lam = atan2 (tan x) (cos y)
        phi = asin (sin y * cos x)

-- incomplete
augustP :: Projection
augustP = Projection forward inverse
  where
    forward (LonLat lam phi) = XYCoord x2 y2
      where
        tanPhi = tan (phi/2)
        k = sqrt (1 - tanPhi * tanPhi)
        c = 1 + k * cos (lam / 2)
        x = sin (lam/2) * k / c
        y = tanPhi / c
        x2 = x*x
        y2 = y*y
    inverse = undefined

collignonP :: Projection
collignonP = flipYAxisP $ Projection forward inverse
  where
    yHi = sqrtPi
    yLo = sqrtPi * (1 - sqrt 2)
    xLo = -pi*(2/sqrtPi)*(sqrt 2)
    xHi = pi*(2/sqrtPi)*(sqrt 2)
    forward (LonLat lam phi) = XYCoord ((x-xLo)/(xHi-xLo)) ((y-yLo)/(yHi-yLo))
      where
        alpha = sqrt (1 - sin phi)
        x = (2 / sqrtPi) * lam * alpha
        y = sqrtPi * (1 - alpha)
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS xLo xHi x'
        y = fromToS yLo yHi y'
        l = (y / sqrtPi -1)**2
        lam = if l > 0 then x * sqrt (pi / l) / 2 else 0
        phi = asin (1-l)

eckert1P :: Projection
eckert1P = Projection forward inverse
  where
    alpha = sqrt (8 / (3*pi))
    yLo = -alpha * halfPi
    yHi = alpha * halfPi
    xLo = -alpha * pi
    xHi = alpha * pi
    forward (LonLat lam phi) = XYCoord ((x-xLo)/(xHi-xLo)) ((y-yLo)/(yHi-yLo))
      where
        x = alpha * lam * (1 - abs phi / pi)
        y = alpha * phi
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS xLo xHi x'
        y = fromToS yLo yHi y'
        phi = y / alpha
        lam = x / (alpha * (1 - abs phi / pi))

eckert3P :: Projection
eckert3P = Projection forward inverse
  where
    k = sqrt (pi * (4 + pi))
    yLo = negate yHi
    yHi = 4/k * halfPi
    xLo = negate xHi
    xHi = 4/k * pi
    forward (LonLat lam phi) = XYCoord ((x-xLo)/(xHi-xLo)) ((y-yLo)/(yHi-yLo))
      where
        x = 2 / k * lam * (1 + sqrt (1 - 4*phi*phi/(pi*pi)))
        y = 4 / k * phi
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS xLo xHi x'
        y = fromToS yLo yHi y'
        lam = x  * k/2 / (1 + sqrt (1 - y*y* (4+pi)/(4*pi)))
        phi = y * k/4

eckert5P :: Projection
eckert5P = Projection forward inverse
  where
    k = sqrt (2 + pi)
    yLo = negate yHi
    yHi = pi/k
    xLo = negate xHi
    xHi = tau/k
    forward (LonLat lam phi) = XYCoord ((x-xLo)/(xHi-xLo)) ((y-yLo)/(yHi-yLo))
      where
        x = lam * (1 + cos phi) / k
        y = 2 * phi / k
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS xLo xHi x'
        y = fromToS yLo yHi y'
        lam = k * x / (1 + cos phi)
        phi = y * k / 2

faheyP :: Projection
faheyP = Projection forward inverse
  where
    faheyK = cos (toRads 35)
    yLo = negate yHi
    yHi = 1 + faheyK
    xLo = negate xHi
    xHi = pi * faheyK * 16/9
    forward (LonLat lam phi) = XYCoord ((x-xLo)/(xHi-xLo)) ((y-yLo)/(yHi-yLo))
      where
        t = tan (phi/2)
        x = lam * faheyK * sqrt (1 - t*t)
        y = (1 + faheyK) * t
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS xLo xHi x'
        y = fromToS yLo yHi y'
        t = y / (1 + faheyK)
        lam = x / (faheyK * sqrt (1 - t*t))
        phi = 2 * atan2 y (1 + faheyK)
