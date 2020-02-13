{-# LANGUAGE MultiWayIf #-}
module Reanimate.GeoProjection
  ( Projection(..)
  , XYCoord(..)
  , LonLat(..)
  , project
  , interpP
  , mergeP
  , isValidP
  , scaleP
  , flipYAxisP
  , moveBottomP
  , moveTopP
    -- * Projections
  , equirectangularP
  , mercatorP
  , mollweideP
  , hammerP
  , lambertP
  , bottomleyP
  , sinusoidalP
  , wernerP
  , bonneP
  , orthoP
  , cassiniP
  , augustP
  , collignonP
  , eckert1P
  , eckert3P
  , eckert5P
  , faheyP
  , foucautP
  , lagrangeP
    -- * GeoJSON helpers
  , drawFeatureCollection -- :: GeoFeatureCollection a -> (a -> SVG -> SVG) -> SVG
  , loadFeatureCollection -- :: FromJSON a => FilePath -> (a -> SVG -> SVG) -> SVG
  , applyProjection -- :: Projection -> SVG -> SVG
  , renderGeometry
  ) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens        ((^.))
import           Control.Monad
import           Control.Monad.ST
import           Data.Aeson
import           Data.Foldable
import           Linear.V2                    hiding (angle)
import Linear (lerp, distance)
import           Data.Geospatial     hiding (LonLat)
import           Data.LinearRing
import qualified Data.LineString     as Line
import           Data.Maybe
import           Debug.Trace
import           Graphics.SvgTree    (Tree (None))
import           Reanimate
import           System.IO.Unsafe


-- Constants
halfPi, sqrtPi, epsilon, tau :: Double
halfPi = pi/2
sqrtPi = sqrt pi
epsilon = 1.0e-12
tau = pi*2

toRads, cot :: Double -> Double
toRads dec = dec/180 * pi
cot = recip . tan

srcPixel :: Image PixelRGB8 -> LonLat -> PixelRGB8
srcPixel src (LonLat lam phi) =
    pixelAt src xPx yPx
  where
    xPx = round $ ((lam+pi)/tau) * fromIntegral (imageWidth src-1)
    yPx = round $ (1-((phi+halfPi)/pi)) * fromIntegral (imageHeight src-1)

findValidCoord :: Image PixelRGB8 -> Projection -> XYCoord -> XYCoord
findValidCoord src p (XYCoord x y) = fromMaybe (XYCoord x y) $ listToMaybe
    [ XYCoord x' y'
    | let xi = round $ x * wMax
          yi = round $ y * hMax
    , ax <- [xi, xi-1, xi+1]
    , ay <- [yi, yi-1, yi+1]
    , (ax >= 0 && ax < w && ay >= 0 && ay < h)
    , let x' = fromIntegral ax / wMax
          y' = fromIntegral ay / hMax
    , validLonLat $ projectionInverse p (XYCoord x' y')
    ]
  where
    w = imageWidth src
    h = imageHeight src
    wMax = fromIntegral (w-1)
    hMax = fromIntegral (h-1)

isInWorld :: Projection -> XYCoord -> Bool
isInWorld p coord =
    odd $ length $ isInWorld' p coord

isInWorld' :: Projection -> XYCoord -> [(Double, Double)]
isInWorld' p (XYCoord x y) =
    [ (x1, y1)
    | (XYCoord x1 y1, XYCoord x2 y2) <- world
    , (y1 > y) /= (y2 > y) -- y is between y1 and y2
    , x < (x2 - x1) * (y - y1) / (y2 - y1) + x1 -- x is to the left of the line
    ]
  where
    world = worldPolygon p

worldPolygon :: Projection -> [(XYCoord, XYCoord)]
worldPolygon p =
    interp (-pi, -halfPi) (-pi, halfPi) ++
    interp (-pi, halfPi) (pi, halfPi) ++
    interp (pi, halfPi) (pi, -halfPi) ++
    interp (pi, -halfPi) (-pi, -halfPi)
  where
    apply (lam, phi) = projectionForward p $ LonLat lam phi
    steps = 100
    interp (x1, y1) (x2,y2) =
      [ ( apply (fromToS x1 x2 (n/steps), fromToS y1 y2 (n/steps))
        , apply (fromToS x1 x2 ((n+1)/steps), fromToS y1 y2 ((n+1)/steps)))
      | n <- [0..steps-1]]

findNearestPixel :: MutableImage s PixelRGBA8 -> Int -> Int -> Int -> Int -> ST s PixelRGBA8
findNearestPixel src w h srcX srcY = worker
    [ (x, y)
    | n <- [1..]
    , x <- [srcX-n .. srcY+n]
    , y <- if x == srcX-n || x == srcX+n then [srcY-n,srcY+n] else [srcY-n .. srcY+n]
    , x >= 0
    , y >= 0
    , x < w
    , y < h
    ]
  where
    worker [] = undefined
    worker ((x,y):rest) = do
      this <- readPixel src x y
      if this == blank
        then worker rest
        else return this
    blank = PixelRGBA8 0x00 0x00 0x00 0x00

interpP :: Image PixelRGB8 -> Projection -> Projection -> Double -> Image PixelRGBA8
interpP src p1 p2 t = runST $ do
    img <- newMutableImage w h
    let blank = PixelRGBA8 0x00 0x00 0x00 0x00
    let isBlank pixel = pixel == blank
    -- forM_ [0..w-1] $ \x ->
    --   forM_ [0..h-1] $ \y -> do
    --     let x1 = fromIntegral x / (wMax)
    --         y1 = 1 - fromIntegral y / (hMax)
    --     when (isInWorld (mergeP p1 p2 t) (XYCoord x1 y1)) $
    --       writePixel img x y $ PixelRGBA8 0xFF 0x00 0x00 0xFF
    let factor = 2
    forM_ [0..(w*factor)-1] $ \x ->
      forM_ [0..(h*factor)-1] $ \y -> do
        let x1' = fromIntegral x / (wMax*fromIntegral factor)
            y1' = fromIntegral y / (hMax*fromIntegral factor)
            lonlat = projectionInverse p1 (XYCoord x1' y1')
            XYCoord x1 y1 = projectionForward p1 lonlat
            XYCoord x2 y2 = findValidCoord src p2 $ projectionForward p2 lonlat
            x3 = round $ fromToS x1 x2 t * wMax
            y3 = round $ (1 - fromToS y1 y2 t) * hMax
        when (validLonLat lonlat && validXYCoord (XYCoord x2 y2)) $ do
          when (x3 >= 0 && x3 < w && y3 >= 0 && y3 < h) $ do
            writePixel img x3 y3 (promotePixel $ srcPixel src lonlat)
    forM_ [0..(w*factor)-1] $ \x ->
      forM_ [0..(h*factor)-1] $ \y -> do
        let x2' = fromIntegral x / (wMax*fromIntegral factor)
            y2' = fromIntegral y / (hMax*fromIntegral factor)
            lonlat = projectionInverse p2 (XYCoord x2' y2')
            XYCoord x2 y2 = projectionForward p2 lonlat
            XYCoord x1 y1 = findValidCoord src p1 $ projectionForward p1 lonlat
            -- (x2,y2) = p2 lam phi
            x3 = round $ fromToS x1 x2 t * wMax
            y3 = round $ (1 - fromToS y1 y2 t) * hMax
        when (validLonLat lonlat && validXYCoord (XYCoord x1 y1)) $
          when (validLonLat lonlat) $
            when (x3 >= 0 && x3 < w && y3 >= 0 && y3 < h) $
              writePixel img x3 y3 (promotePixel $ srcPixel src lonlat)
    forM_ [1..w-1] $ \x ->
      forM_ [0..h-1] $ \y -> do
        let x1 = fromIntegral x / (wMax)
            y1 = 1 - fromIntegral y / (hMax)
        this <- readPixel img x y
        when (isBlank this) $
          when (isInWorld (mergeP p1 p2 t) (XYCoord x1 y1)) $
            writePixel img x y =<< findNearestPixel img w h x y
    unsafeFreezeImage img
  where
    w = imageWidth src
    h = imageHeight src
    wMax = fromIntegral (w-1)
    hMax = fromIntegral (h-1)

eqLonLat :: LonLat -> LonLat -> Bool
eqLonLat (LonLat x1 y1) (LonLat x2 y2)
  = eqDouble x1 x2 && eqDouble y1 y2
{-
eqCoords :: XYCoord -> XYCoord -> Bool
eqCoords (XYCoord x1 y1) (XYCoord x2 y2)
  = eqDouble x1 x2 && eqDouble y1 y2
-}
eqDouble :: Double -> Double -> Bool
eqDouble a b = abs (a-b) < epsilon

data XYCoord = XYCoord Double Double -- 0 to 1
  deriving (Read,Show,Eq,Ord)
data LonLat = LonLat Double Double -- -pi to +pi, -halfPi to +halfPi
  deriving (Read,Show,Eq,Ord)
data Projection = Projection
  { projectionForward :: LonLat -> XYCoord
  , projectionInverse :: XYCoord -> LonLat
  }

-- FIXME: Verify that 'src' has an aspect ratio of 2:1.
project :: Image PixelRGB8 -> Projection -> Image PixelRGBA8
project src (Projection _ pInv) = generateImage fn w h
  where
    w = imageWidth src
    h = imageHeight src
    fn xPx yPx =
      let x = (fromIntegral xPx / fromIntegral (w-1))
          y = 1-(fromIntegral yPx / fromIntegral (h-1))
          lonlat = pInv (XYCoord x y)
      in
      if validLonLat lonlat
        then promotePixel (srcPixel src lonlat)
        else PixelRGBA8 0 0 0 0

validLonLat :: LonLat -> Bool
validLonLat (LonLat lam phi) =
  lam >= -pi && lam <= pi && phi >= -pi/2 && phi <= pi/2

validXYCoord :: XYCoord -> Bool
validXYCoord (XYCoord x y) = x >= 0 && x <= 1 && y >= 0 && y <= 1

isValidP :: Projection -> Bool
isValidP (Projection p pInv) = and
    [ check x y
    | x <- [0..w-1::Int]
    , y <- [0..h-1::Int] ]
  where
    w = 100
    h = 100
    check xPx yPx =
      let x = (fromIntegral xPx / fromIntegral (w-1))
          y = (fromIntegral yPx / fromIntegral (h-1))
          lonlat = pInv (XYCoord x y)
          lonlat2 = pInv $ p lonlat
      in if not (validLonLat lonlat) || eqLonLat lonlat lonlat2
          then True
          else trace (show (lonlat, lonlat2)) $ False

moveBottomP :: Double -> Projection -> Projection
moveBottomP offset (Projection p pInv) = Projection p' pInv'
  where
    p' (LonLat lon lat) =
      case p (LonLat lon lat) of
        XYCoord x y -> XYCoord x (fromToS offset 1 y)
    pInv' (XYCoord x y) = pInv (XYCoord x ((y-offset)/(1-offset)))

moveTopP :: Double -> Projection -> Projection
moveTopP offset = flipYAxisP . moveBottomP offset . flipYAxisP

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
        if | oob lon1 lat1 && oob lon2 lat2 -> LonLat (0/0) (0/0)
           | otherwise     -> LonLat (fromToS lon1 lon2 t) (fromToS lat1 lat2 t)
    oob lon lat = lon < (-pi) || lon > pi || lat < (-pi/2) || lat > pi/2

-- | <<docs/gifs/doc_equirectangularP.gif>>
equirectangularP :: Projection
equirectangularP = Projection forward inverse
  where
    forward (LonLat lam phi) = XYCoord ((lam+pi)/tau) ((phi+pi/2)/pi)
    inverse (XYCoord x y) = LonLat xPi yPi
      where
        xPi = fromToS (-pi) pi x
        yPi = fromToS (-pi/2) (pi/2) y

-- | <<docs/gifs/doc_mercatorP.gif>>
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

-- | <<docs/gifs/doc_mollweideP.gif>>
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
        find_theta :: Int -> Double
        find_theta 0 = phi
        find_theta _ | abs phi == pi/2 = signum phi * pi/2
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

-- | <<docs/gifs/doc_hammerP.gif>>
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

-- | <<docs/gifs/doc_lambertP.gif>>
lambertP :: Projection
lambertP = Projection forward inverse
  where
    forward (LonLat lam phi) =
      XYCoord ((lam+pi)/tau) ((sin phi+1)/2)
    inverse (XYCoord x' y') = LonLat x (asin y)
      where
        x = fromToS (-pi) (pi) x'
        y = fromToS (-1) 1 y'

-- | <<docs/gifs/doc_bottomleyP.gif>>
bottomleyP :: Double -> Projection
bottomleyP phi_1 = Projection forward inverse
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

-- | <<docs/gifs/doc_sinusoidalP.gif>>
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

-- | <<docs/gifs/doc_wernerP.gif>>
wernerP :: Projection
wernerP = moveTopP 0.23 $ Projection forward inverse
  where
    forward (LonLat lam phi) =
        XYCoord ((x+pi)/tau) ((y+pi/2)/pi)
      where
        rho = pi/2 - phi
        e = if rho == 0 then rho else lam * sin rho / rho
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
-- | <<docs/gifs/doc_bonneP.gif>>
bonneP :: Double -> Projection
bonneP 0 = sinusoidalP
bonneP phi_0 = moveTopP (-0.17*factor) $ scaleP 1 (fromToS 1 0.65 factor) $ Projection forward inverse
  where
    factor = sin phi_0 / sin (pi/4)
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

-- | <<docs/gifs/doc_orthoP.gif>>
orthoP :: Double -> Double -> Projection
orthoP lam_0 phi_0 = Projection forward inverse
  where
    -- forward (LonLat lam phi)
    --   | (lam-lam_0) < -halfPi || (lam-lam_0) > halfPi ||
    --     (phi-phi_0) < -halfPi || (phi-phi_0) > halfPi
    --     = XYCoord (0/0) (0/0)
    forward (LonLat lam phi)
        | cosc < 0  =
            let ang = atan2 y x
                xV = cos ang
                yV = sin ang
                xPos = 7/32 + ((xV+1)/2 * 9/16)
            in --trace (show (x,y)) $
              XYCoord xPos ((yV+1)/2)
            --XYCoord (0/0) (0/0)
        | otherwise = XYCoord ((x+(16/9))/(16/9*2)) ((y+1)/2)
      where
        x = cos phi * sin (lam - lam_0)
        y = cos phi_0 * sin phi - sin phi_0 * cos phi * cos (lam - lam_0)
        cosc = sin phi_0 * sin phi + cos phi_0 * cos phi * cos (lam-lam_0)
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS (-16/9) (16/9) x'
        y = fromToS (-1) (1) y'
        lam = wrap (-pi) (pi) $
          lam_0 + atan2 (x * sin c) (rho * cos c * cos phi_0 - y * sin c * sin phi_0)
        phi = wrap (-pi/2) (pi/2) $
          asin (cos c * sin phi_0 + (y * sin c * cos phi_0)/rho)
        rho = sqrt (x*x + y*y)
        c = asin rho
        wrap lower upper v
          | v > upper = v-upper+lower
          | v < lower = v+upper-lower
          | otherwise = v

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

augustP :: Projection
augustP = scaleP 0.70 0.70 $ Projection forward inverse
  where
    xHi = 16/3
    xLo = -xHi
    yHi = 8 / 3
    yLo = -yHi
    forward (LonLat lam phi) = XYCoord ((xPos-xLo)/(xHi-xLo)) ((yPos-yLo)/(yHi-yLo))
      where
        tanPhi = tan (phi/2)
        k = sqrt (1 - tanPhi * tanPhi)
        c = 1 + k * cos (lam / 2)
        x = sin (lam/2) * k / c
        y = tanPhi / c
        x2 = x*x
        y2 = y*y
        xPos = (4 / 3 * x * (3+x2 - 3*y2))
        yPos = (4 / 3 * y * (3 + 3*x2 - y2))
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS xLo xHi x' * 3 / 8
        y = fromToS yLo yHi y' * 3 / 8
        x2 = x*x
        y2 = y*y
        s = 1 + x2 + y2
        sin3Eta = sqrt ((s - sqrt (s*s - 4 * y * y)) / 2)
        eta = asin (sin3Eta) / 3
        xi = if sin3Eta /= 0 then acosh (abs (y / sin3Eta)) / 3 else asinh (abs x) / 3
        cosEta = cos eta
        coshXi = cosh xi
        d = coshXi * coshXi - cosEta * cosEta
        lam = signum x * 2 * atan2 (sinh xi * cosEta) (0.25 - d)
        phi = signum y * 2 * atan2 (coshXi * sin eta) (0.25 + d)

-- | <<docs/gifs/doc_collignonP.gif>>
collignonP :: Projection
collignonP = Projection forward inverse
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

-- | <<docs/gifs/doc_eckert1P.gif>>
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

-- | <<docs/gifs/doc_eckert3P.gif>>
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

-- | <<docs/gifs/doc_eckert5P.gif>>
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

-- | <<docs/gifs/doc_faheyP.gif>>
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

foucautP :: Projection
foucautP = Projection forward inverse
  where
    yLo = negate yHi
    yHi = sqrtPi * tan (halfPi/2)
    xLo = negate xHi
    xHi = tau/sqrtPi
    forward (LonLat lam phi) = XYCoord ((x-xLo)/(xHi-xLo)) ((y-yLo)/(yHi-yLo))
      where
        k = phi / 2
        cosk = cos k
        x = 2 * lam / sqrtPi * cos phi * cosk * cosk
        y = sqrtPi * tan k
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS xLo xHi x'
        y = fromToS yLo yHi y'
        k = atan (y / sqrtPi)
        cosk = cos k
        phi = 2 * k
        lam = x * sqrtPi / 2 / (cos phi * cosk * cosk)

lagrangeP :: Projection
lagrangeP = Projection forward inverse
  where
    yLo = negate yHi
    yHi = 2
    xLo = negate xHi
    xHi = 2
    n = 0.5
    forward (LonLat lam phi)
        | abs (abs phi - halfPi) < epsilon = XYCoord 0.5 (if phi < 0 then 0 else 1)
        | otherwise = XYCoord ((x-xLo)/(xHi-xLo)) ((y-yLo)/(yHi-yLo))
      where
        sinPhi = sin phi
        v = ((1+sinPhi) / (1 - sinPhi))**(n/2)
        c = 0.5 * (v + 1/v) + cos (lam*n)
        x = 2 * sin (lam*n) / c
        y = (v - 1/v) /c
    inverse (XYCoord x' y')
        | abs ((abs y')-1) < epsilon = LonLat 0 (signum y * halfPi)
        | otherwise = LonLat lam phi
      where
        x = fromToS xLo xHi x' / 2
        y = fromToS yLo yHi y' / 2
        x2 = x * x
        y2 = y * y
        t = 2 * y / (1 + x2 + y2)
        t' = ((1+t) / (1-t)) ** (1/n)
        lam = atan2 (2*x) (1-x2-y2) / n
        phi = asin ((t'-1)/(t'+1))
















drawFeatureCollection :: GeoFeatureCollection a -> (a -> SVG -> SVG) -> SVG
drawFeatureCollection geo fn = mkGroup
  [ fn (feature ^. properties) $ renderGeometry (feature ^. geometry)
  | feature <- toList (geo ^. geofeatures)
  ]

{-# INLINE loadFeatureCollection #-}
loadFeatureCollection :: FromJSON a => FilePath -> (a -> SVG -> SVG) -> SVG
loadFeatureCollection path = unsafePerformIO $ do
  mbGeo <- decodeFileStrict path
  case mbGeo of
    Nothing  -> error $ "loadFeatureCollection: Invalid GeoJSON: " ++ path
    Just geo -> return (drawFeatureCollection geo)

-- drawFeatureCollection :: GeoFeatureCollection a -> (a -> SVG -> SVG) -> SVG
-- loadFeatureColection :: FromJSON a => FilePath -> (a -> SVG -> SVG) -> SVG
-- modifyPoints :: ((Double,Double) -> (Double, Double)) -> SVG -> SVG
-- pointsToRadians :: SVG -> SVG
-- applyProjection :: Projection -> SVG -> SVG

renderGeometry :: GeospatialGeometry -> SVG
renderGeometry shape =
  case shape of
    MultiPolygon mpolygon ->
      mkGroup $ map (renderGeometry . Polygon) $ toList (splitGeoMultiPolygon mpolygon)
    Polygon poly ->
      mkGroup
      [ mkLinePath section
      | section <- pure
          [ (x, y)
          | PointXY x y <- map retrieveXY (fromLinearRing (head (toList (poly^.unGeoPolygon))))
          ]
      ]
    Line line ->
      mkLinePath
      [ (x, y)
      | PointXY x y <- map retrieveXY (Line.fromLineString (line ^. unGeoLine))
      ]
    MultiLine ml ->
      mkGroup $ map (renderGeometry . Line) $ toList (splitGeoMultiLine ml)
    _ -> None


applyProjection :: Projection -> SVG -> SVG
applyProjection p = mapSvgLines start
  where
    start (LineMove x:rest) = LineMove (proj x) : worker x rest
    start _ = []
    worker a (LineEnd b : rest) =
      let (x:xs) = reverse $ drop 1 $ mkChunks a b
      in map (\v -> LineBezier [v]) (map proj $ reverse xs) ++ LineEnd (proj x) : start rest
    worker a (LineBezier [b] : rest) =
      let (x:xs) = reverse $ drop 1 $ mkChunks a b
      in map (\v -> LineBezier [v]) (map proj $ reverse xs) ++ LineBezier [proj x] : worker x rest
    worker _ (LineBezier ps : rest) =
      LineBezier (map proj ps) : worker (last ps) rest
    worker _ (LineMove x:rest) = LineMove (proj x) : worker x rest
    worker _ [] = []
    tolerance = 0.01

    proj (V2 lam phi) =
      case projectionForward p $ LonLat lam phi of
        XYCoord x y -> V2 x y
    mkChunks a b =
      let midway = lerp 0.5 a b in
      if distance (proj a) (proj b) < tolerance
        then [a, b]
        else mkChunks a midway ++ drop 1 (mkChunks midway b)
