{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE MultiWayIf   #-}
module Reanimate.GeoProjection
  ( Projection(..)
  , XYCoord(..)
  , LonLat(..)
  , project
  , interpP
  , interpBBP
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
  , cylindricalEqualAreaP
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
  , applyProjection' -- :: Double -> Projection -> SVG -> SVG
  , renderGeometry
  ) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens            ((^.))
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Aeson
import           Data.Foldable
import           Data.Geospatial         hiding (LonLat)
import           Data.Hashable
import           Data.LinearRing
import qualified Data.LineString         as Line
import           Data.Vector.Storable    (unsafeWith)
import qualified Data.Vector.Unboxed     as V
import           Debug.Trace
import           Foreign
import           GHC.Exts                (Double (..), cosDouble#, sinDouble#,
                                          (*##), (+##), (-##), (/##))
import           Graphics.SvgTree        (Tree (None))
import           Linear                  (distance, lerp)
import           Linear.V2               hiding (angle)
import           Reanimate
import           System.IO.Unsafe

{-# INLINE halfPi #-}
{-# INLINE sqrtPi #-}
{-# INLINE sqrt2 #-}
{-# INLINE epsilon #-}
{-# INLINE tau #-}
-- Constants
halfPi, sqrtPi, sqrt2, epsilon, tau :: Double
halfPi = pi/2
sqrtPi = sqrt pi
sqrt2 = sqrt 2
epsilon = 1.0e-12
tau = pi*2

toRads, cot :: Double -> Double
toRads dec = dec/180 * pi
cot = recip . tan


srcPixel :: Pixel pixel => Image pixel -> LonLat -> pixel
srcPixel src (LonLat lam phi) =
    -- pixelAt src xPx yPx
    unsafePixelAt (imageData src) (pixelBaseIndex src xPx yPx)
  where
    !xPx = round $ ((lam+pi)/tau) * fromIntegral (imageWidth src-1)
    !yPx = round $ (1-((phi+halfPi)/pi)) * fromIntegral (imageHeight src-1)


srcPixelFast :: Image PixelRGBA8 -> Double -> Double -> LonLat -> ST s PixelRGBA8
srcPixelFast src wMax hMax (LonLat lam phi) = unsafeIOToST $
    unsafeWith (imageData src) $ \ptr -> do
      let ptr' = plusPtr ptr idx
      r <- peek ptr'
      g <- peek $ plusPtr ptr' 1
      b <- peek $ plusPtr ptr' 2
      a <- peek $ plusPtr ptr' 3
      return $ PixelRGBA8 r g b a
  where
    !idx = pixelBaseIndex src xPx yPx
    !xPx = round $ ((lam+pi)/tau) * wMax
    !yPx = round $ (1-((phi+halfPi)/pi)) * hMax

{- HLINT ignore -}
-- findValidCoord :: Int -> Int -> Double -> Double -> (XYCoord -> LonLat) -> XYCoord -> XYCoord
-- findValidCoord !w !h !wMax !hMax !p_inv (XYCoord x y) = foldr const (XYCoord x y)
--     [ XYCoord x' y'
--     | let !xi = round $ x * wMax
--           !yi = round $ y * hMax
--     , !ax <- [xi, xi-1, xi+1]
--     , ax >= 0
--     , ax < w
--     , let !x' = fromIntegral ax / wMax
--     , !ay <- [yi, yi-1, yi+1]
--     , ay >= 0
--     , ay < h
--     , let !y' = fromIntegral ay / hMax
--     , validLonLat $! p_inv $! XYCoord x' y'
--     ]

-- isInWorld :: Projection -> XYCoord -> Bool
-- isInWorld p coord =
--     odd $ length $ isInWorld' p coord
--
-- isInWorld' :: Projection -> XYCoord -> [(Double, Double)]
-- isInWorld' p (XYCoord x y) =
--     [ (x1, y1)
--     | (XYCoord x1 y1, XYCoord x2 y2) <- world
--     , (y1 > y) /= (y2 > y) -- y is between y1 and y2
--     , x < (x2 - x1) * (y - y1) / (y2 - y1) + x1 -- x is to the left of the line
--     ]
--   where
--     world = worldPolygon p
--
-- worldPolygon :: Projection -> [(XYCoord, XYCoord)]
-- worldPolygon p =
--     interp (-pi, -halfPi) (-pi, halfPi) ++
--     interp (-pi, halfPi) (pi, halfPi) ++
--     interp (pi, halfPi) (pi, -halfPi) ++
--     interp (pi, -halfPi) (-pi, -halfPi)
--   where
--     apply (lam, phi) = projectionForward p $ LonLat lam phi
--     steps = 100
--     interp (x1, y1) (x2,y2) =
--       [ ( apply (fromToS x1 x2 (n/steps), fromToS y1 y2 (n/steps))
--         , apply (fromToS x1 x2 ((n+1)/steps), fromToS y1 y2 ((n+1)/steps)))
--       | n <- [0..steps-1]]

-- findNearestPixel :: MutableImage s PixelRGBA8 -> Int -> Int -> Int -> Int -> ST s PixelRGBA8
-- findNearestPixel src w h srcX srcY = worker $ take 20
--     [ (x, y)
--     | n <- [1..]
--     , x <- [srcX-n .. srcY+n]
--     , y <- if x == srcX-n || x == srcX+n then [srcY-n,srcY+n] else [srcY-n .. srcY+n]
--     , x >= 0
--     , y >= 0
--     , x < w
--     , y < h
--     ]
--   where
--     worker [] = pure $ PixelRGBA8 0xFF 0x00 0x00 0xFF
--     worker ((x,y):rest) = do
--       this <- readPixel src x y
--       if this == blank
--         then worker rest
--         else return this
--     blank = PixelRGBA8 0x00 0x00 0x00 0x00


-- Original version:        134,925 pixels/second
-- Inlined projections:     136,332 pixels/second
-- TEST: no write pixels:   134,288 pixels/second !!!
-- Fast theta:            1,489,719 pixels/second
-- Cached theta:          3,622,254 pixels/second

-- to equirectangularP:   9,015,326 pixels/second
--                        9,680,217
--                        9,466,744
--                       14,830,330
-- to lambertP:           6,735,973
-- to mercatorP:          4,248,927
-- to mollweideP:         3,593,545
-- to hammerP:            3,237,699
-- to bottomleyP:         3,864,848
-- to sinusoidalP:        7,020,756
-- to wernerP:            4,433,295
-- to bonneP:             4,071,187
-- to augustP:            2,553,177
-- to collignonP:         5,486,849
-- to eckert1P:           7,428,849
-- to eckert3P:           6,666,936
-- to eckert5P:           6,106,492
-- to faheyP:             5,137,291
-- to foucautP:           3,983,151
-- to lagrangeP:          3,850,611
-- interpFastP :: Image PixelRGBA8 -> Projection -> Projection -> Double -> Image PixelRGBA8
-- interpFastP !src (Projection _ p1 p1_inv) (Projection _ p2 p2_inv) !t = runST $ do
--     unsafeIOToST $ putStrLn "Allocating new array"
--     !img <- newMutableImage w h
--     unsafeIOToST $ putStrLn "done"
--     start <- unsafeIOToST $ getCurrentTime
--     let factor = 2
--         total = w*factor * h*factor
--     let l1 =
--           loopTo (w*factor) $ \x -> do
--             loopTo (h*factor) $ \y -> do
--               let thisIndex = (x*h*factor+y)
--               when (thisIndex `mod` 1000000 == 0) $ unsafeIOToST $ do
--                 now <- getCurrentTime
--                 let diff = realToFrac (diffUTCTime now start):: Double
--                 printf "%.2f pixels/second\n" (fromIntegral (total-thisIndex) / diff)
--               let !x1' = fromIntegral x / (wMax*fromIntegral factor)
--                   !y1' = fromIntegral y / (hMax*fromIntegral factor)
--                   !lonlat = p1_inv $! XYCoord x1' y1'
--                   -- p = srcPixel src lonlat
--               -- unsafeIOToST (evaluate lonlat)
--
--               when (validLonLat lonlat) $ do
--                 p <- srcPixelFast src wMax hMax lonlat
--                 when (pixelOpacity p /= 0) $ do
--                   let XYCoord !x1 !y1 = p1 lonlat
--                       -- !coord = p2 lonlat
--                       XYCoord !x2 !y2 = p2 lonlat -- findValidCoord w h wMax hMax p2_inv $ p2 lonlat
--                       !x3 = round $ fromToS x1 x2 t * wMax
--                       !y3 = round $ (1 - fromToS y1 y2 t) * hMax :: Int
--                   -- unsafeIOToST (evaluate coord)
--                   -- return ()
--                   when (x3 >= 0 && x3 < w && y3 >= 0 && y3 < h) $
--                     writePixel img x3 y3 p
--     l1
--     unsafeFreezeImage img
--   where
--     loopTo m fn = go m
--       where go 0 = return ()
--             go n = fn (n-1) >> go (n-1)
--     !w = imageWidth src
--     !h = imageHeight src
--     !wMax = fromIntegral (w-1)
--     !hMax = fromIntegral (h-1)

interpP :: Image PixelRGBA8 -> Projection -> Projection -> Double -> Image PixelRGBA8
interpP src p1 _ 0 = project src p1
interpP src _ p2 1 = project src p2
interpP !src (Projection _label1 p1 p1_inv) !(Projection _label2 p2 p2_inv) !t = runST $ do
    !img <- newMutableImage w h

    let factor = 2
        -- total = w*factor * h*factor
    let l1 = do
          -- start <- unsafeIOToST $ getCurrentTime
          loopTo (w*factor) $ \x -> do
            loopTo (h*factor) $ \y -> do
              -- let thisIndex = (x*h*factor+y)
              -- when (thisIndex `mod` 1000000 == 0) $ unsafeIOToST $ do
              --   now <- getCurrentTime
              --   let diff = realToFrac (diffUTCTime now start):: Double
              --   printf "%.2f pixels/second: %s\n" (fromIntegral (total-thisIndex) / diff) label1
              let !x1' = fromIntegral x / (wMax*fromIntegral factor)
                  !y1' = fromIntegral y / (hMax*fromIntegral factor)
                  !lonlat = p1_inv $! XYCoord x1' y1'
                  -- p = srcPixel src lonlat

              when (validLonLat lonlat) $ do
                p <- srcPixelFast src wMax hMax lonlat
                when (pixelOpacity p /= 0) $ do
                  let XYCoord x1 y1 = p1 lonlat
                      -- XYCoord x2 y2 = findValidCoord w h wMax hMax p2_inv $ p2 lonlat
                      XYCoord x2 y2 = p2 lonlat
                      !x3 = round $ fromToS x1 x2 t * wMax
                      !y3 = round $ (1 - fromToS y1 y2 t) * hMax
                  when (x3 >= 0 && x3 < w && y3 >= 0 && y3 < h) $
                    writePixel img x3 y3 p
        l2 = do
          -- start <- unsafeIOToST $ getCurrentTime
          loopTo (w*factor) $ \x ->
            loopTo (h*factor) $ \y -> do
              -- let thisIndex = (x*h*factor+y)
              -- when (thisIndex `mod` 1000000 == 0) $ unsafeIOToST $ do
              --   now <- getCurrentTime
              --   let diff = realToFrac (diffUTCTime now start):: Double
              --   printf "%.2f pixels/second: %s\n" (fromIntegral (total-thisIndex) / diff) label2
              let !x2' = fromIntegral x / (wMax*fromIntegral factor)
                  !y2' = fromIntegral y / (hMax*fromIntegral factor)
                  !lonlat = p2_inv (XYCoord x2' y2')
                  -- p = srcPixel src lonlat
              when (validLonLat lonlat) $ do
                p <- srcPixelFast src wMax hMax lonlat
                when (pixelOpacity p /= 0) $ do
                  let XYCoord x2 y2 = p2 lonlat
                      -- XYCoord x1 y1 = findValidCoord w h wMax hMax p1_inv $ p1 lonlat
                      XYCoord x1 y1 = p1 lonlat
                      !x3 = round $ fromToS x1 x2 t * wMax
                      !y3 = round $ (1 - fromToS y1 y2 t) * hMax
                  when (x3 >= 0 && x3 < w && y3 >= 0 && y3 < h) $ do
                    writePixel img x3 y3 p
    if t < 0.5
      then l1 >> l2
      else l2 >> l1
    unsafeFreezeImage img
  where
    loopTo m fn = go m
      where go 0 = return ()
            go n = fn (n-1) >> go (n-1)
    !w = imageWidth src
    !h = imageHeight src
    !wMax = fromIntegral (w-1)
    !hMax = fromIntegral (h-1)

interpBBP :: Image PixelRGBA8 -> Projection -> Projection ->
            (Double,Double,Double,Double) -> (Double,Double,Double,Double) -> Double -> Image PixelRGBA8
interpBBP !src (Projection _ p1 p1_inv) !(Projection _ p2 p2_inv) (fx,fy,fw,fh) (tx, ty, tw, th) !t = runST $ do
    !img <- newMutableImage w h
    let factor = 2
    let l1 =
          loopTo (w*factor) $ \x -> do
            loopTo (h*factor) $ \y -> do
              let !x1' = fromIntegral x / (wMax*fromIntegral factor)
                  !y1' = fromIntegral y / (hMax*fromIntegral factor)
              when (x1' >= fx && x1' <= fx+fw && y1' >= fy && y1' <= fy+fh) $ do
                let !lonlat = p1_inv $! XYCoord x1' y1'
                      -- p = srcPixel src lonlat

                when (validLonLat lonlat) $ do
                    -- let LonLat lam phi = lonlat
                    --     !xPx = ((lam+pi)/tau)
                    --     !yPx = (((phi+halfPi)/pi))
                    -- when (xPx >= fx && xPx <= fx+fw && yPx >= fy && yPx <= fy+fh) $ do
                    p <- srcPixelFast src wMax hMax lonlat
                    when (pixelOpacity p /= 0) $ do
                      let XYCoord x1 y1 = p1 lonlat
                          XYCoord x2 y2 = p2 lonlat
                          -- XYCoord x2 y2 = findValidCoord w h wMax hMax p2_inv $ p2 lonlat
                          !x3 = round $ fromToS x1 x2 t * wMax
                          !y3 = round $ (1 - fromToS y1 y2 t) * hMax
                      when (x3 >= 0 && x3 < w && y3 >= 0 && y3 < h) $
                        writePixel img x3 y3 p
        l2 =
          loopTo (w*factor) $ \x ->
            loopTo (h*factor) $ \y -> do
              let !x2' = fromIntegral x / (wMax*fromIntegral factor)
                  !y2' = fromIntegral y / (hMax*fromIntegral factor)
              when (x2' >= tx && x2' <= tx+tw && y2' >= ty && y2' <= ty+th) $ do
                let !lonlat = p2_inv (XYCoord x2' y2')
                    -- p = srcPixel src lonlat
                when (validLonLat lonlat) $ do
                  p <- srcPixelFast src wMax hMax lonlat
                  when (pixelOpacity p /= 0) $ do
                    let XYCoord x2 y2 = p2 lonlat
                        -- XYCoord x1 y1 = findValidCoord w h wMax hMax p1_inv $ p1 lonlat
                        XYCoord x1 y1 = p1 lonlat
                        !x3 = round $ fromToS x1 x2 t * wMax
                        !y3 = round $ (1 - fromToS y1 y2 t) * hMax
                    when (x3 >= 0 && x3 < w && y3 >= 0 && y3 < h) $ do
                      writePixel img x3 y3 p
    if t < 0.5
      then l1 >> l2
      else l2 >> l1

    -- when False $
    --   forM_ [0..w-1] $ \x ->
    --     forM_ [0..h-1] $ \y -> do
    --       let x1 = fromIntegral x / (wMax)
    --           y1 = 1 - fromIntegral y / (hMax)
    --       this <- readPixel img x y
    --       when (isBlank this) $
    --         when (isInWorld (mergeP p1 p2 t) (XYCoord x1 y1)) $
    --           writePixel img x y =<< findNearestPixel img w h x y
    unsafeFreezeImage img
  where
    loopTo m fn = go m
      where go 0 = return ()
            go n = fn (n-1) >> go (n-1)
    !w = imageWidth src
    !h = imageHeight src
    !wMax = fromIntegral (w-1)
    !hMax = fromIntegral (h-1)


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

data XYCoord = XYCoord !Double !Double -- 0 to 1
  deriving (Read,Show,Eq,Ord)
data LonLat = LonLat !Double !Double -- -pi to +pi, -halfPi to +halfPi
  deriving (Read,Show,Eq,Ord)
instance Hashable LonLat where
  hashWithSalt s (LonLat a b) = hashWithSalt s (a,b)
data Projection = Projection
  { projectionLabel   :: String
  , projectionForward :: !(LonLat -> XYCoord)
    --
    -- (Double# -> Double# -> (# Double#, Double# #))
  , projectionInverse :: !(XYCoord -> LonLat)
  }

-- FIXME: Verify that 'src' has an aspect ratio of 2:1.
project :: Image PixelRGBA8 -> Projection -> Image PixelRGBA8
project src (Projection _label _ pInv) = generateImage fn w h
  where
    w = imageWidth src
    h = imageHeight src
    fn xPx yPx =
      let x = (fromIntegral xPx / fromIntegral (w-1))
          y = 1-(fromIntegral yPx / fromIntegral (h-1))
          lonlat = pInv (XYCoord x y)
      in
      if validLonLat lonlat
        then srcPixel src lonlat
        else PixelRGBA8 0 0 0 0

validLonLat :: LonLat -> Bool
validLonLat (LonLat lam phi) =
  lam >= -pi && lam <= pi && phi >= -pi/2 && phi <= pi/2

_validXYCoord :: XYCoord -> Bool
_validXYCoord (XYCoord x y) = x >= 0 && x <= 1 && y >= 0 && y <= 1

isValidP :: Projection -> Bool
isValidP (Projection _label p pInv) = and
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
      in not (validLonLat lonlat) || eqLonLat lonlat lonlat2
          || trace (show (lonlat, lonlat2)) False

moveBottomP :: Double -> Projection -> Projection
moveBottomP offset (Projection label p pInv) = Projection label p' pInv'
  where
    p' (LonLat lon lat) =
      case p (LonLat lon lat) of
        XYCoord x y -> XYCoord x (fromToS offset 1 y)
    pInv' (XYCoord x y) = pInv (XYCoord x ((y-offset)/(1-offset)))

moveTopP :: Double -> Projection -> Projection
moveTopP offset = flipYAxisP . moveBottomP offset . flipYAxisP

flipYAxisP :: Projection -> Projection
flipYAxisP (Projection label p pInv) = Projection label p' pInv'
  where
    p' (LonLat lam phi) =
      let XYCoord x y = p (LonLat lam (negate phi))
      in XYCoord x (1-y)
    pInv' (XYCoord x y) =
      let LonLat lam phi = pInv (XYCoord x (1-y))
      in LonLat lam (negate phi)

scaleP :: Double -> Double -> Projection -> Projection
scaleP xScale yScale (Projection label p pInv) = Projection label forward inverse
  where
    forward lonlat =
      case p lonlat of
        XYCoord x y -> XYCoord ((x-0.5)*xScale+0.5) ((y-0.5)*yScale+0.5)
    inverse (XYCoord x y) =
      pInv $ XYCoord ((x-0.5)/xScale+0.5) ((y-0.5)/yScale+0.5)


mergeP :: Projection -> Projection -> Double -> Projection
mergeP p1 p2 t = Projection (projectionLabel p1 ++ "/" ++ projectionLabel p2) p pInv
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
equirectangularP = Projection "equirectangular" forward inverse
  where
    forward (LonLat lam phi) = XYCoord ((lam+pi)/tau) ((phi+pi/2)/pi)
    inverse (XYCoord x y) = LonLat xPi yPi
      where
        xPi = fromToS (-pi) pi x
        yPi = fromToS (-pi/2) (pi/2) y

-- | <<docs/gifs/doc_mercatorP.gif>>
mercatorP :: Projection
mercatorP = Projection "mercator" forward inverse
  where
    forward (LonLat lam phi) =
      XYCoord ((lam+pi)/tau)
        (min 1 $ max (0) $ (((log(tan(pi/4+phi/2))) + pi)/tau))
    inverse (XYCoord x y) = LonLat xPi (atan (sinh yPi))
      where
        xPi = fromToS (-pi) pi x
        yPi = fromToS (-pi) pi y

thetas :: V.Vector Double
thetas = V.fromList $
  map (find_theta_fast . fromIndex) [0 .. granularity]

granularity :: Int
granularity = 50000

toIndex :: Double -> Int
toIndex phi = round ((phi+halfPi)/pi * fromIntegral granularity)
fromIndex :: Int -> Double
fromIndex x = fromToS (-halfPi) halfPi (fromIntegral x / fromIntegral granularity)
granualize :: Double -> Double
granualize = fromIndex . toIndex

{-# INLINE mollweideP #-}
-- | <<docs/gifs/doc_mollweideP.gif>>
mollweideP :: Projection
mollweideP = Projection "mollweide" forward inverse
  where
    forward (LonLat !lam !phi) =
        XYCoord ((x+sqrt2*2)/(4*sqrt2)) ((y+sqrt2)/(2*sqrt2))
      where
        x = (2*sqrt2)/pi * lam * cos theta
        y = sqrt2*sin theta
        theta = thetas V.! toIndex phi
        -- find_theta :: Int -> Double
        -- find_theta 0 = phi
        -- find_theta _ | abs phi == pi/2 = signum phi * pi/2
        -- find_theta n =
        --   let !sub = find_theta (n-1)
        --   in sub - (2*sub+sin (2*sub)-pi*sin phi)/(2+2*cos(2*sub))
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS (-2*sqrt2) (2*sqrt2) x'
        y = fromToS (-1) 1 y'
        theta = granualize (asin y)
        lam = pi*x/(2*sqrt2*cos theta)
        phi = asin ((2*theta+sin(2*theta))/pi)

find_theta_fast :: Double -> Double
find_theta_fast !phi | abs phi == pi/2 = signum phi * halfPi
find_theta_fast !(D# phi) = go phi
  where
    !(D# pi#) = pi
    go acc =
      let c = cosDouble# (acc +## acc)
          s = sinDouble# (acc +## acc)
          next =
            acc -##
            (acc +## acc +## s -## pi# *## (sinDouble# phi))
            /## (2.0## +## c +## c) in
      if abs (D# (next -## acc)) < epsilon
        then D# next
        else go next


-- | <<docs/gifs/doc_hammerP.gif>>
hammerP :: Projection
hammerP = Projection "hammer" forward inverse
  where
    forward (LonLat lam phi) =
        XYCoord ((x+sqrt2*2)/(4*sqrt2)) ((y+sqrt2)/(2*sqrt2))
      where
        x = (2*sqrt2*cos phi*sin (lam/2))/sqrt (1+cos phi*cos (lam/2))
        y = (sqrt2*sin phi)/ sqrt (1+cos phi*cos (lam/2))
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS (-2*sqrt2) (2*sqrt2) x'
        y = fromToS (-sqrt2) sqrt2 y'
        z = sqrt (1 - (x/4)**2 - (y/2)**2)
        lam = 2 * atan2 (z*x) (2*(2*z**2-1))
        phi = asin (z*y)

cylindricalEqualAreaP :: Double -> Projection
cylindricalEqualAreaP phi0 = Projection "lambert" forward inverse
  where
    cosPhi0 = cos phi0
    forward (LonLat lam phi) =
      XYCoord ((lam+pi)/tau) ((sin phi/cosPhi0+1/cosPhi0)/2/cosPhi0)
    inverse (XYCoord x' y') = LonLat x (asin y / (asin cosPhi0 / halfPi))
      where
        x = fromToS (-pi) pi x'
        y = fromToS (-1) 1 y' * cosPhi0

-- | <<docs/gifs/doc_lambertP.gif>>
lambertP :: Projection
lambertP = Projection "lambert" forward inverse
  where
    forward (LonLat lam phi) =
      XYCoord ((lam+pi)/tau) ((sin phi+1)/2)
    inverse (XYCoord x' y') = LonLat x (asin y)
      where
        x = fromToS (-pi) pi x'
        y = fromToS (-1) 1 y'

-- | <<docs/gifs/doc_bottomleyP.gif>>
bottomleyP :: Double -> Projection
bottomleyP !phi_1 = Projection "bottomley" forward inverse
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
        x = fromToS (-pi) pi x'
        y = fromToS (-pi/2) (pi/2) y'
        x1 = x * sin phi_1
        y1 = pi/2 - y
        rho = sqrt (x1*x1 + y1*y1)
        e = atan2 x1 y1
        lam = (if rho == 0 then 1 else rho / sin rho) * e/sin phi_1
        phi = pi/2 - rho

-- | <<docs/gifs/doc_sinusoidalP.gif>>
sinusoidalP :: Projection
sinusoidalP = Projection "sinusoidal" forward inverse
  where
    forward (LonLat lam phi) =
        XYCoord ((x+pi)/tau) ((y+pi/2)/pi)
      where
        x = lam * cos phi
        y = phi
    inverse (XYCoord x' y') = LonLat (x/cos y) y
      where
        x = fromToS (-pi) pi x'
        y = fromToS (-pi/2) (pi/2) y'

-- | <<docs/gifs/doc_wernerP.gif>>
wernerP :: Projection
wernerP = moveTopP 0.23 $ Projection "werner" forward inverse
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
        x = fromToS (-pi) pi x'
        y = fromToS (-pi/2) (pi/2) y'
        rho = sqrt (x**2 + (pi/2 - y)**2)
        e = atan2 x (pi/2 -y)
        phi = pi/2 - rho
        lam = e * rho / sin rho

-- FIXME: find right scale and position.
-- | <<docs/gifs/doc_bonneP.gif>>
bonneP :: Double -> Projection
bonneP 0 = sinusoidalP
bonneP phi_0 = moveTopP (-0.17*factor) $ scaleP 1 (fromToS 1 0.65 factor) $
    Projection "bonne" forward inverse
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
        x = fromToS (-pi) pi x'
        y = fromToS (-pi/2) (pi/2) y'
        cotPhi0 = cot phi_0
        rho = sqrt (x*x + (cot phi_0 - y)**2)
        -- e = atan2 x (cot phi_1 + phi_1 + y)
        phi = cotPhi0 + phi_0 - rho
        lam = rho / cos phi * atan2 x (cotPhi0-y)

-- | <<docs/gifs/doc_orthoP.gif>>
orthoP :: LonLat -> Projection
orthoP (LonLat lam_0 phi_0) = Projection "ortho" forward inverse
  where
    forward (LonLat lam phi)
        | cosc < 0  =
            let ang = atan2 y x
                xV = cos ang
                yV = sin ang
                xPos = 7/32 + ((xV+1)/2 * 9/16)
            in -- trace (show (x,y)) $
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
        y = fromToS (-1) 1 y'
        lam = wrap (-pi) pi $
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
cassiniP = Projection "cassini" forward inverse
  where
    forward (LonLat lam phi) =
      XYCoord ((asin (cos phi * sin lam)+halfPi)/pi) ((atan2 (tan phi) (cos lam)+pi)/tau)
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS (-halfPi) halfPi x'
        y = fromToS (-pi) pi y'
        lam = atan2 (tan x) (cos y)
        phi = asin (sin y * cos x)


augustP :: Projection
augustP = scaleP 0.70 0.70 $ Projection "august"  forward inverse
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
        xPos = 4 / 3 * x * (3+x2 - 3*y2)
        yPos = 4 / 3 * y * (3 + 3*x2 - y2)
    inverse (XYCoord x' y') = LonLat lam phi
      where
        x = fromToS xLo xHi x' * 3 / 8
        y = fromToS yLo yHi y' * 3 / 8
        x2 = x*x
        y2 = y*y
        s = 1 + x2 + y2
        sin3Eta = sqrt ((s - sqrt (s*s - 4 * y * y)) / 2)
        eta = asin sin3Eta / 3
        xi = if sin3Eta /= 0 then acosh (abs (y / sin3Eta)) / 3 else asinh (abs x) / 3
        cosEta = cos eta
        coshXi = cosh xi
        d = coshXi * coshXi - cosEta * cosEta
        lam = signum x * 2 * atan2 (sinh xi * cosEta) (0.25 - d)
        phi = signum y * 2 * atan2 (coshXi * sin eta) (0.25 + d)

-- | <<docs/gifs/doc_collignonP.gif>>
collignonP :: Projection
collignonP = Projection "collignon" forward inverse
  where
    yHi = sqrtPi
    yLo = sqrtPi * (1 - sqrt2)
    xLo = -pi*(2/sqrtPi)*sqrt2
    xHi = pi*(2/sqrtPi)*sqrt2
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
eckert1P = Projection "eckert1" forward inverse
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
eckert3P = Projection "eckert3" forward inverse
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
eckert5P = Projection "eckert5" forward inverse
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

{-# INLINE faheyP #-}
-- | <<docs/gifs/doc_faheyP.gif>>
faheyP :: Projection
faheyP = Projection "fahey" forward inverse
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

{-# INLINE foucautP #-}
foucautP :: Projection
foucautP = Projection "foucaut" forward inverse
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

{-# INLINE lagrangeP #-}
lagrangeP :: Projection
lagrangeP = Projection "lagrange" forward inverse
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
        | abs (y'-1) < epsilon
          -- = LonLat 0 (signum y * halfPi)
          = LonLat 100 100
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
applyProjection = applyProjection' 1e-2

applyProjection' :: Double -> Projection -> SVG -> SVG
applyProjection' tolerance p = mapSvgLines start
  where
    start (LineMove x:rest) = LineMove (proj x) : worker x rest
    start _                 = []
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
    lowTolerance = tolerance*tolerance

    proj (V2 lam phi) =
      case projectionForward p $ LonLat lam phi of
        XYCoord x y -> V2 x y
    mkChunks a b =
      let midway = lerp 0.5 a b in
      if distance (proj a) (proj b) < tolerance || distance a b < lowTolerance
        then [a, b]
        else mkChunks a midway ++ drop 1 (mkChunks midway b)
