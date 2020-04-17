{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Reanimate.Morph.LeastWork
  ( StretchCosts(..)
  , defaultStretchCosts
  , zeroStretchCosts
  , stretchWork
  , BendCosts(..)
  , defaultBendCosts
  , bendWork
  , bendInfo
  , leastWork
  , anyLeastWork
  , rawLeastWork
  , leastWork'
  ) where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Array
import           Data.Array.Base
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Array.Unsafe
import           Data.Hashable
import           Data.Maybe
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as VU
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           Reanimate.LaTeX
import           Reanimate.Math.Common
import           Reanimate.Morph.Cache
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear
import           System.IO
import           Text.Printf

import           Debug.Trace

data StretchCosts = StretchCosts
  { stretchStiffness       :: Double
  , stretchCollapsePenalty :: Double
  , stretchElasticity      :: Double
  }

instance Hashable StretchCosts where
  hashWithSalt salt StretchCosts{..} =
    hashWithSalt salt (stretchStiffness, stretchCollapsePenalty, stretchElasticity)

defaultStretchCosts :: StretchCosts
defaultStretchCosts = StretchCosts
  { stretchStiffness       = 1
  , stretchCollapsePenalty = 2
  , stretchElasticity      = 2 }

zeroStretchCosts :: StretchCosts
zeroStretchCosts = defaultStretchCosts{ stretchStiffness = 0 }

-- stretch work paramters:
--   stretch stiffness
--   collapse penalty
--   exponent
stretchWork :: StretchCosts -> Double -> Double -> Double
stretchWork _ 0 0 = 0
stretchWork StretchCosts{..} lenStart lenEnd = checkNaN $
    stretchStiffness * (delta**stretchElasticity)/
    ((1-c)*lenMin + c*lenMax)
  where
    checkNaN v
      | isNaN v   = 0
      | otherwise = v
    c = recip stretchCollapsePenalty
    lenMin = min lenStart lenEnd
    lenMax = max lenStart lenEnd
    delta = lenEnd-lenStart


data BendCosts = BendCosts
  { bendStiffness        :: Double
  , bendElasticity       :: Double
  , bendDeviationPenalty :: Double
  , bendZeroPenalty      :: Double
  }

instance Hashable BendCosts where
  hashWithSalt salt BendCosts{..} =
    hashWithSalt salt
      (bendStiffness
      ,bendElasticity
      ,bendDeviationPenalty
      ,bendZeroPenalty)

defaultBendCosts :: BendCosts
defaultBendCosts = BendCosts
  { bendStiffness        = 1
  , bendElasticity       = 2
  , bendDeviationPenalty = 10
  , bendZeroPenalty      = 1000 }

type PType = Double

bendWork :: BendCosts -> V2 PType -> V2 PType -> V2 PType -> V2 PType -> Double
bendWork BendCosts{..} startA endA startB endB =
  case bendInfo startA endA startB endB of
    (deltaAngle, deviation, throughZero) ->
      (bendStiffness * (deltaAngle + bendDeviationPenalty * deviation))**bendElasticity +
      if throughZero then bendZeroPenalty else 0

deltaX :: Fractional a => V2 a
deltaX = V2 (recip 1000000000) 0

testBendInfo :: Polygon -> Polygon -> [Int] -> [Int] -> (Double, Double, Bool)
testBendInfo a b [a1,a2,a3] [b1,b2,b3] =
  let startA = realToFrac <$> (a V.! a1 - a V.! a2)
      endA   = realToFrac <$> (b V.! b1 - b V.! b2)
      endB   = realToFrac <$> (b V.! b3 - b V.! b2)
      startB = realToFrac <$> (a V.! a3 - a V.! a2)
  in bendInfo startA endA startB endB

zeroBendFactor :: Fractional a => a
zeroBendFactor = 1

vZero :: (Ord a, Fractional a) => V2 a -> Bool
vZero (V2 x y) = abs x < epsilon && abs y < epsilon

bendInfo :: V2 PType -> V2 PType -> V2 PType -> V2 PType -> (Double, Double, Bool)
-- bendInfo startA endA startB endB
--   | vZero startA, not (vZero endA) = bendInfo (endA ^* zeroBendFactor) endA startB endB
--   | vZero endA, not (vZero startA) = bendInfo startA (startA ^* zeroBendFactor) startB endB
--   | vZero startB, not (vZero endB) = bendInfo startA endA (endB ^* zeroBendFactor) endB
--   | vZero endB, not (vZero startB) = bendInfo startA endA startB (startB ^* zeroBendFactor)
bendInfo startA endA startB endB =
    -- trace (show $ map (fmap realToFrac) [startA, endA, startB, endB]) $
    -- trace (show (realToFrac <$> q0, realToFrac <$> q1, realToFrac <$> q2)) $
    -- (ang4, alpha+beta, throughZero)
    (ang4, alpha+beta, throughZero)
  where
    !x0 = dot startA startB
    !y0 = crossZ startA startB
    !x1 = (dot endA startB + dot startA endB) / 2
    !y1 = (crossZ endA startB + crossZ startA endB) / 2
    !x2 = dot endA endB
    !y2 = crossZ endA endB
    fudge x | vZero x = V2 (-0.1) 0
    fudge x = x
    q0,q1, q2 :: V2 PType
    !q0 = fudge $ V2 x0 y0
    !q1 = V2 x1 y1
    !q2 = fudge $ V2 x2 y2
    curve n = (q0^*(squared (1-n)) + q1^*(2*n*(1-n)) + q2^*squared n)
    squared a = a*a
    ang3 = abs (angleR q0 q2)
    ang4 = if isWrap then 2*pi - ang3 else ang3
    d0 = crossZ q0 q1
    d1 = crossZ q0 q2 / 2
    d2 = crossZ q1 q2
    roots = filter (>0) $ filter (<1) $ quadraticRoot (d0+d2-2*d1) (2*d1-2*d0) d0
    alpha = fromMaybe 0 $ listToMaybe
      [ min q0Angle q2Angle - ang
      | root <- roots
      , let ang = angleR (V2 1 0) (curve root)
      , ang < min q0Angle q2Angle  ]
    beta = fromMaybe 0 $ listToMaybe
      [ ang - max q0Angle q2Angle
      | root <- roots
      , let ang = angleR (V2 1 0) (curve root)
      , ang > max q0Angle q2Angle  ]
    q0Angle = angleR (V2 1 0) q0
    q2Angle = angleR (V2 1 0) q2
    isWrap = d1*d1 - d0*d2 < 0 && isInside q0 q1 q2 (V2 0 0)
    throughZero = quadThroughZero q0 q1 q2

angleR :: V2 Double -> V2 Double -> Double
angleR a b = atan2 (realToFrac $ crossZ a b) (realToFrac $ dot a b)

leastWork :: StretchCosts -> BendCosts -> PointCorrespondence
leastWork stretchCosts bendCosts =
    cachePointCorrespondence key (leastWork_ stretchCosts bendCosts)
  where
    key = hash ("leastWork v0"::String, stretchCosts, bendCosts)

leastWork_ :: StretchCosts -> BendCosts -> PointCorrespondence
leastWork_ stretchCosts bendCosts src dst
  | length src > length dst =
    case leastWork stretchCosts bendCosts dst src of
      (dst', src') -> (src', dst')
leastWork_ stretchCosts bendCosts src dst =
    worker undefined (-1) options
  where
    worker bestP _bestPScore [] =
      --trace ("\nBest score: " ++ show _bestPScore)
      bestP
    worker bestP bestPScore (x:xs) =
      let (src',dst', newScore) = leastWork' stretchCosts bendCosts src x in
      if newScore < bestPScore || bestPScore < 0
        then worker (src',dst') newScore xs
        else worker bestP bestPScore xs
    options = cyclePolygons dst
    -- options = [cyclePolygons dst !! 79 ]

anyLeastWork :: StretchCosts -> BendCosts -> PointCorrespondence
anyLeastWork stretchCosts bendCosts src dst =
  case closestLinearCorrespondence src dst of
    (src', dst') -> case leastWork' stretchCosts bendCosts src' dst' of
      (src'', dst'', _score) -> (src'', dst'')

rawLeastWork :: StretchCosts -> BendCosts -> PointCorrespondence
rawLeastWork stretchCosts bendCosts src dst =
  case leastWork' stretchCosts bendCosts src dst of
    (src', dst', _score) -> (src', dst')

-- type PointCorrespondence = Polygon → Polygon → (Polygon, Polygon)
leastWork' :: StretchCosts -> BendCosts -> Polygon -> Polygon -> (Polygon, Polygon, Double)
leastWork' stretchCosts bendCosts src dst =
    leastWork'' stretchCosts bendCosts src dst
      srcV dstV (mkDistances srcV) (mkDistances dstV)
  where
    srcV = V.map (fmap realToFrac) src
    dstV = V.map (fmap realToFrac) dst
    mkDistances poly = VU.generate (length poly) $ \i ->
      distance (V.unsafeIndex poly i) (V.unsafeIndex poly ((i+1) `mod` length poly))

leastWork'' :: StretchCosts -> BendCosts -> Polygon -> Polygon
  -> V.Vector (V2 Double) -> V.Vector (V2 Double)
  -> VU.Vector Double -> VU.Vector Double
  -> (Polygon, Polygon, Double)
leastWork'' stretchCosts bendCosts src dst srcV dstV srcDist dstDist = runST $ do
    -- unsafeIOToST $ hPutStrLn stderr $ "Size: " ++ show (nSrc, nDst)
    -- unsafeIOToST $ hFlush stderr
    work <- asArray $ newArray ((0,0),(nSrc,nDst)) (0::Double)
    north <- asArray $ newArray ((0,0),(nSrc,nDst)) (0::Int)
    west <- asArray $ newArray ((0,0),(nSrc,nDst)) (0::Int)
    let -- calc :: Int -> Int -> ST s ()
        calc = \i j -> do
          let cond = False -- i==14 && j==26
          w0 <- if i==0 then return 0 else do
            prevWork <- readArray work (i-1,j)
            prevWest <- readArray west (i-1,j)
            prevNorth <- readArray north (i-1,j)
            let !len = VU.unsafeIndex srcDist (i-1)
                -- !len = distance (realToFrac <$> pAccess src (i-1)) (realToFrac <$> pAccess src i)
                !sWork = stretchWork stretchCosts len 0
                -- !srcPrev   = realToFrac <$> pAccess src (i-1-prevWest)
                !srcPrev   = V.unsafeIndex srcV (i-1-prevWest)
                -- !srcMiddle = realToFrac <$> pAccess src (i-1)
                !srcMiddle   = V.unsafeIndex srcV (i-1)
                -- !srcNext   = realToFrac <$> pAccess src i
                !srcNext   = V.unsafeIndex srcV i
                -- !dstPrev   = realToFrac <$> pAccess dst (j-prevNorth)
                !dstPrev   = V.unsafeIndex dstV (j-prevNorth)
                -- !dstMiddle = realToFrac <$> pAccess dst j
                !dstMiddle = V.unsafeIndex dstV j
                !bWork =
                  bendWork bendCosts
                    (srcPrev-srcMiddle) (dstPrev-dstMiddle)
                    (srcNext-srcMiddle) (V2 0 0)
            -- when (len' /= len) $ do
            --   error $ "w0: Bad length: " ++ show (len', len, i, j)
            when cond $ do
              unsafeIOToST $ hPutStrLn stderr $ "West:  " ++ show (i-1-prevWest, i-1, i)
              unsafeIOToST $ hPutStrLn stderr $ "West:  " ++ show (j-prevNorth, j, j)
              unsafeIOToST $ hPutStrLn stderr $ "bWork: " ++ show (bWork)
              unsafeIOToST $ hPutStrLn stderr $ "len:    " ++ show (len)
              unsafeIOToST $ hPutStrLn stderr $ "sWork:  " ++ show (sWork)
            return (prevWork + sWork + bWork + if prevWest==0&&prevNorth==1 then 10000 else 0)
          w1 <- if j==0 then return 0 else do
            prevWork <- readArray work (i,j-1)
            prevWest <- readArray west (i,j-1)
            prevNorth <- readArray north (i,j-1)
            let len = VU.unsafeIndex dstDist (j-1)
                --len = distance (realToFrac <$> pAccess dst (j-1)) (realToFrac <$> pAccess dst j)
                sWork = stretchWork stretchCosts 0 len
                -- srcPrev   = realToFrac <$> pAccess src (i-prevWest)
                -- srcMiddle = realToFrac <$> pAccess src i
                -- dstPrev   = realToFrac <$> pAccess dst (j-1-prevNorth)
                -- dstMiddle = realToFrac <$> pAccess dst (j-1)
                -- dstNext   = realToFrac <$> pAccess dst j
                srcPrev   = V.unsafeIndex srcV (i-prevWest)
                srcMiddle = V.unsafeIndex srcV i
                dstPrev   = V.unsafeIndex dstV (j-1-prevNorth)
                dstMiddle = V.unsafeIndex dstV (j-1)
                dstNext   = V.unsafeIndex dstV j
                bWork =
                  bendWork bendCosts
                      (srcPrev-srcMiddle) (dstPrev-dstMiddle)
                      (V2 0 0) (dstNext-dstMiddle)
            -- when (len' /= len) $ do
            --   error $ "w1: Bad length: " ++ show (len', len, j, realToFrac <$> pAccess dst (j-1),  realToFrac <$> pAccess dst j)
            when (isNaN bWork) $
              error $ "bWork NaN: " ++ show (i,j)
            when (isNaN sWork) $
              error $ "sWork NaN: " ++ show (i,j, len)
            when cond $ do
              unsafeIOToST $ hPutStrLn stderr $ "North:  " ++ show (i-prevWest, i, i)
              unsafeIOToST $ hPutStrLn stderr $ "North:  " ++ show (j-1-prevNorth, j-1, j)
              unsafeIOToST $ hPutStrLn stderr $ "bWork:  " ++ show (bWork)
              unsafeIOToST $ hPutStrLn stderr $ "len:    " ++ show (len)
              unsafeIOToST $ hPutStrLn stderr $ "sWork:  " ++ show (sWork)
            return (prevWork + sWork + bWork + if prevWest==1&&prevNorth==0 then 10000 else 0)
          w2 <- if j==0 || i==0 then return 0 else do
            prevWork <- readArray work (i-1,j-1)
            prevWest <- readArray west (i-1,j-1)
            prevNorth <- readArray north (i-1,j-1)
            -- let findPrev key = do
            --       keyNorth <- readArray north
            let -- startLen = distance (realToFrac <$> pAccess src (i-1)) (realToFrac <$> pAccess src i)
                -- endLen = distance (realToFrac <$> pAccess dst (j-1)) (realToFrac <$> pAccess dst j)
                startLen = VU.unsafeIndex srcDist (i-1)
                endLen = VU.unsafeIndex dstDist (j-1)
                sWork = stretchWork stretchCosts startLen endLen
                -- srcPrev   = realToFrac <$> pAccess src (i-1-prevWest)
                -- srcMiddle = realToFrac <$> pAccess src (i-1)
                -- srcNext   = realToFrac <$> pAccess src i
                -- dstPrev   = realToFrac <$> pAccess dst (j-1-prevNorth)
                -- dstMiddle = realToFrac <$> pAccess dst (j-1)
                -- dstNext   = realToFrac <$> pAccess dst j
                srcPrev   = V.unsafeIndex srcV (i-1-prevWest)
                srcMiddle = V.unsafeIndex srcV (i-1)
                srcNext   = V.unsafeIndex srcV i
                dstPrev   = V.unsafeIndex dstV (j-1-prevNorth)
                dstPrev'   = V.unsafeIndex dstV (j-2)
                dstMiddle = V.unsafeIndex dstV (j-1)
                dstNext   = V.unsafeIndex dstV j
                bWork =
                  bendWork bendCosts
                    (srcPrev-srcMiddle) (dstPrev-dstMiddle)
                    (srcNext-srcMiddle) (dstNext-dstMiddle)
                bWork' =
                  bendWork bendCosts
                    (srcPrev-srcMiddle) (dstPrev'-dstMiddle)
                    (srcNext-srcMiddle) (dstNext-dstMiddle)
            when cond $ do
              unsafeIOToST $ hPutStrLn stderr $ "Diag:  " ++ show (i-1-prevWest, i-1, i)
              unsafeIOToST $ hPutStrLn stderr $ "Diag:  " ++ show (j-1-prevNorth, j-1, j)
              unsafeIOToST $ hPutStrLn stderr $ "bWork: " ++ show (bWork)
              -- unsafeIOToST $ hPutStrLn stderr $ "bWork': " ++ show (bWork')
            return (prevWork + sWork + bWork)
          let goNorth = do
                when cond $ do
                  unsafeIOToST $ hPutStrLn stderr "Go north"
                  unsafeIOToST $ hPrint stderr (w0, w1, w2)
                writeArray work (i,j) w1
                writeArray north (i,j) 1
              goWest = do
                when cond $ do
                  unsafeIOToST $ hPutStrLn stderr "Go west"
                  unsafeIOToST $ hPrint stderr (w0, w1, w2)
                writeArray work (i,j) w0
                writeArray west (i,j) 1
              goNorthWest = do
                when cond $
                  unsafeIOToST $ hPutStrLn stderr "Go northwest"
                writeArray work (i,j) w2
                writeArray north (i,j) 1
                writeArray west (i,j) 1
          -- when (j==2 && i==2) $
          --   unsafeIOToST $ hPrint stderr (i,j,w0, w1, w2)
          when (isNaN w0 || isNaN w1 || isNaN w2) $
            error $ "NaN: " ++ show (w0,w1,w2, i, j)
          if | i==0                 -> goNorth
             | j==0                 -> goWest
             | w2 <= w0 && w2 <= w1 -> goNorthWest
             | w0 <= w1 && w0 <= w2 -> goWest
             | w1 <= w0 && w1 <= w2 -> goNorth
             | otherwise            -> error $ "urk: " ++ show (w0, w1, w2, i, j)
    forM_ [1.. max nSrc nDst-1] $ \idx -> do
      forM_ [0..idx-1] $ \i -> do
        when (idx < nSrc && i < nDst) $
          calc idx i
        when (i < nSrc && idx < nDst) $
          calc i idx
      when (idx < nSrc && idx < nDst) $
        calc idx idx
    -- workMatrix <- unsafeFreeze work
    -- unsafeIOToST $ do
    --   hPutStr stderr "   "
    --   forM_ [0..nSrc-1] $ \i -> do
    --     hPutStr stderr (printf "%6d " i)
    --   hPutStr stderr "\n"
    --   forM_ [0..nDst-1] $ \j -> do
    --     hPutStr stderr (printf "%2d " j)
    --     forM_ [0..nSrc-1] $ \i -> do
    --       hPutStr stderr (printf "%6.1f " (workMatrix ! (i,j)))
    --     hPutStr stderr "\n"
    finalWork <- readArray work (nSrc-1,nDst-1)
    let walk = \i j acc -> do
          w <- readArray work (i,j)
          -- when (i==14) $
          --   unsafeIOToST $ hPrint stderr (i,j)
          let acc' = (i,j):acc
          if (i==0 && j==0)
            then return acc'
            else do
              isNorth <- (==1) <$> readArray north (i,j)
              isWest  <- (==1) <$> readArray west (i,j)
              if | isNorth && isWest -> walk (i-1) (j-1) acc'
                 | isNorth           -> walk i (j-1) acc'
                 | otherwise         -> walk (i-1) j acc'
    pairs <- walk (nSrc-1) (nDst-1) []
    return
      ( V.fromList [ pAccess src idx | idx <- map fst pairs ]
      , V.fromList [ pAccess dst idx | idx <- map snd pairs ]
      , finalWork
      )
  where
    nSrc = length src
    nDst = length dst
    asArray :: ST s (STUArray s i e) -> ST s (STUArray s i e)
    asArray = id

{-
quadToCubic (QuadBezier a b c) =
  CubicBezier a ((1/3)*^(a ^+^ 2*^b)) ((1/3)*^(2*^b ^+^ c)) c

cubicRoot (p3 - 3*p2 + 3*p1 - p0) (3*p2 - 6*p1 + 3*p0) (3*p1 - 3*p0) p0
  where (CubicBezier (Point p0 _) (Point p1 _) (Point p2 _) (Point p3 _)) = b

p3 = a
p2 = ((1/3)*^(a ^+^ 2*^b))
p1 = ((1/3)*^(2*^b ^+^ c))
p0 = c
cubicRoot 0 (a - 2b + c) (2b - 2c) c
-}

quadThroughZero :: V2 Double -> V2 Double -> V2 Double -> Bool
quadThroughZero a@(V2 _ q0) b@(V2 _ q1) c@(V2 _ q2) =
    any xPositive $ map eval $ filter (\x -> x > 0 && x < 1) extrema
  where
    xPositive (V2 x _) = x>0
    eval n = a^*(squared (1-n)) + b^*(2*n*(1-n)) + c^*squared n
    squared x = x*x
    extrema = quadraticRoot (q2-2*q1+q0) (2*q1-2*q0) (q0)

quadraticRoot :: Double -> Double -> Double -> [Double]
quadraticRoot a b c
  | a == 0 && b == 0 = []
  | a == 0 = [-c/b]
  | otherwise = result
  where
    d = b*b - 4*a*c
    bSign | b < 0 = -1
          | otherwise = 1
    q = - (b + bSign * sqrt d) / 2
    x1 = q/a
    x2 = c/q
    result | d < 0     = []
           | d == 0    = [x1]
           | otherwise = [x1, x2]
