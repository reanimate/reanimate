#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
module Main where

import Control.Lens
import Data.Maybe
import Control.Monad
import Reanimate
import Linear.V2
import Linear.V3
import Linear.Matrix hiding (trace)
import Linear.Vector
import Numeric.LinearAlgebra hiding (scale)
import Numeric.LinearAlgebra.HMatrix hiding (scale)
import Data.List (delete)
import Data.Tuple
import Debug.Trace
import qualified Data.Text as T

main :: IO ()
main = reanimate $ addStatic (mkBackground "black") $
  mkAnimation 10 $ \t -> lowerTransformations $ scale 3 $ pathify $ center $
  mkGroup
  [ translate 1.7 0 $ drawTrig points1 group1
  , translate (-1.7) 0 $ drawTrig points2 group1
  , translate 0 0 $ drawTrig (zipWith (lerp t) points1 points2) group1
  ]

sqrt3 = 97/56
points1 =
  [ V2 1 0
  , V2 (-1/2) (sqrt3 / 2)
  , V2 (-1/2) (-sqrt3 / 2)
  , 3 * points1!!0 ^/ 4
  , 3 * points1!!1 ^/ 4
  , 3 * points1!!2 ^/ 4
  , points1!!0 ^/ 2
  , points1!!1 ^/ 2
  , points1!!2 ^/ 2
  ]
points2 =
  [ V2 1 0
  , V2 (-1/2) (sqrt3 / 2)
  , V2 (-1/2) (-sqrt3 / 2)
  , 3 * points2!!2 ^/ 4
  , 3 * points2!!0 ^/ 4
  , 3 * points2!!1 ^/ 4
  , points2!!1 ^/ 2
  , points2!!2 ^/ 2
  , points2!!0 ^/ 2
  ]
group1 =
  [ (1,5,4), (1,2,5), (2,6,5), (2,3,6), (3,4,6), (3,1,4)
  , (4,8,7), (4,5,8), (5,9,8), (5,6,9), (6,7,9), (6,4,7), (7,8,9)]

mesh1 :: [(P,P,P)]
mesh1 =
  [ (points1 !! (a-1), points1 !! (b-1), points1 !! (c-1))
  | (a, b, c) <- group1
  ]

drawTrig points gs = withStrokeColor "white" $ withFillColor "red" $
  withStrokeWidth (defaultStrokeWidth/2) $ mkGroup
  [ mkGroup
    [ mkLine (ax, ay) (bx, by)
    , mkLine (bx, by) (cx, cy)
    , mkLine (cx, cy) (ax, ay)
    , translate ax ay $ mkCircle 0.02
    , translate bx by $ mkCircle 0.02
    , translate cx cy $ mkCircle 0.02
    , withStrokeWidth (defaultStrokeWidth/8) $ withFillColor "green" $ mkGroup
      [ translate ax ay $ ppNum a
      , translate bx by $ ppNum b
      , translate cx cy $ ppNum c ]
    ]
  | (a, b, c) <- gs
  , let V2 ax ay = points!!(a-1)
        V2 bx by = points!!(b-1)
        V2 cx cy = points!!(c-1)
  ]
  where
    ppNum n =
      let svg = scale 0.2 $ center $ latex $ T.pack $ "\\texttt{" ++ show n ++ "}"
      in translate 0 (svgHeight svg/2 + 0.04) svg

area (V2 a1 a2) (V2 b1 b2) (V2 c1 c2) =
  1/2 * det33 (V3 (V3 1 1 1) (V3 a1 b1 c2) (V3 a2 b2 c2))

-- Anticlockwise. No duplicate vertices. length >= 3
type Polygon = [V2 Double]
type P = V2 Double

lineIntersect :: Fractional a => (V2 a,V2 a) -> (V2 a,V2 a) -> V2 a
lineIntersect (V2 x1 y1,V2 x2 y2) (V2 x3 y3, V2 x4 y4) =
    V2 (xTop/xBot) (yTop/yBot)
  where
    xTop = (x1*y2 - y1*x2)*(x3-x4) - (x1 - x2)*(x3*y4-y3*x4)
    xBot = (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)
    yTop = (x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x3*y4-y3*x4)
    yBot = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)

isBetween :: Ord a => V2 a -> (V2 a,V2 a) -> Bool
isBetween (V2 x y) (V2 x1 y1, V2 x2 y2) =
  ((y1 > y) /= (y2 > y) || y==y1||y==y2) && -- y is between y1 and y2
  ((x1 > x) /= (x2 > x) || x==x1||x==x2)


isConvex :: Polygon -> Bool
isConvex vertices = and
  [ area (vertices!!i) (vertices!!j) (vertices!!k) > 0
  | i <- [0..n-1]
  , j <- [i+1..n-1]
  , k <- [j+1..n-1]
  ]
  where n = length vertices

-- T = (U, G)
-- G = [Polygon]
-- U = nub $ concat G

bCoords (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) (V2 x y) =
    (lam1, lam2, lam3)
  where
    lam1 = ((y2-y3)*(x-x3) + (x3 - x2)*(y-y3)) /
           ((y2-y3)*(x1-x3) + (x3-x2)*(y1-y3))
    lam2 = ((y3-y1)*(x-x3) + (x1-x3)*(y-y3)) /
           ((y2-y3)*(x1-x3) + (x3-x2)*(y1-y3))
    lam3 = 1 - lam1 - lam2



findStarNeighbours :: Eq a => [(a,a,a)] -> a -> [(a, a)]
findStarNeighbours allTrig self =
  [ (b,c)
  | (a,b,c) <- allTrig
  , self == a
  ] ++
  [ (c,a)
  | (a,b,c) <- allTrig
  , self == b
  ] ++
  [ (a,b)
  | (a,b,c) <- allTrig
  , self == c
  ]

isInteriorNeighbours :: Eq a => [(a, a)] -> Bool
isInteriorNeighbours [] = False
isInteriorNeighbours ((a,b):rest) = worker a b rest
  where
    worker start this [] = start == this
    worker start this xs =
      case lookup this xs of
        Just next -> worker start next (delete (this,next) xs)
        Nothing   ->
          case lookup this (map swap xs) of
            Just next -> worker start next (delete (next, this) xs)
            Nothing   -> False

getExteriorPoly :: Eq a => [(a, a)] -> Maybe [a]
getExteriorPoly [] = Nothing
getExteriorPoly ((a,b):rest) = worker [a] a b rest
  where
    worker acc start this [] = do
      guard (start == this)
      return (reverse acc)
    worker acc start this xs =
      case lookup this xs of
        Just next -> worker (this:acc) start next (delete (this,next) xs)
        Nothing   ->
          case lookup this (map swap xs) of
            Just next -> worker (this:acc) start next (delete (next, this) xs)
            Nothing   -> Nothing

test points group = do
    disp 3 mM
    disp 3 bM
    let Just ret = linearSolve mM bM
    disp 3 ret
    disp 3 (mul mM ret)
  where
    mM = (h><w) (concat m)
    bM = (h><1) b
    h = length m
    w = length (head m)
    (m, b) = unzip $ toParameters points group

toParameters points groups = concat
  [ let lst = [(if i == j then -1 else t)
              | j <- interior
              , let t = lookupLam_ij points groups i j
              ]
        pos = negate $ sum
           [ pj ^* t
           | j <- exterior
           , let t = lookupLam_ij points groups i j
                 pj = points !! (j-1)
           ]
    in [ (dupX lst, pos ^. _x)
       , (dupY lst, pos ^. _y)]
  | i <- interior ]
  where
    dupX [] = []
    dupX (x:xs) = x:0:dupX xs
    dupY [] = []
    dupY (x:xs) = 0:x:dupY xs
    interior = filter (isInteriorNeighbours . findStarNeighbours group1) [1 .. length points1]
    exterior = filter (not . isInteriorNeighbours . findStarNeighbours group1) [1 .. length points1]

lookupLam_ij points groups i j = fromMaybe 0 $ listToMaybe
  [ t
  | (i', j', t) <- lam_ij points groups
  , i==i', j==j'
  ]
lam_ij points groups =
  [ (i, j, t)
  | i <- [1..length points]
  , (j, t) <- lam_j points groups i
  ]
lam_j points groups p =
  [ (nP, sum [ t | (j,k,t) <- mu, j == nP ] / fromIntegral (length nPoints))
  | let n = findStarNeighbours groups p
        nPoints = fromMaybe [] $ getExteriorPoly n
        mu = calcMu points groups p
  , nP <- nPoints ]
calcMu points groups p = concat
    [ [ (nP, nP, t1)
      , (a, nP, t2)
      , (b, nP, t3) ]
      -- (nP, a, b)
    | {-p <- [1..length points]-}
      let selfVert = points !! (p-1)
          n = findStarNeighbours groups p
          nPoints :: [Int]
          nPoints = fromMaybe [] $ getExteriorPoly n
    , (i, nP) <- zip [1 .. ] nPoints
    , let vert = points !! (nP-1)
    , let line = (vert, selfVert)
    , let (a,b,aP,bP) = head $
            [ (a,b,aP,bP)
            | (a,b) <- n
            , let aP = points !! (a-1)
                  bP = points !! (b-1)
                  segment = (points !! (a-1), points !! (b-1))
                  cross = lineIntersect line segment
            , isBetween cross segment
            , a /= nP
            , b /= nP ]
    -- , b == (nPoints ++ nPoints) !! i
    , let (t1,t2,t3) = bCoords vert aP bP selfVert
    ]
  where

{-
What we want is 'lambda i j'.
'i' is interior node. 'j' is neighbour.



-}
