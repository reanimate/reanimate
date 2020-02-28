#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
module Main where

import Reanimate
import Linear.V2
import Linear.V3
import Linear.Matrix
import Linear.Vector
import Data.List
import Data.Tuple
import qualified Data.Text as T

main :: IO ()
main = reanimate $ addStatic (mkBackground "black") $
  staticFrame 1 $ lowerTransformations $ scale 3 $ pathify $ center $
  drawTrig points1 group1


points1 =
  [ V2 1 0
  , V2 (-1/2) (sqrt 3 / 2)
  , V2 (-1/2) (-sqrt 3 / 2)
  , 3 * points1!!0 ^/ 4
  , 3 * points1!!1 ^/ 4
  , 3 * points1!!2 ^/ 4
  , points1!!0 ^/ 2
  , points1!!1 ^/ 2
  , points1!!2 ^/ 2
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
    , translate ax ay $ mkCircle 0.03
    , translate bx by $ mkCircle 0.03
    , translate cx cy $ mkCircle 0.03
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
      in translate 0 (svgHeight svg/2) svg

area (V2 a1 a2) (V2 b1 b2) (V2 c1 c2) =
  1/2 * det33 (V3 (V3 1 1 1) (V3 a1 b1 c2) (V3 a2 b2 c2))

-- Anticlockwise. No duplicate vertices. length >= 3
type Polygon = [V2 Double]
type P = V2 Double

lineIntersect :: (P,P) -> (P,P) -> P
lineIntersect (V2 x1 y1,V2 x2 y2) (V2 x3 y3, V2 x4 y4) =
    V2 (xTop/xBot) (yTop/yBot)
  where
    xTop = (x1*y2 - y1*x2)*(x3-x4) - (x1 - x2)*(x3*y4-y3*x4)
    xBot = (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)
    yTop = (x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x3*y4-y3*x4)
    yBot = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)

isBetween :: P -> (P,P) -> Bool
isBetween (V2 x y) (V2 x1 y1, V2 x2 y2) =
  (y1 > y) /= (y2 > y) && -- y is between y1 and y2
  (x1 > x) /= (x2 > x) ||
  V2 x y == V2 x1 y1 ||
  V2 x y == V2 x2 y2


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
  [ (a,c)
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

calcMu points groups = concat
    [ [ (nP, nP, t1)
      , (a, nP, t2)
      , (b, nP, t3) ]
    | p <- [1..length points]
    , let selfVert = points !! (p-1)
          n = findStarNeighbours groups p
          nPoints = nub $ map fst n ++ map snd n
    , isInteriorNeighbours n
    , nP <- nPoints
    , let vert = points !! (nP-1)
    , let line = (vert, selfVert)
    , (a,b) <- n
    , let aP = points !! (a-1)
          bP = points !! (b-1)
          segment = (points !! (a-1), points !! (b-1))
          cross = lineIntersect line segment
    , isBetween cross segment
    , a /= nP
    , b /= nP
    , let (t1,t2,t3) = bCoords vert aP bP selfVert
    ]
  where

{-
What we want is 'lambda i j'.
'i' is interior node. 'j' is neighbour.



-}
