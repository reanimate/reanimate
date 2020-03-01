#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
module Main where

import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad
import           Data.List                     (delete, nub, partition)
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Tuple
import qualified Data.Vector                   as V
import           Debug.Trace
import           Linear.Matrix                 hiding (trace)
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Numeric.LinearAlgebra         hiding (scale, (<>))
import qualified Numeric.LinearAlgebra         as Matrix
import           Numeric.LinearAlgebra.HMatrix hiding (scale, (<>))
import           Reanimate

type Points = V.Vector (V2 Double)
type Edges = [(Int, Int, Int)]
data Mesh = Mesh { meshPoints :: Points, meshEdges :: Edges }
data MeshPair = MeshPair Points Points Edges
-- The points in a RelMesh are:
--   relMeshStatic ++ x where Ax = B
data RelMesh = RelMesh
  { relMeshStatic :: Points
  , relMeshEdges  :: Edges
  , relMeshA      :: Matrix Double
  , relMeshB      :: Matrix Double
  }
data RelMeshPair = RelMeshPair Points Edges (Matrix Double) (Matrix Double) (Matrix Double) (Matrix Double)
-- Linear interpolation on RelMesh gives smooth morph.
-- solveMesh :: RelMesh -> Mesh
-- mkRelative :: Mesh -> RelMesh
-- mkRelativePair :: MeshPair -> RelMeshPair
-- triangulate :: Polygon -> Polygon -> MeshPair ?
-- embed :: MeshPair -> MeshPair
-- compatible :: Mesh -> Mesh -> Maybe MeshPair
-- linearInterpolate :: MeshPair -> Double -> Mesh
-- convexInterpolate :: RelMeshPair -> Double -> RelMesh

main :: IO ()
main = reanimate $ playThenReverseA $ pauseAround 1 1 $ addStatic (mkBackground "black") $
  signalA (curveS 2) $ mkAnimation 5 $ \t -> lowerTransformations $ scale 3 $ pathify $ center $
  mkGroup
  [ translate 1.7 0 $ drawTrig example1
  -- , translate (-1.7) 0 $ drawTrig points2 group1
  , translate (-1.7) 0 $ drawTrig (linearInterpolate meshPair t)
  , drawTrig $ solveMesh $ convexInterpolate (mkRelativePair meshPair) t
  ]
  where
    meshPair = fromJust $ compatible example1 example2

-- FIXME: Check that the triangles are all anticlockwise.
-- FIXME: Check that the edges connect all the points.
-- FIXME: Check that the edgse leave no gaps.
compatible :: Mesh -> Mesh -> Maybe MeshPair
compatible a b =
  if meshEdges a == meshEdges b && V.length (meshPoints a) == V.length (meshPoints b)
    then Just $ MeshPair (meshPoints a) (meshPoints b) (meshEdges a)
    else Nothing

linearInterpolate :: MeshPair -> Double -> Mesh
linearInterpolate (MeshPair aP bP edges) t = Mesh
    { meshPoints = V.zipWith (lerp (1-t)) aP bP
    , meshEdges = edges }

example1 :: Mesh
example1 = Mesh points edges
  where
    points = V.fromList
      [ V2 1 0
      , V2 (-1/2) (sqrt 3 / 2)
      , V2 (-1/2) (-sqrt 3 / 2)
      , 3 * points V.! 0 ^/ 4
      , 3 * points V.! 1 ^/ 4
      , 3 * points V.! 2 ^/ 4
      , points V.! 0 ^/ 2
      , points V.! 1 ^/ 2
      , points V.! 2 ^/ 2
      ]
    edges =
      [ (1,5,4), (1,2,5), (2,6,5), (2,3,6), (3,4,6), (3,1,4)
      , (4,8,7), (4,5,8), (5,9,8), (5,6,9), (6,7,9), (6,4,7), (7,8,9)]

example2 :: Mesh
example2 = Mesh points edges
  where
    points = V.fromList
      [ V2 1 0
      , V2 (-1/2) (sqrt 3 / 2)
      , V2 (-1/2) (-sqrt 3 / 2)
      , 3 * points V.! 2 ^/ 4
      , 3 * points V.! 0 ^/ 4
      , 3 * points V.! 1 ^/ 4
      , points V.! 1  ^/ 2
      , points V.! 2 ^/ 2
      , points V.! 0 ^/ 2
      ]
    edges = meshEdges example1

drawTrig (Mesh points gs) = withStrokeColor "grey" $ withFillColor "white" $
  withStrokeWidth (defaultStrokeWidth/2) $ mkGroup
  [ mkGroup
    [ mkGroup
      [ mkLine (ax, ay) (bx, by)
      , mkLine (bx, by) (cx, cy)
      , mkLine (cx, cy) (ax, ay)
      ]
    | (a, b, c) <- gs
    , let V2 ax ay = points V.! (a-1)
          V2 bx by = points V.! (b-1)
          V2 cx cy = points V.! (c-1)
    ]
  , mkGroup $ concat
    [ [ colored v $ translate ax ay $ mkCircle circR
      , withStrokeWidth (defaultStrokeWidth*0.0) $
        withStrokeColor "white" $ withFillColor "black" $ mkGroup
        [ translate ax ay $ ppNum v ]
      ]
    | v <- nub $ concat [ [a,b,c] | (a,b,c) <- gs]
    , let V2 ax ay = points V.! (v-1)
    ]]
  where
    colored n =
      let c = promotePixel $ turbo (fromIntegral n / fromIntegral (length gs-1))
      in withStrokeColorPixel c . withFillColorPixel c
    circR = 0.05
    ppNum n = scaleToHeight (circR*1.5) $ center $ latex $ T.pack $ "\\texttt{" ++ show n ++ "}"

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

isInterior :: Eq a => [(a, a)] -> Bool
isInterior = isJust . getExteriorPoly

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

convexInterpolate :: RelMeshPair -> Double -> RelMesh
convexInterpolate (RelMeshPair static edges leftM leftB rightM rightB) t =
  RelMesh
  { relMeshStatic = static
  , relMeshEdges  = edges
  , relMeshA      = Matrix.scale (1-t) leftM +
                    Matrix.scale t rightM
  , relMeshB      = Matrix.scale (1-t) leftB +
                    Matrix.scale t rightB
  }

solveMesh :: RelMesh -> Mesh
solveMesh (RelMesh static edges m b) =
  case linearSolve m b of
    Nothing -> error "Failed to solve mesh"
    Just ret ->
      Mesh (static <> V.fromList (worker (toLists ret))) edges
  where
    worker []             = []
    worker ([x]:[y]:rest) = V2 x y : worker rest
    worker _              = error "invalid result"

mkRelative :: Mesh -> RelMesh
mkRelative (Mesh points edges) = RelMesh (V.fromList exteriorPoints) edges mM bM
  where
    mM = (s><s) (concat m)
    bM = (s><1) b
    (s,exterior, (m, b)) = toParameters points edges
    exteriorPoints =
      [ points V.! (i-1)
      | i <- exterior
      ]

mkRelativePair :: MeshPair -> RelMeshPair
mkRelativePair (MeshPair p1 p2 edges) =
  let RelMesh static edges leftM leftB = mkRelative (Mesh p1 edges)
      RelMesh _ _ rightM rightB = mkRelative (Mesh p1 edges)
  in RelMeshPair static edges leftM leftB rightM rightB

toParameters points groups = (length interior*2,exterior,unzip $ concat
  [ let lst = [(if i == j then -1 else t)
              | j <- interior
              , let t = fromMaybe 0 $ lookup (i,j) lam_ij_cache
              ]
        pos = negate $ sum
           [ pj ^* t
           | j <- exterior
           , let t = fromMaybe 0 $ lookup (i,j) lam_ij_cache
                 pj = points V.! (j-1)
           ]
    in [ (dupX lst, pos ^. _x)
       , (dupY lst, pos ^. _y)]
  | i <- interior ])
  where
    lam_ij_cache = lam_ij points groups
    dupX []     = []
    dupX (x:xs) = x:0:dupX xs
    dupY []     = []
    dupY (x:xs) = 0:x:dupY xs
    (interior, exterior) =
      partition (isInterior . findStarNeighbours groups) [1 .. length points]

lam_ij points groups =
  [ ((i, j), t)
  | i <- [1..V.length points]
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
      let selfVert = points V.! (p-1)
          n = findStarNeighbours groups p
          nPoints :: [Int]
          nPoints = fromMaybe [] $ getExteriorPoly n
    , (i, nP) <- zip [1 .. ] nPoints
    , let vert = points V.! (nP-1)
    , let line = (vert, selfVert)
    , let (a,b,aP,bP) = head $
            [ (a,b,aP,bP)
            | (a,b) <- n
            , let aP = points V.! (a-1)
                  bP = points V.! (b-1)
                  segment = (points V.! (a-1), points V.! (b-1))
                  cross = lineIntersect line segment
            , isBetween cross segment
            , a /= nP
            , b /= nP ]
    -- , b == (nPoints ++ nPoints) !! i
    , let (t1,t2,t3) = bCoords vert aP bP selfVert
    ]
