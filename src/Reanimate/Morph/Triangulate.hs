module Reanimate.Morph.Triagulate where

-- import Graphics.SvgTree
import Linear.V2
import Linear.Vector
-- import Reanimate
import Numeric.LinearAlgebra
-- import Numeric.LinearAlgebra.Sparse
-- import Data.Sparse.SpMatrix
-- import Data.Sparse.SpVector

type Poly = [V2 Double]

-- renderPolygon :: Poly -> SVG
-- renderPolygon = undefined

triangulate :: Poly -> Poly -> ([Poly],[Poly])
triangulate = undefined

type Point = V2 Double

p1,p2,p3,p4 :: Point
p1 = V2 0 0
p2 = V2 2 0
p3 = V2 1 2
p4 = V2 1 1

-- let (t1,t2,t3) = barycentricCoordinates p1 p2 p3 p4
-- p4 = t1*p1 + t2*p2 + t3*p3
--
--
--                       [ t1
--                       , t2
--                       , t3 ]
-- [ [p1_x, p2_x, p3_x ] [ p4_x
-- , [p1_y, p2_y, p3_y ] , p4_y
-- , [1, 1, 1] ]         , 1 ]
-- barycentricCoordinates :: Point -> Point -> Point -> Point -> (Double, Double, Double)
barycentricCoordinates (V2 p1x p1y) (V2 p2x p2y) (V2 p3x p3y) (V2 p4x p4y) =
    linearSolve m v
    -- (m, v)
  where
    m = (3><3) [p1x, p2x, p3x, p1y, p2y, p3y, 1, 1, 1 :: Double]
    v = (3><1) [ p4x, p4y, 1 :: Double ]

bCoords (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) (V2 x y) =
    (lam1, lam2, lam3)
  where
    lam1 = ((y2-y3)*(x-x3) + (x3 - x2)*(y-y3)) /
           ((y2-y3)*(x1-x3) + (x3-x2)*(y1-y3))
    lam2 = ((y3-y1)*(x-x3) + (x1-x3)*(y-y3)) /
           ((y2-y3)*(x1-x3) + (x3-x2)*(y1-y3))
    lam3 = 1 - lam1 - lam2
