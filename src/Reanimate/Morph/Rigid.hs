{-# LANGUAGE RecordWildCards #-}
{-
The shape of a triangle can be represented by two explicit points (ie vectors)
and a third implicit point at (0,0). A triangle defined by three points can be
converted by subtracting one of the points from the others:
  shape (p1,p2,p3) = (p2-p1, p3-p1)
This keeps the shape intact but drops the position of the triangle.

If P and Q are two such triangles, there exists a matrix A that transforms
P into Q: AP = Q
The 'A' matrix can be calculated by multiplying Q and the inverse of P:
  A = QP_inv

So, if we have a set of triangles, we can drop their positional information
and compute their shape transformation matrices. But can we do it in reverse?
Can we start with a set of transformation matrices (which don't give us the
positions of the triangles) and compute a set of points that represent triangles
with the right transformation?
In other words, if we have a set of triangles (with absolute position) and
a transformation matrix for each triangle, can we compute new absolute positions
such that AP=Q?

Solving this is not straightforward as moving a single point will change the
transformation of multiple triangles. Fortunately, though, this problem can be
expression as linear equations and solved quickly with LAPACK.

To express the problem as a linear system, we need 4 equations for each
triangle and 2 unknowns for each point.

The system has this shape: Mx=B
We're trying to find 'x' which are the positions of the new points.
'M' is a matrix that transforms points into transformation matrices.
'B' is the target transformation matrices.

With a bit of rewriting, we can reduce 'A = QP_inv' to 4 linear equations
with 6 unknowns. The 6 unknowns are the x and y positions for the three corners
in a triangle.
This is just another way of calculating the transformation matrix 'A' and we
can verify it by looking at the output of:
  * computeA src dst
  * applyCoeff (coeffOfB src) dst
The output should be the same because they're both computing 'A'.

'applyCoeff' simply multiplies the coefficients by a 6x1 matrix containing
the x and y positions for 'dst'.

So, this means we have roughly the right shape: Mx=B. M is the coefficients
we found for 'src', 'x' is dst, and 'B' is the transformation matrix that
turns 'src' into 'dst'.
We can get the coefficients for each triangle and place them together in a
single large matrix. Then we can put the desired transformation matrices in 'B'
and ask a linear solver to find 'x' which satisfies the equations.

Phew, step one is done. We now have the machinery for turning arbitrary
transformation matrices into proper, connected triangles. Next step is figuring
out which rotational matrices to use. At the two extremes, we have the identity
matrices (representing our starting shape) and the matrices computed from the
target triangles (represetning out target shape). How can these two matrices be
interpolated for a smooth morph?

There are several ways of interpolating matrices. Linear interpolation is
possible. But, it looks prettier if we separate rotation from shear. Then we
can use spherical linear interpolation on the rotation and linear interpolation
on the shear.

-}
module Reanimate.Morph.Rigid where

import           Control.Lens
import           Data.Foldable                 (toList)
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import           Linear.Quaternion
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import qualified Numeric.LinearAlgebra         as Matrix
import           Numeric.LinearAlgebra.HMatrix (GMatrix, Matrix, toLists, (!),
                                                (><))
import           Reanimate.Animation
import           Reanimate.Math.Compatible     (compatiblyTriangulateP)
import qualified Reanimate.Math.DCEL           as DCEL
import           Reanimate.Math.Polygon        (mkPolygon, pCopy,
                                                polygonPoints)
import           Reanimate.Morph.Common
import           Reanimate.Svg

type P = V2 Double
type Trig = (P,P,P)
type RelTrig = (Int,Int,Int)

data Mesh = Mesh
  { meshPointsA   :: Vector P
  , meshPointsB   :: Vector P
  , meshOutline   :: Vector Int
  -- , meshSteiner :: Vector Int
  , meshTriangles :: Vector RelTrig }

renderMeshPair :: Mesh -> SVG
renderMeshPair Mesh{..} = withStrokeColor "black" $ mkGroup
  [ mkLinePathClosed
    [ (aPx, aPy)
    , (bPx, bPy)
    , (cPx, cPy)]
  | (a,b,c) <- V.toList meshTriangles
  , let V2 aPx aPy = meshPointsA V.! a
        V2 bPx bPy = meshPointsA V.! b
        V2 cPx cPy = meshPointsA V.! c
  ]

-- applyA (computeA a b) a = b + some_constant_translation
-- A = Q P_inv
computeA :: Trig -> Trig -> Matrix Double
computeA p q = matQ <> Matrix.inv matP
  where
    matP = trigToMatrix p
    matQ = trigToMatrix q

-- A = UDV = U(VV_t)DV = (UV)(V_tDV) = RS
-- R = UV
-- S = V_tDV
computeRS :: Matrix Double -> (Matrix Double, Matrix Double)
computeRS a = (r, s)
  where
    (u,d,vt) = Matrix.svd a
    v = Matrix.tr vt
    r = u <> v
    s = vt <> Matrix.diag d <> v

matrixToQuaternion :: Matrix Double -> Quaternion Double
matrixToQuaternion r =  q
  where
    w = 0.5 * sqrt (1 + r!0!0 + r!1!1 + 1)
    z = 1/(4*w) * (r!0!1 - r!1!0)
    q = Quaternion w (V3 0 0 z)

quaternionToMatrix :: Quaternion Double -> Matrix Double
quaternionToMatrix q = (2><2)
    [ 1 - 2*(qj*qj + qk*qk), 2*(qi*qj+qk*qr)
    , 2*(qi*qj - qk*qr), 1 - 2*(qi*qi + qk*qk) ]
  where
    [qr,qi,qj,qk] = toList q

-- A = R((1-t)I + tS)
computeA_RSt :: Matrix Double -> Matrix Double -> Double -> Matrix Double
computeA_RSt r s t = r_t <> (realToFrac (1-t) * Matrix.ident 2 + realToFrac t * s)
  where
    i = Quaternion 1 0
    q = slerp i (matrixToQuaternion r) t
    r_t = quaternionToMatrix q

applyA :: Matrix Double -> Trig -> Trig
applyA a p =
    case toLists (a <> matP) of
      [ [x1, x2], [y1, y2] ] -> (V2 0 0, V2 x1 y1, V2 x2 y2)
      _                      -> error "invalid matrix"
  where
    matP = trigToMatrix p

coeffOfB :: Trig -> Matrix Double
coeffOfB p = (4><6)
    [ -a0-a2,      0, a0,   0, a2,  0
    , -a1-a3,      0, a1,   0, a3,  0
    ,      0, -a0-a2,  0,  a0,  0, a2
    ,      0, -a1-a3,  0,  a1,  0, a3 ]
  where
    [[a0,a1],[a2,a3]] = toLists (Matrix.inv (trigToMatrix p))

applyCoeff :: Matrix Double -> Trig -> Matrix Double
applyCoeff b (V2 x1 y1, V2 x2 y2, V2 x3 y3) = b <> matQ
  where
    matQ = (6><1) [ x1, y1, x2, y2, x3, y3]

trigToMatrix :: Trig -> Matrix Double
trigToMatrix (p1,p2,p3) = matP
  where
    V2 p12x p12y = p2-p1
    V2 p13x p13y = p3-p1
    matP = (2><2)
      [p12x, p13x
      ,p12y, p13y]



data Prep = Prep
  { prepPivot   :: (P, P)
  , prepPointsA :: Vector P
  , prepPointsB :: Vector P
  , prepRS      :: Vector (Matrix Double, Matrix Double)
  , prepRSRev   :: Vector (Matrix Double, Matrix Double)
  , prepUToB    :: GMatrix
  }

symmetric :: Bool
symmetric = True

prepare :: Mesh -> Prep
prepare Mesh{..} = Prep
    { prepPivot = (aOrigin, bOrigin)
    , prepPointsA =
        V.map (subtract aOrigin) $
        V.take pivotIdx meshPointsA <> V.drop (pivotIdx+1) meshPointsA
    , prepPointsB =
        V.map (subtract bOrigin) $
        V.take pivotIdx meshPointsB <> V.drop (pivotIdx+1) meshPointsB
    , prepRS = rsList
    , prepRSRev = rsRevList
    , prepUToB = Matrix.mkSparse uToB }
  where
    aOrigin = meshPointsA V.! pivotIdx
    bOrigin = meshPointsB V.! pivotIdx
    pivotIdx = 0
    -- pivotIdx = case V.head meshTriangles of
    --     (a,_,_) -> a
    mkAbs p (a,b,c) = (p V.! a,p V.! b,p V.! c)
    absATrigs = V.map (mkAbs meshPointsA) meshTriangles
    absBTrigs = V.map (mkAbs meshPointsB) meshTriangles
    aList = V.zipWith computeA absATrigs absBTrigs
    rsList = V.map computeRS aList
    revList = V.zipWith computeA absBTrigs absATrigs
    rsRevList = V.map computeRS revList
    n = length meshTriangles
    -- nT = if symmetric then n*2 else n
    -- pivotToB = ((nT*4)><2) $ concat
    --   [ [ fromMaybe 0 (lookup (x,pivotIdx*2) bigM)
    --     , fromMaybe 0 (lookup (x,pivotIdx*2+1) bigM)]
    --   | x <- [0..(nT*4)-1] ]
    uToB =
      [ ((x,y-2),key) | ((x,y),key) <- bigM, y /= pivotIdx*2 && y /= pivotIdx*2+1 ]
    bigM =
      concat (zipWith worker [0..] (V.toList meshTriangles)) ++
      if symmetric
        then concat $ zipWith workerRev [n..] (V.toList meshTriangles)
        else []
    worker i src@(a,b,c) = concat $
      let effs = coeffOfB (mkAbs meshPointsA src) in
      [ [((i*4+h, e*2), effs!h!(j*2))
        ,((i*4+h, e*2+1), effs!h!(j*2+1))]
      | h <- [0..3]
      , (e,j) <- zip [a,b,c] [0..]
      ]
    workerRev i dst@(a,b,c) = concat $
      let effs = coeffOfB (mkAbs meshPointsB dst) in
      [ [((i*4+h, e*2), effs!h!(j*2))
        ,((i*4+h, e*2+1), effs!h!(j*2+1))]
      | h <- [0..3]
      , (e,j) <- zip [a,b,c] [0..]
      ]

interpolate :: Prep -> Double -> Vector P
interpolate Prep{..} t = V.fromList $
    pivot : worker (Matrix.toList solution)
  where
    -- solution = Matrix.cgSolve False prepUToB b
    solution = Matrix.cgx $ last solutions
    solutions = Matrix.cgSolve'
      False
      1e-9
      1e-9
      10000
      prepUToB
      b
      (Matrix.fromList $ concat [ [x,y] | V2 x y <- V.toList target ])
      -- 0
    target = if t < 0.5
      then prepPointsA
      else prepPointsB
    worker (x:y:xs) = V2 x y ^+^ pivot : worker xs
    worker _        = []
    pivot = case prepPivot of
      (src, dst) -> lerp t dst src
    n = V.length prepRS
    b = Matrix.vector $
      [ concat (toLists a)!!j
      | i <- [0..n-1]
      , let (r,s) = prepRS V.! i
      , let a = computeA_RSt r s t
      , j <- [0..3]
      ] ++
      [ concat (toLists a)!!j
      | symmetric
      , i <- [0..n-1]
      , let (r,s) = prepRSRev V.! i
      , let a = computeA_RSt r s (1-t)
      , j <- [0..3]
      ]

toRigidMesh :: DCEL.Mesh (V2 Double) -> DCEL.Mesh (V2 Double) -> Mesh
toRigidMesh meshA meshB = Mesh
    { meshPointsA = pointsA
    , meshPointsB = pointsB
    , meshOutline = outline
    , meshTriangles = trigs }
  where
    pointsA = V.fromList $ map DCEL._vertexPosition $ M.elems (meshA^.DCEL.meshVertices)
    pointsB = V.fromList $ map DCEL._vertexPosition $ M.elems (meshB^.DCEL.meshVertices)
    outline = V.fromList
      [ fromJust (V.elemIndex v pointsA)
      | eid <- DCEL.faceEdges (meshA^.DCEL.meshOuterFace) meshA
      , let edge = DCEL.meshGetEdge eid meshA
            v = DCEL._vertexPosition (DCEL.meshGetVertex (edge^.DCEL.edgeVertex) meshA)
      ]
    trigs = V.fromList
      [ (aIdx, bIdx, cIdx)
      | fid <- M.keys (meshA^.DCEL.meshFaces)
      , fid /= (meshA^.DCEL.meshOuterFace)
      , let edges = map (`DCEL.meshGetEdge` meshA) (DCEL.faceEdges fid meshA)
            vs = map (`DCEL.meshGetVertex` meshA) $ map DCEL._edgeVertex edges
            ps = map DCEL._vertexPosition vs
            [aIdx, bIdx, cIdx] = map (fromJust . (`V.elemIndex` pointsA)) ps
            -- p = mkPolygon (V.fromList $ map (fmap realToFrac) ps)
      -- , pIsSimple p || error "invalid polygon"
      ]


{-# INLINE rigidMorph #-}
rigidMorph :: Trajectory
rigidMorph (p1', p2') = \t ->
    let points = V.map (fmap realToFrac) $ interpolate optPrep t
    in mkPolygon (V.map (\i -> points V.! i) (meshOutline optMesh))
  where
    p1 = pCopy p1'
    p2 = pCopy p2'
    (p1s,p2s) = unzip (compatiblyTriangulateP p1 p2)
    m2 = DCEL.buildMesh $ DCEL.polygonsMesh
         (map (fmap realToFrac) $ V.toList $ polygonPoints p2)
         (map (map (fmap realToFrac) . V.toList . polygonPoints) p2s)

    m1 = DCEL.buildMesh $ DCEL.polygonsMesh
          (map (fmap realToFrac) $ V.toList $ polygonPoints p1)
          (map (map (fmap realToFrac) . V.toList . polygonPoints) p1s)
    
    optPrep   = prepare optMesh
    optMesh   = toRigidMesh  m1final m2final
    pipeline1 = last . take 20 . iterate 
      (uncurry DCEL.delaunayFlip .
      uncurry DCEL.splitInternalEdges . 
      (\(a,b) -> (DCEL.meshSmoothPosition a, DCEL.meshSmoothPosition b)))
    (m1final, m2final) = last $ take 20 $ iterate 
      (pipeline1 .
        uncurry DCEL.splitLongestEdge .
        pipeline1
        ) (m1,m2)

