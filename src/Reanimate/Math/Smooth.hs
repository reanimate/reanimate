{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.Math.Smooth
  ( steinerPoints
  , renderMesh
  , renderAMesh
  , smoothMesh
  , smoothStep
  , angleSmooth
  , meshMinAngle
  , splitMeshEdges
  )
where

import qualified Data.IntSet                   as ISet
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.List
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV
import           Linear.V2
import           Linear.Metric
import           Linear.Vector
import           Reanimate.Animation
import           Reanimate.Constants
import           Reanimate.Math.Common
import           Reanimate.Math.Polygon
import           Reanimate.Math.Render
import           Reanimate.Math.Triangulate
import           Reanimate.Morph.Rigid
import           Reanimate.Svg

steinerPoints :: Polygon -> [Polygon] -> [V2 Rational]
steinerPoints outerPolygon = concatMap
  (V.toList . V.filter isSteiner . polygonPoints)
  where isSteiner p = V.notElem p (polygonPoints outerPolygon)
-- compatiblyTriangulateP :: Polygon -> Polygon -> [(Polygon, Polygon)]

renderMesh :: Polygon -> [Polygon] -> SVG
renderMesh outerPolygon innerPolygons = mkGroup
  [ mkGroup
    [ withFillOpacity 1
      $ withStrokeWidth (defaultStrokeWidth * 0)
      $ withStrokeColor "black"
      $ withFillColor "lightgreen"
      $ polygonShape p
    | p <- innerPolygons
    ]
  , mkGroup
    [ withFillColor "red" $ aroundCenter (scale 0.2) $ drawPoint
        (realToFrac <$> p)
    | p <- steiners
    ]
  ]
  where steiners = steinerPoints outerPolygon innerPolygons

renderAMesh :: Mesh -> SVG
renderAMesh m = mkGroup
  [ translate (-1.5) 0 $ renderMesh
    (mkPolygon outlineA)
    [ mkPolygon $ V.fromList $ map (fmap realToFrac)
                                   [pA V.! a, pA V.! b, pA V.! c]
    | (a, b, c) <- V.toList (meshTriangles m)
    ]
  , translate (1.5) 0 $ renderMesh
    (mkPolygon outlineB)
    [ mkPolygon $ V.fromList $ map (fmap realToFrac)
                                   [pB V.! a, pB V.! b, pB V.! c]
    | (a, b, c) <- V.toList (meshTriangles m)
    ]
  ]
 where
  pA       = meshPointsA m
  pB       = meshPointsB m
  outlineA = V.map (\i -> realToFrac <$> meshPointsA m V.! i) (meshOutline m)
  outlineB = V.map (\i -> realToFrac <$> meshPointsB m V.! i) (meshOutline m)


drawPoint :: V2 Double -> SVG
drawPoint (V2 x y) = translate x y $ mkCircle 0.1


-- Plot angles for a mesh.
-- Plot min-angles for meshes as they are iteratively smoothed.

-- Smoothing algorithm:
--   For each steiner point:
--      Find desired position from each vertex, take average, update position.
--   For each edge:
--     Does flipping it increase min-angle in both meshes?
--       If yes, do it.
--   For long edges:
--     Cut in half:
--       Cut the two connected faces in half.
-- all points
-- steiner points
--

{-
data Mesh = Mesh
  { meshPointsA :: Vector P
  , meshPointsB :: Vector P
  , meshOutline :: Vector Int
  -- , meshSteiner :: Vector Int
  , meshTriangles :: Vector RelTrig }

-}
smoothMesh :: Mesh -> [Mesh]
smoothMesh m = runST $ do
  trigs <- edgesToTriangulationM (length $ meshPointsA m) $ concat
    [ [(a, b), (b, c), (c, a)] | (a, b, c) <- V.toList (meshTriangles m) ]
  pA <- V.thaw $ V.map (fmap realToFrac) (meshPointsA m)
  pB <- V.thaw $ V.map (fmap realToFrac) (meshPointsB m)
  replicateM 120 $ do
    smoothStep trigs pA steiner
    smoothStep trigs pB steiner
    newA <- V.freeze pA
    newB <- V.freeze pB
    return $ m { meshPointsA = newA, meshPointsB = newB }
 where
  steiner = V.fromList
    [ i | i <- [0 .. length (meshPointsA m) - 1], V.notElem i (meshOutline m) ]


smoothStep
  :: forall s
   . V.MVector s [Int]
  -> V.MVector s (V2 Double)
  -> V.Vector Int
  -> ST s ()
smoothStep triangulation pts steiner =
  forM_ [0 .. length steiner - 1] {-length steiner-1-}
                                  $ \s_i -> do
    let i = steiner V.! s_i
    pt    <- MV.read pts i
    edges <- V.fromList <$> (sortEdges pt =<< MV.read triangulation i)
    let angleBased = angleSmooth pt edges
        laplacian  = sum edges ^/ (fromIntegral $ length edges) -- laplacian
    -- unsafeIOToST $ hPrint stderr (pt, edges, newX)
    if isValidLocation pt edges angleBased
      then MV.write pts i angleBased
      else if isValidLocation pt edges laplacian
        then MV.write pts i laplacian
        else return ()
 where
  sortEdges :: V2 Double -> [Int] -> ST s [V2 Double]
  sortEdges pt edges = do
    edgePoints <- mapM (MV.read pts) edges
    return $ sortOn (dir pt) edgePoints
  -- Direction from south of 'a', to 'a', to 'b'.
  dir :: V2 Double -> V2 Double -> Double
  dir a b = (atan2 (crossZ (V2 0 1) (b - a)) (dot (V2 0 1) (b - a)))

angleSmooth :: V2 Double -> V.Vector (V2 Double) -> V2 Double
angleSmooth origin js = V.sum (V.generate n nth) ^/ V.sum (V.generate n factor)
 where
  n = length js
  factor i =
    let n_self   = js V.! i
        n_origin = origin - n_self
        n_prev   = js V.! mod (i - 1) n - n_self
        n_next   = js V.! mod (i + 1) n - n_self
        a1       = acos (dot n_origin n_next / (norm n_origin * norm n_next))
        a2       = acos (dot n_origin n_prev / (norm n_origin * norm n_prev))
        alpha    = a1 + a2
    in  recip (alpha * alpha)
  nth i =
    let V2      x         y = origin
        n_self@(V2 x_0 y_0) = js V.! i
        n_origin            = origin - n_self
        n_prev              = js V.! mod (i - 1) n - n_self
        n_next              = js V.! mod (i + 1) n - n_self
        a1 = acos (dot n_origin n_next / (norm n_origin * norm n_next))
        a2 = acos (dot n_origin n_prev / (norm n_origin * norm n_prev))
        alpha               = a1 + a2
        b                   = (a2 - a1) / 2
        x'                  = x_0 + (x - x_0) * cos b - (y - y_0) * sin b
        y'                  = y_0 + (x - x_0) * sin b + (y - y_0) * cos b
    in  V2 x' y' ^/ (alpha * alpha)

isValidLocation :: V2 Double -> V.Vector (V2 Double) -> V2 Double -> Bool
isValidLocation origin edges newLoc =
  or
      [ isInside origin a b newLoc
      | i <- [0 .. length edges - 1]
      , let a = edges V.! i
            b = edges V.! mod (i + 1) (length edges)
      ]
    && V.toList edges
    == sortOn (dir newLoc) (V.toList edges)
    && minAngle origin edges
    <  minAngle newLoc edges
 where
  dir :: V2 Double -> V2 Double -> Double
  dir a b = (atan2 (crossZ (V2 0 1) (b - a)) (dot (V2 0 1) (b - a)))

minAngle :: V2 Double -> V.Vector (V2 Double) -> Double
minAngle origin edges = minimum $ concat
  [ [a1, a2, a3]
  | i <- [0 .. length edges - 1]
  , let a            = edges V.! i
        b            = edges V.! mod (i + 1) (length edges)
        (a1, a2, a3) = triangleAngles origin a b
  ]

meshMinAngle :: Mesh -> (Double, Double)
meshMinAngle m =
  ( minimum $ concat
    [ [a1, a2, a3]
    | (a, b, c) <- V.toList (meshTriangles m)
    , let (a1, a2, a3) = triangleAngles (meshPointsA m V.! a)
                                        (meshPointsA m V.! b)
                                        (meshPointsA m V.! c)
    ]
  , minimum $ concat
    [ [a1, a2, a3]
    | (a, b, c) <- V.toList (meshTriangles m)
    , let (a1, a2, a3) = triangleAngles (meshPointsB m V.! a)
                                        (meshPointsB m V.! b)
                                        (meshPointsB m V.! c)
    ]
  )

-- Smooth:
--   Find all edges
--   Sort counter-clockwise from 12 o'clock. (ie direction from south)
--   Compute new position.
--   Update position.

{-
data Mesh = Mesh
  { meshPointsA :: Vector P
  , meshPointsB :: Vector P
  , meshOutline :: Vector Int
  -- , meshSteiner :: Vector Int
  , meshTriangles :: Vector RelTrig }
-}
-- edge: a to b
-- opposite points: c and d
-- delete edge (a,b)
-- insert point p
-- insert edge (a,p)
-- insert edge (p,b)
-- insert edge (c,p)
-- insert edge (p,d)
splitMeshEdges :: Double -> Mesh -> Mesh
splitMeshEdges maxLen mesh = runST $ do
  t <- MV.replicate (n * 2) []
  forM_ (V.toList (meshTriangles mesh)) $ \(a, b, c) -> do
    insertEdge t a b
    insertEdge t b c
    insertEdge t a c
  forM_ [0 .. n - 1] $ \i -> MV.modify t (ISet.toList . ISet.fromList) i
  triangulation <- V.freeze t
  newPoints     <- newSTRef []
  offset        <- newSTRef n
  forM_ [0 .. n - 1] $ \i -> forM_ (triangulation V.! i) $ \j ->
    when (i < j) $ when (maxEdgeDistanceSquared i j > maxLen * maxLen) $ do
      p <- readSTRef offset
      writeSTRef offset (p + 1)
      let (p1, p2) = splitEdge i j
      modifySTRef newPoints ((p1, p2) :)
      let [c, d] = (triangulation V.! i) `intersect` (triangulation V.! j)
      deleteEdge t i j
      insertEdge t i p
      insertEdge t j p
      insertEdge t c p
      insertEdge t d p
  return undefined
 where
  splitEdge i j =
    ( lerp 0.5 (meshPointsA mesh V.! i) (meshPointsA mesh V.! j)
    , lerp 0.5 (meshPointsB mesh V.! i) (meshPointsB mesh V.! j)
    )
  maxEdgeDistanceSquared i j =
    distSquared (meshPointsA mesh V.! i) (meshPointsA mesh V.! j)
      `max` distSquared (meshPointsB mesh V.! i) (meshPointsB mesh V.! j)
  n = length (meshPointsA mesh)
  deleteEdge v a b = do
    MV.modify v (delete b) a
    MV.modify v (delete a) b
  insertEdge v a b = do
    MV.modify v (b :) a
    MV.modify v (a :) b
  -- sortEdges :: V.Vector (V2 Double) -> V2 Double -> V2 Double -> [Int] -> [Int]
  -- sortEdges v prev pt = sortOn (\i -> dir prev pt (v V.! i))

  -- dir :: V2 Double -> V2 Double -> V2 Double -> Double
  -- dir l a b = atan2 (crossZ (l - a) (b - a)) (dot (l - a) (b - a))
