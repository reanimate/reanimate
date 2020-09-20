{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- Code originally from: https://github.com/wereHamster/haskell-half-edge-mesh
module Reanimate.Math.DCEL where

import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import qualified Data.List             as L
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe
import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Linear.Metric
import           Linear.V2
import           Linear.Vector         (lerp, (^/))
import           Reanimate
import           Reanimate.Math.Common (isInsideStrict, lineIntersect,
                                        triangleAngles)
import           Text.Printf
-- import           Debug.Trace

type VertexId = Int
type EdgeId = Int
type FaceId = Int

-- Invariants:
--   No duplicate positions
--   All edges have an opposite twin
--   Edge prev/next keep the same direction (CW or CCW)
--   twin (twin edge) = edge
--   All next/prev links have the same face
data Mesh a = Mesh
    { _meshIdCounter :: Int
    , _meshOuterFace :: FaceId
    , _meshVertices  :: Map VertexId (Vertex a)
    , _meshEdges     :: Map EdgeId Edge
    , _meshFaces     :: Map FaceId Face
    } deriving (Show)

data Vertex a = Vertex
    { _vertexId       :: VertexId
    , _vertexPosition :: a
    -- , _vertexEdge     :: EdgeId -- Index to CCW edge pointing to this vertex.
    } deriving (Show)

data Edge = Edge
    { _edgeId     :: EdgeId
    , _edgeVertex :: VertexId
    , _edgeTwin   :: EdgeId
    , _edgeNext   :: EdgeId
    , _edgePrev   :: EdgeId
    , _edgeFace   :: FaceId
    } deriving (Show)

data Face = Face
    { _faceId   :: FaceId
    , _faceEdge :: EdgeId
    } deriving (Show)

type MeshM position a = State (Mesh position) a

validMesh :: Mesh a -> [String]
validMesh Mesh{..} = execWriter $ do
  return ()

makeLenses ''Mesh
makeLenses ''Vertex
makeLenses ''Edge
makeLenses ''Face

meshGetEdge :: EdgeId -> Mesh a -> Edge
meshGetEdge eid Mesh{..} = M.findWithDefault err eid _meshEdges
  where
    err = error $ "Edge not found: " ++ show eid

meshGetVertex :: VertexId -> Mesh a -> Vertex a
meshGetVertex vid Mesh{..} = M.findWithDefault err vid _meshVertices
  where
    err = error $ "Vertex not found: " ++ show vid

meshGetFace :: FaceId -> Mesh a -> Face
meshGetFace fid Mesh{..} = M.findWithDefault err fid _meshFaces
  where
    err = error $ "Face not found: " ++ show fid

meshAngles :: Mesh (V2 Double) -> [Double]
meshAngles mesh@Mesh{..} =
  [ faceMinAngle fid mesh
  | fid <- M.keys _meshFaces
  ]

meshEdgeAngle :: Mesh (V2 Double) -> EdgeId -> Double
meshEdgeAngle m eId =
    ang
  where
    edge = meshGetEdge eId m
    next = meshGetEdge (edge ^. edgeNext) m
    prev = meshGetEdge (edge ^. edgePrev) m
    v1 = meshGetVertex (prev ^. edgeVertex) m ^. vertexPosition
    v2 = meshGetVertex (edge ^. edgeVertex) m ^. vertexPosition
    v3 = meshGetVertex (next ^. edgeVertex) m ^. vertexPosition
    (_, ang, _) = triangleAngles v1 v2 v3

facePositions :: FaceId -> Mesh a -> [a]
facePositions fid m =
  [ meshGetVertex (edge^.edgeVertex) m ^. vertexPosition
  | eid <- faceEdges fid m
  , let edge = meshGetEdge eid m
  ]

faceEdges :: FaceId -> Mesh a -> [EdgeId]
faceEdges fid m = worker (meshGetEdge lastEdge m ^.edgeNext)
  where
    face = meshGetFace fid m
    lastEdge = face^.faceEdge
    worker eid
      | eid == lastEdge = [eid]
      | otherwise = eid : worker (meshGetEdge eid m ^. edgeNext)

faceMinAngle :: FaceId -> Mesh (V2 Double) -> Double
faceMinAngle fid m = minimum (map (meshEdgeAngle m) (faceEdges fid m))

ppMesh :: Show a => Mesh a -> String
ppMesh Mesh{..} = unlines
  [ printf "Outer face: %d" _meshOuterFace
  , ""
  , "Vertices:"
  , printf "%4s %5s %10s" ("ID"::String) ("Edge"::String) ("Position"::String)
  , unlines
    [ printf "%4d %10s" _vertexId  (show _vertexPosition)
    | Vertex{..} <- M.elems _meshVertices
    ]
  , "Edges:"
  , printf "%4s %3s %4s %4s %4s %4s"
    ("ID"::String) ("Vtx"::String) ("Twin"::String) ("Next"::String)
    ("Prev"::String) ("Face"::String)
  , unlines
    [ printf "%4d %3d %4d %4d %4d %4d" _edgeId _edgeVertex _edgeTwin _edgeNext _edgePrev _edgeFace
    | Edge{..} <- M.elems _meshEdges
    ]
  , "Faces:"
  , unlines
    [ printf "%4d %4d" _faceId _faceEdge
    | Face{..} <- M.elems _meshFaces
    ]
  ]

emptyMesh :: Mesh a
emptyMesh = Mesh 1 0 M.empty M.empty M.empty

-- addFace :: FaceId -> [a] -> MeshM a ()
-- addFace parentFace subPoly = do
--   innerFace <- createFace
--   vIds <- mapM getVertex pts
--   edges     <- forM (zip vIds (tail vIds ++ take 1 vIds)) $ \(v0, v1) -> do
--     e <- do
--       mbEdge <- findEdge' v0 v1
--       case mbEdge of
--         Just e  -> pure (e ^. edgeId)
--         Nothing -> do
--           e <- createEdge v1
--           twin <- createEdge v0
--           setTwinEdge e twin
--           setFace twin parentFace
--           return e
--     setFace e innerFace
--     modifyVertex (vertexEdge .~ e) v0
--     pure e

--   setFaceEdge innerFace (head edges)
--   forM_ (zip edges (tail edges ++ take 1 edges)) $ \(e0, e1) ->
--     linkEdges e0 e1

polygonsMesh :: Eq a => [a] -> [[a]] -> MeshM a ()
polygonsMesh outer trigs = do
  modify $ meshFaces .~ M.empty
  polygonMeshOuter (reverse outer)
  forM_ trigs $ \pts -> do
    innerFace <- createFace
    vIds <- mapM getVertex pts
    edges     <- forM (zip vIds (tail vIds ++ take 1 vIds)) $ \(_v0, v1) -> do
      e <- createEdge v1
      setFace e innerFace
      -- modifyVertex (vertexEdge .~ e) v0
      pure e

    setFaceEdge innerFace (head edges)
    forM_ (zip edges (tail edges ++ take 1 edges)) $ \(e0, e1) ->
      linkEdges e0 e1
    forM edges $ \e -> do
      edge <- getEdge e
      prev <- getEdge (edge ^. edgePrev)
      mbTwin <- findEdge' (edge ^. edgeVertex) (prev ^. edgeVertex)
      case mbTwin of
        Nothing -> return ()
        Just twin ->
          setTwinEdge e (twin^.edgeId)

-- Positions must be in CW order
polygonMesh :: [a] -> MeshM a ()
polygonMesh vs = do
  outerFace <- gets _meshOuterFace
  innerFace <- createFace
  vIds      <- mapM createVertex vs
  edges     <- forM (zip vIds (tail vIds ++ take 1 vIds)) $ \(v0, v1) -> do
    e  <- createEdge v1 -- Edge from v0 to v1
    e' <- createEdge v0 -- Twin edge from v1 to v0
    setFace e  innerFace
    setFace e' outerFace
    -- modifyVertex (vertexEdge .~ e) v0
    setTwinEdge e e'
    pure e

  outerEdge <- _edgeTwin <$> getEdge (head edges)
  setFaceEdge innerFace (head edges)
  setFaceEdge outerFace outerEdge

  forM_ (zip edges (tail edges ++ take 1 edges)) $ \(e0, e1) -> do
    linkEdges e0 e1
    e0' <- _edgeTwin <$> getEdge e0
    e1' <- _edgeTwin <$> getEdge e1
    linkEdges e1' e0'

polygonMeshOuter :: [a] -> MeshM a ()
polygonMeshOuter vs = do
  outerFace <- gets _meshOuterFace
  vIds      <- mapM createVertex vs
  edges     <- forM vIds $ \v0 -> do
    e' <- createEdge v0
    setFace e' outerFace
    pure e'

  setFaceEdge outerFace (head edges)

  let f = Face outerFace (head edges)
  modify $ meshFaces %~ M.insert outerFace f

  forM_ (zip edges (tail edges ++ take 1 edges)) $ \(e0, e1) -> do
    linkEdges e0 e1


newId :: MeshM a Int
newId = do
  counter <- gets _meshIdCounter
  modify $ meshIdCounter %~ succ
  return counter

createVertex :: a -> State (Mesh a) VertexId
createVertex position = do
  k <- newId
  let v = Vertex k position -- (error "_vertexEdge not set")
  modify $ meshVertices %~ M.insert k v
  return k

getVertex :: Eq a => a -> State (Mesh a) VertexId
getVertex position = do
  vs <- gets _meshVertices
  case L.find comparingPosition (M.assocs vs) of
    Nothing     -> createVertex position
    Just (k, _) -> return k
  where comparingPosition (_k, v) = _vertexPosition v == position

requireVertex :: VertexId -> MeshM a (Vertex a)
requireVertex vid = do
  ret <- gets (M.lookup vid . _meshVertices)
  case ret of
    Nothing -> error "Invalid vertex id"
    Just v  -> pure v

modifyVertex :: (Vertex a -> Vertex a) -> VertexId -> State (Mesh a) ()
modifyVertex f k = modify $ meshVertices %~ M.adjust f k

getEdge :: EdgeId -> State (Mesh a) Edge
getEdge e = do
  edges <- gets _meshEdges
  maybe (bug edges) return $ M.lookup e edges
    --return $ fromJust $ M.lookup e edges
  where bug _edges = error $ "Can't find edge with id " ++ show e

withEdge :: EdgeId -> (Edge -> State (Mesh a) ()) -> State (Mesh a) ()
withEdge e f = f =<< getEdge e

-- findEdge :: VertexId -> VertexId -> State (Mesh a) EdgeId
-- findEdge v0 v1 = do
--   edges <- gets _meshEdges
--   case L.find comparingVertices (M.assocs edges) of
--     Nothing     -> error $ "No edge between: " ++ show (v0, v1)
--     Just (k, _) -> return k
--   where comparingVertices (k, e) = (k, _edgeNext e) == (v0, v1)

createEdge :: VertexId -> State (Mesh a) EdgeId
createEdge v = do
  k <- newId
  let e = Edge k
               v
               (error $ "_edgeTwin not set " ++ show (k,v))
               (error $ "_edgeNext not set " ++ show (k,v))
               (error $ "_edgePrev not set " ++ show (k,v))
               (error $ "_edgeFace not set " ++ show (k,v))
  modify $ meshEdges %~ M.insert k e
  return k

modifyEdge :: (Edge -> Edge) -> EdgeId -> State (Mesh a) ()
modifyEdge f k = modify $ meshEdges %~ M.adjust f k

linkEdges :: EdgeId -> EdgeId -> State (Mesh a) ()
linkEdges e0 e1 = do
  setNextEdge e0 e1
  setPreviousEdge e1 e0

setNextEdge :: EdgeId -> EdgeId -> State (Mesh a) ()
setNextEdge e0 e1 = modifyEdge (edgeNext .~ e1) e0

setPreviousEdge :: EdgeId -> EdgeId -> State (Mesh a) ()
setPreviousEdge e0 e1 = modifyEdge (edgePrev .~ e1) e0

setTwinEdge :: EdgeId -> EdgeId -> MeshM a ()
setTwinEdge e0 e1 = do
  modifyEdge (edgeTwin .~ e1) e0
  modifyEdge (edgeTwin .~ e0) e1

setFace :: EdgeId -> FaceId -> State (Mesh a) ()
setFace e0 f0 = modifyEdge (edgeFace .~ f0) e0

updateFaces :: EdgeId -> FaceId -> State (Mesh a) ()
updateFaces e0 f0 = do
  e <- getEdge e0
  setFace e0 f0
  worker (e ^. edgeNext)
 where
  worker e1
    | e0 == e1 = return ()
    | otherwise = do
      e <- getEdge e1
      setFace e1 f0
      worker (e ^. edgeNext)

createFace :: State (Mesh a) FaceId
createFace = do
  k <- newId
  let f = Face k (error "_faceEdge not set")
  modify $ meshFaces %~ M.insert k f
  return k

modifyFace :: (Face -> Face) -> FaceId -> State (Mesh a) ()
modifyFace f k = modify $ meshFaces %~ M.adjust f k

setFaceEdge :: FaceId -> EdgeId -> MeshM a ()
setFaceEdge f0 e0 = modifyFace (\f -> f { _faceEdge = e0 }) f0

deleteFace :: FaceId -> MeshM a ()
deleteFace f0 = modify $ meshFaces %~ M.delete f0

getFace :: FaceId -> State (Mesh a) Face
getFace f = do
  faces <- gets _meshFaces
  maybe bug return (M.lookup f faces)
  where bug = error $ "Can't find face with id " ++ show f


buildMesh :: State (Mesh a) b -> Mesh a
buildMesh f = execState f emptyMesh

numVertices :: Mesh a -> Int
numVertices = M.size . _meshVertices

numEdges :: Mesh a -> Int
numEdges = M.size . _meshEdges

numFaces :: Mesh a -> Int
numFaces = M.size . _meshFaces

vertices :: Mesh a -> [Vertex a]
vertices = M.elems . _meshVertices

outgoingEdges :: VertexId -> Mesh a -> [Edge]
outgoingEdges v mesh =
  [ meshGetEdge (edge ^. edgeTwin) mesh
  | edge <- M.elems (_meshEdges mesh)
  , _edgeVertex edge == v
  ]


-- for each inner node:
--    get all neighbour nodes. sort by CCW
--    compute laplacian
--    compute angle-based
--    check correctness
--    move vertex

--   p    e
-- _ -> u -> v
--   p'   e'
-- _ <- u <- v
--
--   p    e1   e
-- _ -> u -> t -> v
--   p'   e1'  e'
-- _ <- u <- t <- v
--
-- faces stay the same
splitEdge :: VertexId -> EdgeId -> MeshM a EdgeId
splitEdge t e = do
  eEdge     <- getEdge e
  eEdgeTwin <- getEdge (eEdge ^. edgeTwin)

  e1        <- createEdge t
  setFace e1 (eEdge ^. edgeFace)
  -- modifyVertex (vertexEdge .~ e1) t

  e1' <- createEdge (eEdgeTwin ^. edgeVertex)
  setFace e1' (eEdgeTwin ^. edgeFace)
  setTwinEdge e1 e1'

  let oldPrev = eEdge ^. edgePrev
      oldNext = eEdgeTwin ^. edgeNext

  linkEdges oldPrev             e1
  linkEdges (eEdge ^. edgeTwin) e1'
  linkEdges e1                  e
  linkEdges e1'                 oldNext

  modifyEdge (edgeVertex .~ t) (eEdge ^. edgeTwin)

  return e1

splitTriangle :: V2 Double -> EdgeId -> MeshM (V2 Double) (FaceId, FaceId, FaceId, FaceId)
splitTriangle vertex e = do
  -- outer <- gets _meshOuterFace
  edge <- getEdge e
  next <- getEdge (edge^.edgeNext)
  twin <- getEdge (edge^.edgeTwin)
  next' <- getEdge (twin^.edgeNext)

  t         <- createVertex vertex
  _ <- splitEdge t e

  smoothVertex t

  (f1,f2) <- insertEdge (edge^.edgeFace) t (next^.edgeVertex)
  (f3,f4) <- insertEdge (twin^.edgeFace) t (next'^.edgeVertex)
  return (f1,f2,f3,f4)

splitOuterTriangle :: V2 Double -> EdgeId -> MeshM (V2 Double) (FaceId, FaceId)
splitOuterTriangle vertex e = do
  -- outer <- gets _meshOuterFace
  edge <- getEdge e
  twin <- getEdge (edge^.edgeTwin)
  next' <- getEdge (twin^.edgeNext)

  t         <- createVertex vertex
  _ <- splitEdge t e

  (f1,f2) <- insertEdge (twin^.edgeFace) t (next'^.edgeVertex)
  return (f1,f2)
  -- return (outer, outer)

findEdge :: FaceId -> VertexId -> MeshM a Edge
findEdge f v = do
  edges <- gets (M.elems . _meshEdges)
  case
      listToMaybe [ e | e <- edges, e ^. edgeFace == f, e ^. edgeVertex == v ]
    of
      Nothing -> error $ "Edge not found: " ++ show (f, v)
      Just e  -> pure e

findEdge' :: VertexId -> VertexId -> MeshM a (Maybe Edge)
findEdge' v0 v1 = do
  m <- get
  pure $ listToMaybe
    [ e
    | e <- M.elems (m^.meshEdges)
    , e ^. edgeVertex == v1
    , let prev = meshGetEdge (e ^. edgePrev) m
    , prev ^. edgeVertex == v0
    ]

findEdge'' :: [FaceId] -> VertexId -> MeshM a (Maybe Edge)
findEdge'' fs v = do
  edges <- gets (M.elems . _meshEdges)
  pure $
      listToMaybe [ e | e <- edges, e ^. edgeFace `notElem` fs, e ^. edgeVertex == v ]

{-

-}
deleteEdge :: EdgeId -> MeshM a ()
deleteEdge e0 = do
  e <- getEdge e0
  e' <- getEdge (e^.edgeTwin)
  linkEdges (e^.edgePrev) (e'^.edgeNext)
  linkEdges (e'^.edgePrev) (e^.edgeNext)
  updateFaces (e^.edgeNext) (e^.edgeFace)
  setFaceEdge (e^.edgeFace) (e^.edgeNext)
  deleteFace (e'^.edgeFace)
  modify $ meshEdges %~ M.delete (e^.edgeId)
  modify $ meshEdges %~ M.delete (e'^.edgeId)

insertEdge :: FaceId -> VertexId -> VertexId -> MeshM a (FaceId, FaceId)
insertEdge f0 v0 v1 = do
  hv0 <- findEdge f0 v0
  hv1 <- findEdge f0 v1
  f1  <- createFace
  f2  <- createFace
  h1  <- createEdge v1
  h2  <- createEdge v0
  setFaceEdge f1 h1
  setFaceEdge f2 h2
  setTwinEdge h1 h2
  linkEdges (hv0 ^. edgeId) h1
  linkEdges (hv1 ^. edgeId) h2
  linkEdges h1              (hv1 ^. edgeNext)
  linkEdges h2              (hv0 ^. edgeNext)
  updateFaces (hv0 ^. edgeId) f1
  updateFaces (hv1 ^. edgeId) f2
  modify $ meshFaces %~ M.delete f0
  return (f1,f2)

steinerNodes :: Mesh a -> [VertexId]
steinerNodes Mesh{..} =
  [ v
  | v <- M.keys _meshVertices
  , S.notMember v notSteiner
  ]
  where
    notSteiner = S.fromList
      [ _edgeVertex edge
      | edge <- M.elems _meshEdges
      , _edgeFace edge == _meshOuterFace ]

-- findNeighbours :: VertexId -> Mesh a -> [Edge]
-- findNeighbours vertex Mesh{..} =
--   [ edge
--   | edge <- M.elems _meshEdges
--   , _edgeVertex edge == vertex
--   ]

{-
For each internal node:
  * Find neighbours
  * Compute angle-based position
  * Compute laplacian position
  * Update if possible
-}
meshSmoothPosition :: Mesh (V2 Double) -> Mesh (V2 Double)
meshSmoothPosition = execState worker
  where
    worker = gets steinerNodes >>= mapM_ smoothVertex
    -- worker = mapM_ smoothVertex [221]

smoothVertex :: VertexId -> MeshM (V2 Double) ()
smoothVertex steiner = do
    self <- _vertexPosition <$> requireVertex steiner
    es <- gets (outgoingEdges steiner)
    vs <- mapM (requireVertex . _edgeVertex) es
    let ps = V.fromList (map _vertexPosition vs')
        vs' = sortVertices self vs
        angleBased = angleSmooth self ps
        laplacian  = sum ps ^/ fromIntegral (length ps) -- laplacian
    -- trace ("self: " ++ show self) $ return ()
    -- trace ("angleBased: " ++ show angleBased) $ return ()
    -- trace ("Edges: " ++ show (map _vertexId vs')) $ return ()
    -- trace ("Valid angleBased: " ++ show (isValidLocation self ps angleBased)) $ return ()
    -- trace ("Valid laplacian: " ++ show (isValidLocation self ps laplacian)) $ return ()
    -- let checks = [ isInsideStrict self a b angleBased
    --       | i <- [0 .. length ps - 1]
    --       , let a = ps V.! i
    --             b = ps V.! mod (i + 1) (length ps)
    --       ]
    -- trace ("Checks: " ++ show checks) $ return ()
    if isValidLocation self ps angleBased
      then modifyVertex (vertexPosition .~ angleBased) steiner
      else if isValidLocation self ps laplacian
        then modifyVertex (vertexPosition .~ laplacian) steiner
        else return ()
  where
    -- sortEdges :: V2 Double -> [V2 Double] -> [V2 Double]
    -- sortEdges = sortOn . dir
    sortVertices :: V2 Double -> [Vertex (V2 Double)] -> [Vertex (V2 Double)]
    sortVertices self = sortOn (dirV self)
    -- Direction from south of 'a', to 'a', to 'b'.
    dir :: V2 Double -> V2 Double -> Double
    dir a b = atan2 (crossZ (V2 0 1) (b - a)) (dot (V2 0 1) (b - a))
    dirV :: V2 Double -> Vertex (V2 Double) -> Double
    dirV a v = dir a (_vertexPosition v)


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
      [ isInsideStrict origin a b newLoc
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
  dir a b = atan2 (crossZ (V2 0 1) (b - a)) (dot (V2 0 1) (b - a))

isCCW :: (Ord a, Num a) => V2 a -> V2 a -> V2 a -> Bool
isCCW a b c = sum [fn a b, fn b c, fn c a] < 0
  where
    fn (V2 x1 y1) (V2 x2 y2) = (x2-x1)*(y2+y1)

minAngle :: V2 Double -> V.Vector (V2 Double) -> Double
minAngle origin edges = minimum $ concat
  [ [a1, a2, a3]
  | i <- [0 .. length edges - 1]
  , let a            = edges V.! i
        b            = edges V.! mod (i + 1) (length edges)
        (a1, a2, a3) = triangleAngles origin a b
  ]

flipEdge :: Edge -> MeshM (V2 Double) (FaceId, FaceId)
flipEdge e = do
  e' <- getEdge (e^.edgeTwin)

  v0 <- _vertexPosition <$> requireVertex (e^.edgeVertex)
  v1 <- _vertexPosition <$> requireVertex (e'^.edgeVertex)

  a' <- getEdge (e^.edgeNext)
  b' <- getEdge (e'^.edgeNext)

  v0' <- _vertexPosition <$> requireVertex (a'^.edgeVertex)
  v1' <- _vertexPosition <$> requireVertex (b'^.edgeVertex)

  case lineIntersect (v0, v1) (v0', v1') of
    Nothing -> pure (e^.edgeFace, e'^.edgeFace)
    Just{} -> do
      deleteEdge (e^.edgeId)
      insertEdge (e^.edgeFace) (a'^.edgeVertex) (b'^.edgeVertex)

internalEdges :: Mesh a -> [EdgeId]
internalEdges m =
  [ edge^.edgeId
  | edge <- M.elems (m^.meshEdges)
  , edge^.edgeFace /= m^.meshOuterFace
  , let twin = meshGetEdge (edge^.edgeTwin) m
  , edge^.edgeVertex < twin^.edgeVertex
  , twin^.edgeFace /= m^.meshOuterFace
  ]

outerEdges :: Mesh a -> [EdgeId]
outerEdges m =
  [ edge^.edgeId
  | edge <- M.elems (m^.meshEdges)
  , edge^.edgeFace == m^.meshOuterFace
  ]

longestEdge :: Mesh (V2 Double) -> Mesh (V2 Double) -> EdgeId
longestEdge m1 m2 =
    maximumBy cmp (internalEdges m1)
  where
    cmp e1 e2 =
      compare
        (max (edgeLength e1 m1) (edgeLength e1 m2))
        (max (edgeLength e2 m1) (edgeLength e2 m2))

edgeLength :: EdgeId -> Mesh (V2 Double) -> Double
edgeLength eid m =
  let edge = meshGetEdge eid m
      twin = meshGetEdge (edge^.edgeTwin) m
      p1 = _vertexPosition $ meshGetVertex (edge^.edgeVertex) m
      p2 = _vertexPosition $ meshGetVertex (twin^.edgeVertex) m
  in distance p1 p2

applyCompatible :: (t -> Mesh a -> Maybe (Mesh a)) -> (Mesh a -> [t]) -> Mesh a -> Mesh a -> (Mesh a, Mesh a)
applyCompatible _fn _vs m1 m2
  | _meshIdCounter m1 /= _meshIdCounter m2 = error "invalid ID counter"
applyCompatible fn vs m1 m2 = worker m1 m2 (vs m1)
  where
    worker l r [] = (l,r)
    worker l r (eid:rest) =
      case (,) <$> fn eid l <*> fn eid r of
        Nothing       -> worker l r rest
        Just (l', r') -> worker l' r' rest

delaunayFlip :: Mesh (V2 Double) -> Mesh (V2 Double) -> (Mesh (V2 Double), Mesh (V2 Double))
delaunayFlip = applyCompatible delaunayFlip' internalEdges

delaunayFlip' :: EdgeId -> Mesh (V2 Double) -> Maybe (Mesh (V2 Double))
delaunayFlip' eid m =
  let edge = meshGetEdge eid m
      twin = meshGetEdge (edge^.edgeTwin) m
      f1 = edge^.edgeFace
      f2 = twin^.edgeFace
      beforeAng = min (faceMinAngle f1 m) (faceMinAngle f2 m)
      ((f1',f2'), mAfter) = runState (flipEdge edge) m
      afterAng = min (faceMinAngle f1' mAfter) (faceMinAngle f2' mAfter)
  in
  if (afterAng <= beforeAng) || f1' == f1
    then Nothing
    else Just mAfter

splitInternalEdges :: Mesh (V2 Double) -> Mesh (V2 Double) -> (Mesh (V2 Double), Mesh (V2 Double))
splitInternalEdges = applyCompatible splitInternalEdge internalEdges

splitInternalEdge :: EdgeId -> Mesh (V2 Double) -> Maybe (Mesh (V2 Double))
splitInternalEdge eid m = evalState worker m
  where
    edgeFaces edge = do
      twin <- getEdge (edge^.edgeTwin)
      return (edge^.edgeFace, twin^.edgeFace)
    worker = do
      edge <- getEdge eid
      twin <- getEdge (edge^.edgeTwin)
      v0 <- _vertexPosition <$> requireVertex (edge^.edgeVertex)
      v1 <- _vertexPosition <$> requireVertex (twin^.edgeVertex)
      let middle = lerp 0.5 v0 v1
      (f1,f2) <- edgeFaces edge
      mBefore <- get
      let beforeAng = min (faceMinAngle f1 mBefore) (faceMinAngle f2 mBefore)
      (f3,f4,f5,f6) <- splitTriangle middle eid
      mAfter <- get
      let afterAng = minimum
            [ faceMinAngle f3 mAfter
            , faceMinAngle f4 mAfter
            , faceMinAngle f5 mAfter
            , faceMinAngle f6 mAfter ]
      if afterAng < beforeAng
        then return Nothing
        else return (Just mAfter)

splitLongestEdge :: Mesh (V2 Double) -> Mesh (V2 Double) -> (Mesh (V2 Double), Mesh (V2 Double))
splitLongestEdge m1 m2 =
    (splitInternalEdgeForced longest m1, splitInternalEdgeForced longest m2)
  where
    longest = longestEdge m1 m2

splitInternalEdgeForced :: EdgeId -> Mesh (V2 Double) -> Mesh (V2 Double)
splitInternalEdgeForced eid m = execState worker m
  where
    worker = do
      edge <- getEdge eid
      twin <- getEdge (edge^.edgeTwin)
      v0 <- _vertexPosition <$> requireVertex (edge^.edgeVertex)
      v1 <- _vertexPosition <$> requireVertex (twin^.edgeVertex)
      let middle = lerp 0.5 v0 v1
      splitTriangle middle eid

splitOuterEdges :: Mesh (V2 Double) -> Mesh (V2 Double) -> (Mesh (V2 Double), Mesh (V2 Double))
splitOuterEdges = applyCompatible splitOuterEdge outerEdges

splitOuterEdge :: EdgeId -> Mesh (V2 Double) -> Maybe (Mesh (V2 Double))
splitOuterEdge eid m = evalState worker m
  where
    worker = do
      edge <- getEdge eid
      twin <- getEdge (edge^.edgeTwin)
      v0 <- _vertexPosition <$> requireVertex (edge^.edgeVertex)
      v1 <- _vertexPosition <$> requireVertex (twin^.edgeVertex)
      let middle = lerp 0.5 v0 v1
      let f1 = twin^.edgeFace
      mBefore <- get
      let beforeAng = faceMinAngle f1 mBefore
      (f2,f3) <- splitOuterTriangle middle eid
      mAfter <- get
      let afterAng = minimum
            [ faceMinAngle f2 mAfter
            , faceMinAngle f3 mAfter ]
      if afterAng < beforeAng
        then return Nothing
        else return (Just mAfter)

renderMesh :: Double -> Mesh (V2 Double) -> SVG
renderMesh radius m = mkGroup
  [ mkGroup
    [ withStrokeColor "black" $
      mkLine (x1, y1) (x2, y2)
    | edge <- M.elems (_meshEdges m)
    , _edgeFace edge /= _meshOuterFace m
    , let twin = meshGetEdge (edge ^. edgeTwin) m
    , let V2 x1 y1 = _vertexPosition (meshGetVertex (_edgeVertex edge) m)
          V2 x2 y2 = _vertexPosition (meshGetVertex (_edgeVertex twin) m)
    ]
  -- , mkGroup
  --   [ mkLinePathClosed
  --     [
  --     |
  --     ] [(Double, Double)] -> Tree
  --   | face <- M.elems _meshFaces
  --   ]
  , mkGroup
    [ translate x y $ withFillColor "red" $ mkCircle radius
    | vertex <- M.elems (_meshVertices m)
    , let V2 x y = _vertexPosition vertex
    ]
  ]

renderMeshEdges :: Mesh (V2 Double) -> SVG
renderMeshEdges mesh@Mesh {..} = mkGroup
  [ mkGroup
    [ mkGroup
      [ withStrokeColorPixel (promotePixel $ viridis $ ang/pi) $
        mkLine (x1, y1) (x2, y2)
      ]
    | edge <- M.elems _meshEdges
    , _edgeFace edge /= _meshOuterFace
    , let twin = meshGetEdge (edge ^. edgeTwin) mesh
          V2 x1 y1 = _vertexPosition (meshGetVertex (_edgeVertex edge) mesh)
          V2 x2 y2 = _vertexPosition (meshGetVertex (_edgeVertex twin) mesh)
          ang = meshEdgeAngle mesh (edge^.edgeId)
    , ang/pi*180 < 2
    ]
  ]

renderMeshSimple :: Double -> Mesh (V2 Double) -> SVG
renderMeshSimple radius Mesh {..} = mkGroup
  [ mkGroup
    [ translate x y $ mkGroup
      [ withFillColor "red" $ mkCircle 0.01
      , scaleToHeight (radius*2) $ center $ latex $ T.pack $ show (_vertexId vertex) ]
    | vertex <- M.elems _meshVertices
    , let V2 x y = _vertexPosition vertex
    ]
  ]

renderMeshColored :: Mesh (V2 Double) -> SVG
renderMeshColored m@Mesh{..} = mkGroup
  [ withFillColorPixel c $ mkLinePathClosed $
    [ (x,y) | V2 x y <- facePositions fid m ]
  | fid <- M.keys _meshFaces
  , let ang = faceMinAngle fid m
        bestAng = pi/3
        score = ang/bestAng
        c = promotePixel $ viridis score
  ]
-- facePositions :: FaceId -> Mesh a -> [a]
-- faceMinAngle :: FaceId -> Mesh (V2 Double) -> Double

renderMeshStats :: Mesh (V2 Double) -> SVG
renderMeshStats mesh = mkGroup
    [ latex $ T.pack $ printf "Min:  %.1f" (minAng/pi*180)
    , translate 0 (sep*1) $
      latex $ T.pack $ printf "Mean: %.1f" (meanAngle/pi*180)
    , translate 0 (sep*2) $
      latex $ T.pack $ printf "Avg:  %.1f" (avgAngle/pi*180)
    , translate 0 (sep*3) $
      latex $ T.pack $ printf "Max:  %.1f" (maxAngle/pi*180)
    ]
  where
    sep = -1
    angs = meshAngles mesh
    minAng = minimum angs
    meanAngle = L.sort angs !! (length angs `div` 2)
    avgAngle = sum angs / fromIntegral (length angs)
    maxAngle = maximum angs
