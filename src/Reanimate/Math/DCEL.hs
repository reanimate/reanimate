{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- Code originally from: https://github.com/wereHamster/haskell-half-edge-mesh
module Reanimate.Math.DCEL where

import           Control.Lens
import           Data.Map                                 ( Map )
import qualified Data.Map                      as M
import qualified Data.List                     as L
import           Data.Maybe
import           Control.Monad.State
import           Debug.Trace
import           Reanimate
import           Linear.V2
import Text.Printf

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
    , _vertexEdge     :: EdgeId -- Index to CCW edge pointing to this vertex.
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

makeLenses ''Mesh
makeLenses ''Vertex
makeLenses ''Edge
makeLenses ''Face

ppMesh :: Show a => Mesh a -> String
ppMesh Mesh{..} = unlines
  [ printf "Outer face: %d" _meshOuterFace
  , ""
  , "Vertices:"
  , printf "%4s %5s %10s" ("ID"::String) ("Edge"::String) ("Position"::String)
  , unlines
    [ printf "%4d %5d %10s" _vertexId  _vertexEdge (show _vertexPosition)
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
  outerFace <- gets _meshOuterFace
  polygonMesh outer
  modify $ meshFaces .~ M.empty
  forM_ trigs $ \pts -> do
    innerFace <- createFace
    vIds <- mapM getVertex pts
    edges     <- forM (zip vIds (tail vIds ++ take 1 vIds)) $ \(v0, v1) -> do
      e <- do
        mbEdge <- findEdge' v0 v1
        case mbEdge of
          Just e  -> pure (e ^. edgeId)
          Nothing -> do
            mbTwin <- findEdge'' [innerFace,outerFace, 1] v0
            e <- createEdge v1
            case mbTwin of
              Nothing -> return ()
              Just twin -> 
                setTwinEdge e (twin^.edgeId)
            return e
      setFace e innerFace
      modifyVertex (vertexEdge .~ e) v0
      pure e
    
    setFaceEdge innerFace (head edges)
    forM_ (zip edges (tail edges ++ take 1 edges)) $ \(e0, e1) ->
      linkEdges e0 e1

-- Positions must be in CCW order
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
    modifyVertex (vertexEdge .~ e) v0
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


newId :: MeshM a Int
newId = do
  counter <- gets _meshIdCounter
  modify $ meshIdCounter %~ succ
  return counter

createVertex :: a -> State (Mesh a) VertexId
createVertex position = do
  k <- newId
  let v = Vertex k position (error "_vertexEdge not set")
  modify $ meshVertices %~ M.insert k v
  return k

getVertex :: Eq a => a -> State (Mesh a) VertexId
getVertex position = do
  vs <- gets _meshVertices
  case L.find comparingPosition (M.assocs vs) of
    Nothing     -> createVertex position
    Just (k, _) -> return k
  where comparingPosition (_k, v) = (_vertexPosition v) == position

modifyVertex :: (Vertex a -> Vertex a) -> VertexId -> State (Mesh a) ()
modifyVertex f k = modify $ meshVertices %~ M.adjust f k

getEdge :: EdgeId -> State (Mesh a) Edge
getEdge e = do
  edges <- gets _meshEdges
  maybe (bug edges) return $ M.lookup e edges
    --return $ fromJust $ M.lookup e edges
  where bug _edges = error $ "Can't find edge with id " ++ (show e)

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
    | otherwise = traceShow e1 $ do
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

getFace :: FaceId -> State (Mesh a) Face
getFace f = do
  faces <- gets _meshFaces
  maybe bug return (M.lookup f faces)
  where bug = error $ "Can't find face with id " ++ (show f)


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
  [ _meshEdges mesh M.! (edge ^. edgeTwin)
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
splitEdge :: a -> EdgeId -> MeshM a ()
splitEdge vertex e = do
  eEdge     <- getEdge e
  eEdgeTwin <- getEdge (eEdge ^. edgeTwin)

  t         <- createVertex vertex
  e1        <- createEdge t
  setFace e1 (eEdge ^. edgeFace)
  modifyVertex (vertexEdge .~ e1) t

  e1' <- createEdge (eEdgeTwin ^. edgeVertex)
  setFace e1' (eEdgeTwin ^. edgeFace)
  setTwinEdge e1 e1'

  let oldPrev = eEdge ^. edgePrev
      oldNext = eEdgeTwin ^. edgeNext

  linkEdges oldPrev             e1
  linkEdges (eEdge ^. edgeTwin) e1'
  linkEdges e1                  e
  linkEdges e1'                 oldNext

  return ()

  -- e goes from u to v
  -- New edge: u to t
  --     twin: t to u
  -- New edge: t to v
  --     Twin: v to t
  -- Add new face
  -- Set new face for all edges in one direction

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
  edges <- gets _meshEdges
  pure $ listToMaybe
    [ e
    | e <- M.elems edges
    , e ^. edgeVertex == v1
    , let twin = edges M.! (e ^. edgeTwin)
    , twin ^. edgeVertex == v0
    ]

findEdge'' :: [FaceId] -> VertexId -> MeshM a (Maybe Edge)
findEdge'' fs v = do
  edges <- gets (M.elems . _meshEdges)
  pure $
      listToMaybe [ e | e <- edges, e ^. edgeFace `notElem` fs, e ^. edgeVertex == v ]

insertEdge :: FaceId -> VertexId -> VertexId -> MeshM a ()
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


renderMesh :: Mesh (V2 Double) -> SVG
renderMesh Mesh {..} = mkGroup
  [ mkGroup
    [ withStrokeColor "black" $
      mkLine (x1, y1) (x2, y2)
    | edge <- M.elems _meshEdges
    , let twin = _meshEdges M.! (edge ^. edgeTwin)
    , (edge ^. edgeVertex) < (twin ^. edgeVertex)
    , let V2 x1 y1 = _vertexPosition (_meshVertices M.! _edgeVertex edge)
          V2 x2 y2 = _vertexPosition (_meshVertices M.! _edgeVertex twin)
    ]
  , mkGroup
    [ translate x y $ withFillColor "red" $ mkCircle 0.05
    | vertex <- M.elems _meshVertices
    , let V2 x y = _vertexPosition vertex
    ]
  ]

