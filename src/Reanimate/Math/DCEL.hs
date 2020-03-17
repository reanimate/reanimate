{-# LANGUAGE RecordWildCards #-}
module Reanimate.Math.DCEL where
{-
import           Control.Applicative
import           Data.List.Split
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           Text.Printf

import           Reanimate.Math.Common

type EdgeIdx = Int
type VertexIdx = Int
type FaceIdx = Int

data DCEL = DCEL
  { dcelFaces    :: Vector EdgeIdx
  , dcelVertices :: Vector EdgeIdx
  , dcelEdges    :: Vector Edge
  , dcelPoints   :: Vector P
  } deriving (Show)

data Edge = Edge
  { edgeVertex :: VertexIdx
  , edgeFace   :: FaceIdx
  , edgeTwin   :: EdgeIdx
  , edgeNext   :: EdgeIdx
  , edgePrev   :: EdgeIdx
  } deriving (Show)

getPoint :: Int -> DCEL -> P
getPoint i DCEL{..} = dcelPoints V.! (i `mod` V.length dcelPoints)

dcelSize :: DCEL -> Int
dcelSize DCEL{..} = V.length dcelPoints



isValidDCEL :: DCEL -> Maybe String
isValidDCEL DCEL{..} = foldr (<|>) empty $
  [ V.all (< V.length dcelEdges) dcelFaces ~> "Bad face"
  , V.all (< V.length dcelEdges) dcelVertices ~> "Bad vertice"
  , V.all (< V.length dcelEdges) (V.map edgeNext dcelEdges) ~> "Bad next"
  , V.all (< V.length dcelEdges) (V.map edgePrev dcelEdges) ~> "Bad prev"
  , V.all (< V.length dcelEdges) (V.map edgeTwin dcelEdges) ~> "Bad twin"
  , V.all (< V.length dcelPoints) (V.map edgeVertex dcelEdges) ~> "Bad edge vertex"
  , V.all (< V.length dcelFaces) (V.map edgeFace dcelEdges) ~> "Bad edge face"
  , V.length dcelVertices == V.length dcelPoints ~> "Vertex/Point mismatch"
  ]
  where
    infix 0 ~>
    True ~> _    = Nothing
    False ~> msg = Just msg

ppDCEL :: DCEL -> String
ppDCEL DCEL{..} =
  unlines
  [ "Faces: Edge"
  , unlines
    [ printf "%3d:  %3d" face edge
    | (face, edge) <- zip [0::Int .. ] (V.toList dcelFaces)
    ]
  , "Vertices: Edge"
  , unlines
    [ printf "%3d:     %3d" vertex edge
    | (vertex, edge) <- zip [0::Int ..] (V.toList dcelVertices)
    ]
  , "Edges: Vertex Face Twin Next Prev"
  , unlines
    [ printf "%3d:   %3d   %3d  %3d  %3d  %3d"
        edge edgeVertex edgeFace edgeTwin edgeNext edgePrev
    | (edge, Edge{..}) <- zip [0::Int ..] (V.toList dcelEdges)
    ]
  , "Points:"
  , unlines
    [ printf "%3d:  " (idx*chunkSize) ++
      unwords [ printf "<%.1f,%.1f>" x y | V2 x y <- chunk ]
    | (idx, chunk) <- zip [0::Int ..] (chunksOf chunkSize (V.toList dcelPoints))
    ]
  ]
  where chunkSize = 5

emptyDCEL :: DCEL
emptyDCEL = undefined

{-
faces: [0,1] -- 0 is outside, 1 is inside.
vertices: [0..n-1]
edges:
  i: vertex=i
     face=1
     twin=i+n
     next=(i+1)%n
     prev=(i-1)%n
  i: j=i-n
     vertex=edge[j].prev.vertex
     face=0
     twin=j
     next=(j-1)%n + i
     prev=(j+1)%n +i
points: pts
-}
fromSimplePolygon :: [V2 Double] -> DCEL
fromSimplePolygon [] = error "empty polygon"
fromSimplePolygon pts | not (isCCW $ V.fromList pts) = error "Polygon not counter-clockwise"
fromSimplePolygon pts = DCEL
    { dcelFaces = V.fromList [0,1]
    , dcelVertices = V.fromList [0..n-1]
    , dcelEdges = V.fromList $
      [ Edge
        { edgeVertex = i
        , edgeFace = 1
        , edgeTwin = i+n
        , edgeNext = (i+1) `mod` n
        , edgePrev = (i-1) `mod` n }
      | i <- [0..n-1] ] ++
      [ Edge
        { edgeVertex = (i-1) `mod` n
        , edgeFace = 0
        , edgeTwin = i
        , edgeNext = (i-1) `mod` n + i
        , edgePrev = (i-1) `mod` n + i }
      | i <- [0..n-1] ]
    , dcelPoints = V.fromList pts
    }
  where
    n = length pts

-- addEdge :: VertexIdx -> VertexIdx -> DCEL -> DCEL ->
-- addEdge a b dcel =
--   add edge, vertex=a, face=a
--   add twin
-}
