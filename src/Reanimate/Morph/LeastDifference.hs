{-# LANGUAGE OverloadedStrings #-}
module Reanimate.Morph.LeastDifference where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                 as T
import           Data.Tuple
import qualified Data.Vector               as V
import           Linear.V2
import           Linear.Vector
import           Reanimate.Debug
import           Reanimate.LaTeX
import           Reanimate.Math.Common
import qualified Reanimate.Math.Compatible as Compat
import           Reanimate.Math.Polygon
import           Reanimate.Math.Render
import           Reanimate.Morph.Common
import           Reanimate.Svg
import           Reanimate.Morph.Linear

import Debug.Trace

leastDifference :: PointCorrespondence
leastDifference = undefined

pCutsTop edges p =
  [ (l, r, topL, topR, edge)
  | (l, r) <- pCuts p
  , let edge = (pAccess l 0, pAccess r 1)
        topL = pTopology (edge : edges) l
        topR = pTopology (edge : edges) r
  ]

{-# INLINE pCompatibleCut #-}
pCompatibleCut :: (Real a, Fractional a) => [(Edge a,Edge a)] -> APolygon a -> APolygon a -> Maybe ((APolygon a, APolygon a),(APolygon a, APolygon a), [(Edge a,Edge a)])
pCompatibleCut edges p1 p2 = listToMaybe $ sortOn pairSizeDifference $ do
  (p1l, p1r, aTopL, aTopR, aEdge) <- pCutsTop (map fst edges) p1
  -- let aEdge = (pAccess p1l 0, pAccess p1r 1)
  --     aTopL = pTopology (aEdge : map fst edges) p1l
  --     aTopR = pTopology (aEdge : map fst edges) p1r
  (p2l, p2r, bTopL, bTopR, bEdge) <- pCutsTop (map snd edges) p2
  -- let bEdge = (pAccess p2r 0, pAccess p2r 1)
  --     bTopL = pTopology (bEdge : map snd edges) p2l
  --     bTopR = pTopology (bEdge : map snd edges) p2r
  let newEdges = (aEdge,bEdge) : edges
  -- guard $ pTopology (map fst newEdges) p1l == pTopology (map snd newEdges) p2l
  -- guard $ pTopology (map fst newEdges) p1r == pTopology (map snd newEdges) p2r
  guard $ aTopL == bTopL
  guard $ aTopR == bTopR
  return ((p1l, p1r), (p2l, p2r), newEdges)

sizeDifference :: (Fractional a) => (APolygon a, APolygon a) -> a
sizeDifference (a,b) = abs (pArea a - pArea b)

pairSizeDifference :: (Fractional a, Ord a) => ((APolygon a, APolygon a),(APolygon a, APolygon a),b) -> a
pairSizeDifference (a,b,_) = max (sizeDifference a) (sizeDifference b)

pEdges :: APolygon a -> [Edge a]
pEdges p = [ (pAccess p i, pAccess p $ i+1) | i <- [0 .. pSize p-1]]

type Edge a = (V2 a, V2 a)

data Topology = MutableSegment | ImmutableSegment Int
  deriving (Eq, Show)

{- INLINE pTopology -}
pTopology :: Eq a => [Edge a] -> APolygon a -> [Topology]
pTopology immutableEdges p = worker
  [ maybe MutableSegment ImmutableSegment isImmutable
  | i <- [0 .. pSize p-1]
  , let isImmutable =
          ((pAccess p i, pAccess p $ i+1) `elemIndex` immutableEdges) <|>
          ((pAccess p $ i+1, pAccess p i) `elemIndex` immutableEdges)
  ]
  where
    worker (MutableSegment:MutableSegment:xs) = worker (MutableSegment:xs)
    worker (x:xs)                             = x : worker xs
    worker []                                 = []

triangulate :: Polygon -> Polygon -> [(Polygon,Polygon)]
triangulate a b =
  [ (castPolygon l, castPolygon r)
  | (l,r) <- triangulate_ (castPolygon a) (castPolygon b)
  ]

triangulate_ :: FPolygon -> FPolygon -> [(FPolygon,FPolygon)]
triangulate_ a b
  | distSquared (pCentroid al) (pCentroid bl) +
    distSquared (pCentroid ar) (pCentroid br) <
    distSquared (pCentroid al) (pCentroid br) +
    distSquared (pCentroid ar) (pCentroid bl)
  -- = [(al,bl), (ar,br)]
  = triangulate' edges al bl ++ triangulate' edges ar br
  | otherwise
  -- = [(al,br), (ar,bl)]
  = triangulate' edges al br ++ triangulate' edges ar bl
  where
    edges = [(aEdge,bEdge)]
    aEdge = (pAccess ar 0, pAccess ar 1)
    bEdge = (pAccess br 0, pAccess br 1)
    (al, ar) = pCutEqual a
    (bl, br) = pCutEqual b

{-# SPECIALIZE triangulate' :: [(Edge Double,Edge Double)] -> FPolygon -> FPolygon -> [(FPolygon,FPolygon)] #-}
triangulate' :: (Real a, Fractional a) => [(Edge a,Edge a)] -> APolygon a -> APolygon a -> [(APolygon a,APolygon a)]
triangulate' edges a b
  | pSize a == 3 || pSize b == 3 = giveUp
  | trace (show (pSize a, pSize b)) False = undefined
  | Just ((al,ar),(bl,br), newEdges) <- pCompatibleCut edges a b
    = -- traceSVG (helper "topology") $
      triangulate' newEdges al bl ++ triangulate' newEdges ar br
    -- = [(al,bl), (ar,br)]
  | otherwise = giveUp
  where
    -- helper msg = mkGroup
    --   [ translate (-3) 2 $ withFillColor "grey" $ polygonShape a
    --   , translate (-4) (-2) $ withFillColor "grey" $ polygonShape al
    --   , translate (-4) (-2) $ polygonNumDots al
    --   , translate (-2) (-2) $ withFillColor "grey" $ polygonShape ar
    --   , translate (-2) (-2) $ polygonNumDots ar
    --   , translate (3) 2 $ withFillColor "grey" $ polygonShape b
    --   , translate 0 4 $ center $ latex msg
    --   , translate 0 (-2) $ center $ scale 0.5 $ latex $ T.pack $ show (pTopology (map fst newEdges) al)
    --   , translate 0 (-3) $ center $ scale 0.5 $ latex $ T.pack $ show (pTopology (map fst newEdges) ar)
    --   , translate (-3) 2 $ mkGroup
    --     [ withStrokeColor "green" $ mkLine (x1,y1) (x2,y2)
    --     | (e1, e2) <- map fst newEdges
    --     , let V2 x1 y1 = realToFrac <$> e1
    --           V2 x2 y2 = realToFrac <$> e2
    --     ]
    --   , translate (3) 2 $ mkGroup
    --     [ withStrokeColor "green" $ mkLine (x1,y1) (x2,y2)
    --     | (e1, e2) <- map snd newEdges
    --     , let V2 x1 y1 = realToFrac <$> e1
    --           V2 x2 y2 = realToFrac <$> e2
    --     ]
    --   ]
    giveUp =
      let aNewEdges = max 0 (pSize b - pSize a)
          bNewEdges = max 0 (pSize a - pSize b)
          a' = pAddPointsRestricted (map fst edges) aNewEdges a
          b' = pAddPointsRestricted (map snd edges) bNewEdges b
          (a'', b'') = closestLinearCorrespondenceA a' b'
      in [(a'', b'')]
        --Compat.compatiblyTriangulateP a'' b''
