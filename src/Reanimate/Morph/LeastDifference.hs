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

leastDifference :: PointCorrespondence
leastDifference = undefined

pCuts :: Polygon -> [(Polygon,Polygon)]
pCuts p =
  [ pCutAt p i
  | i <- [2 .. pSize p-2 ]
  , pParent p 0 i == 0 ]

pCutAt :: Polygon -> Int -> (Polygon, Polygon)
pCutAt p i = (mkPolygon $ V.fromList left, mkPolygon $ V.fromList right)
  where
    n     = pSize p
    left  = map (pAccess p) [0 .. i]
    right = map (pAccess p) (0:[i..n-1])

pCut :: Polygon -> (Polygon, Polygon)
pCut p
  -- pSize p == 3 = (mkPolygon $ V.fromList left, mkPolygon $ V.fromList right)
  | otherwise = safeHead . sortOn sizeDifference . concatMap pCuts . pCycles $ p
  where
    safeHead [] = error "pCut head"
    safeHead (x:_) = x
  -- where
  --   inbetween = lerp 0.5 (pAccess p 1) (pAccess p 2)
  --   left  = [pAccess p 0, pAccess p 1, inbetween]
  --   right = [pAccess p 0, inbetween, pAccess p 2]

pCompatibleCut :: [(Edge,Edge)] -> Polygon -> Polygon -> Maybe ((Polygon, Polygon),(Polygon, Polygon), [(Edge,Edge)])
pCompatibleCut edges p1 p2 = listToMaybe $ sortOn pairSizeDifference $ do
  (p1l, p1r) <- concatMap pCuts $ pCycles p1
  (p2l, p2r) <- concatMap pCuts $ pCycles p2
  let newEdges =
        (aEdge,bEdge) : edges
      aEdge = (pAccess p1l 0, pAccess p1r 1)
      bEdge = (pAccess p2r 0, pAccess p2r 1)
  guard $ pTopology (map fst newEdges) p1l == pTopology (map snd newEdges) p2l
  guard $ pTopology (map fst newEdges) p1r == pTopology (map snd newEdges) p2r
  return ((p1l, p1r), (p2l, p2r), (aEdge,bEdge) : edges)

sizeDifference :: (Polygon, Polygon) -> Rational
sizeDifference (a,b) = abs (pArea a - pArea b)

pairSizeDifference :: ((Polygon, Polygon),(Polygon, Polygon),a) -> Rational
pairSizeDifference (a,b,_) = max (sizeDifference a) (sizeDifference b)

pEdges :: Polygon -> [Edge]
pEdges p = [ (pAccess p i, pAccess p $ i+1) | i <- [0 .. pSize p-1]]

type Edge = (V2 Rational, V2 Rational)

data Topology = MutableSegment | ImmutableSegment Int
  deriving (Eq, Show)

pTopology :: [Edge] -> Polygon -> [Topology]
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
triangulate a b
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
    (al, ar) = pCut a
    (bl, br) = pCut b

triangulate' :: [(Edge,Edge)] -> Polygon -> Polygon -> [(Polygon,Polygon)]
triangulate' edges a b
  | pSize a == 3 || pSize b == 3 = giveUp
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
          (a'', b'') = closestLinearCorrespondence a' b'
      in -- [closestLinearCorrespondence a' b']
        Compat.compatiblyTriangulateP a'' b''
