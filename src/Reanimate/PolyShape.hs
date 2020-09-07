{-|
Module      : Reanimate.PolyShape
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

A PolyShape is a closed set of curves.

-}
module Reanimate.PolyShape
  ( PolyShape(..)
  , PolyShapeWithHoles
  , svgToPolyShapes     -- :: Tree -> [PolyShape]
  , svgToPolygons       -- :: Double -> Svg -> [Polygon]

  , renderPolyShape     -- :: PolyShape -> Tree
  , renderPolyShapes    -- :: [PolyShape] -> Tree
  , renderPolyShapePoints -- :: PolyShape -> Tree

  , plPathCommands      -- :: PolyShape -> [PathCommand]
  , plLineCommands      -- :: PolyShape -> [LineCommand]

  , plLength            -- :: PolyShape -> Double
  , plArea
  , plCurves            -- :: PolyShape -> [CubicBezier Double]
  , isInsideOf          -- :: PolyShape -> PolyShape -> Bool

  , plFromPolygon       -- :: [RPoint] -> PolyShape
  , plToPolygon         -- :: Double -> PolyShape -> Polygon
  , plDecompose         -- :: [PolyShape] -> [[RPoint]]
  , unionPolyShapes     -- :: [PolyShape] -> [PolyShape]
  , unionPolyShapes'    -- :: Double -> [PolyShape] -> [PolyShape]
  , plDecompose'        -- :: Double -> [PolyShape] -> [[RPoint]]
  , decomposePolygon    -- :: [Point Double] -> [[RPoint]]
  , plGroupShapes       -- :: [PolyShape] -> [PolyShapeWithHoles]
  , mergePolyShapeHoles -- :: PolyShapeWithHoles -> PolyShape
  , plPartial
  , plGroupTouching
  ) where

import           Algorithms.Geometry.PolygonTriangulation.Triangulate (triangulate')
import           Control.Lens                                         ((&), (.~), (^.))
import           Data.Ext
import           Data.Geometry.PlanarSubdivision                      (PolygonFaceData (..))
import qualified Data.Geometry.Point                                  as Geo
import qualified Data.Geometry.Polygon                                as Geo
import           Data.List                                            (nub, partition, sortOn)
import qualified Data.PlaneGraph                                      as Geo
import           Data.Proxy
import qualified Data.Vector                                          as V
import           Geom2D.CubicBezier.Linear                            (ClosedPath (..),
                                                                       CubicBezier (..),
                                                                       FillRule (..), PathJoin (..),
                                                                       QuadBezier (..), arcLength,
                                                                       arcLengthParam,
                                                                       bezierIntersection,
                                                                       bezierSubsegment,
                                                                       closedPathCurves, closest,
                                                                       colinear, curvesToClosed,
                                                                       evalBezier, quadToCubic,
                                                                       reorient, splitBezier, union,
                                                                       vectorDistance)
import           Graphics.SvgTree                                     (PathCommand (..), RPoint,
                                                                       Tree, defaultSvg,
                                                                       pathDefinition, pathTree)
import           Linear.V2
import           Reanimate.Animation
import           Reanimate.Constants
import           Reanimate.Math.Polygon                               (Polygon, mkPolygon, pArea,
                                                                       pIsCCW)
import           Reanimate.Svg

-- | Shape drawn by continuous line. May have overlap, may be convex.
newtype PolyShape = PolyShape { unPolyShape :: ClosedPath Double }
  deriving (Show)

-- | Polyshape with smaller, fully-enclosed holes.
data PolyShapeWithHoles = PolyShapeWithHoles
  { polyShapeParent :: PolyShape
  , polyShapeHoles  :: [PolyShape]
  }


-- | Render a set of polyshapes as a single SVG path.
renderPolyShapes :: [PolyShape] -> Tree
renderPolyShapes pls =
  pathTree $ defaultSvg & pathDefinition .~ concatMap plPathCommands pls

-- | Render a polyshape as a single SVG path.
renderPolyShape :: PolyShape -> Tree
renderPolyShape pl =
    pathTree $ defaultSvg & pathDefinition .~ plPathCommands pl

-- | Render control-points of a polyshape as circles.
renderPolyShapePoints :: PolyShape -> Tree
renderPolyShapePoints = mkGroup . map renderPoint . plCurves
  where
    renderPoint (CubicBezier (V2 x y) _ _ _) =
      translate x y $ mkCircle 0.02

-- | Length of polyshape circumference.
plLength :: PolyShape -> Double
plLength = sum . map cubicLength . plCurves
  where
    cubicLength c = arcLength c 1 polyShapeTolerance

-- | Area of polyshape.
plArea :: PolyShape -> Double
plArea pl = realToFrac $ pArea $ plToPolygon polyShapeTolerance pl

-- 1/10th of a pixel if rendered at 2560x1440
polyShapeTolerance :: Double
polyShapeTolerance = screenWidth/25600

-- | Construct a polyshape from the vertices in a polygon.
plFromPolygon :: [RPoint] -> PolyShape
plFromPolygon = PolyShape . ClosedPath . map worker
  where
    worker val = (val, JoinLine)

-- | Approximate a polyshape as a polygon within the given tolerance.
plToPolygon :: Double -> PolyShape -> Polygon
plToPolygon tol pl =
  let p = V.init . V.fromList . map (fmap realToFrac) .
          plPolygonify tol $ pl
  in if pIsCCW (mkPolygon p) then mkPolygon p else mkPolygon (V.reverse p)

-- | Partially draw polyshape.
plPartial :: Double -> PolyShape -> PolyShape
plPartial delta pl | delta >= 1 = pl
plPartial delta pl = PolyShape $ curvesToClosed (lineOut ++ [joinB] ++ lineIn)
  where
    lineOutEnd = cubicC3 (last lineOut)
    lineInBegin = cubicC0 (head lineIn)
    joinB = CubicBezier lineOutEnd lineOutEnd lineOutEnd lineInBegin
    lineOut = takeLen (len*delta/2) $ plCurves pl
    lineIn =
      reverse $ map reorient $
      takeLen (len*delta/2) $ reverse $ map reorient $ plCurves pl
    len = plLength pl
    takeLen _ [] = []
    takeLen l (c:cs) =
      let cLen = arcLength c 1 polyShapeTolerance in
      if l < cLen
        then [bezierSubsegment c 0 (arcLengthParam c l polyShapeTolerance)]
        else c : takeLen (l-cLen) cs

-- plPartial' :: Double -> ([RPoint], PolyShape) -> PolyShape
-- plPartial' delta (seen', PolyShape (ClosedPath lst)) =
--   case lst of
--     []                         -> PolyShape (ClosedPath [])
--     (startP, startJoin) : rest -> PolyShape $ ClosedPath $
--       (startP, startJoin) : worker startP rest
--   where
--     seen = filter (`elem` plPoints) seen'
--     closestSeen pt = minimumBy (comparing (vectorDistance pt)) seen
--     worker _ [] = []
--     worker _ ((newP, newJoin) : rest)
--       | newP `elem` seen = (newP, newJoin) : worker newP rest
--       | otherwise =
--         let newAt = interpolateVector (closestSeen newP) newP delta
--         in (newAt, newJoin) : worker newAt rest
--     plPoints =
--       [ p | (p,_) <- lst ]

-- | Find intersection points.
plGroupTouching :: [PolyShape] -> [[([RPoint],PolyShape)]]
plGroupTouching [] = []
plGroupTouching pls = worker [polyShapeOrigin (head pls)] pls
  where
    worker _ [] = []
    worker seen shapes =
      let (touching, notTouching) = partition (isTouching seen) shapes
      in if null touching
        then plGroupTouching notTouching
        else map ((,) seen . changeOrigin seen) touching   :
             worker (seen ++ concatMap plPoints touching) notTouching
    isTouching pts = any (`elem` pts) . plPoints
    changeOrigin seen (PolyShape (ClosedPath segments)) = PolyShape $ ClosedPath $ helper [] segments
      where
        helper acc [] = reverse acc
        helper acc lst@((startP,startJ):rest)
          | startP `elem` seen = lst ++ reverse acc
          | otherwise = helper ((startP, startJ):acc) rest
    plPoints :: PolyShape -> [RPoint]
    plPoints (PolyShape (ClosedPath lst)) =
      [ p | (p,_) <- lst ]

-- | Deconstruct a polyshape into non-intersecting, convex polygons.
plDecompose :: [PolyShape] -> [[RPoint]]
plDecompose = plDecompose' 0.001

-- | Deconstruct a polyshape into non-intersecting, convex polygons.
plDecompose' :: Double -> [PolyShape] -> [[RPoint]]
plDecompose' tol =
  concatMap (decomposePolygon . plPolygonify tol . mergePolyShapeHoles) .
  plGroupShapes .
  unionPolyShapes

-- | Split polygon into smaller, convex polygons.
decomposePolygon :: [RPoint] -> [[RPoint]]
decomposePolygon poly =
  [ [ V2 x y
    | v <- V.toList (Geo.boundaryVertices f pg)
    , let Geo.Point2 x y = pg^.Geo.vertexDataOf v . Geo.location ]
  | (f, Inside) <- V.toList (Geo.internalFaces pg) ]

  where
    pg = triangulate' Proxy p
    p = Geo.fromPoints $
      [ Geo.Point2 x y :+ ()
      | V2 x y <- poly ]

plPolygonify :: Double -> PolyShape -> [RPoint]
plPolygonify tol shape =
    startPoint (head curves) : concatMap worker curves
  where
    curves = plCurves shape
    worker c | endPoint c == startPoint c =
      [] -- error $ "Bad bezier: " ++ show c
    worker c =
      if colinear c tol -- && arcLength c 1 tol < 1
        then [endPoint c]
        else
          let (lhs,rhs) = splitBezier c 0.5
          in worker lhs ++ worker rhs
    endPoint (CubicBezier _ _ _ d) = d
    startPoint (CubicBezier a _ _ _) = a

-- | Convert a polyshape to a list of SVG path commands.
plPathCommands :: PolyShape -> [PathCommand]
plPathCommands = lineToPath . plLineCommands

-- | Convert a polyshape to a list of line commands.
plLineCommands :: PolyShape -> [LineCommand]
plLineCommands pl =
  case curves of
    []                  -> []
    (CubicBezier start _ _ _:_) ->
      LineMove start :
      zipWith worker (drop 1 dstList ++ [start]) joinList ++
      [LineEnd start]
  where
    ClosedPath closedPath = unPolyShape pl
    (dstList, joinList) = unzip closedPath
    curves = plCurves pl
    worker dst JoinLine =
      LineBezier [dst]
    worker dst (JoinCurve a b) =
      LineBezier [a,b,dst]

-- | Extract all shapes from SVG nodes. Drawing attributes such
--   as stroke and fill color are discarded.
svgToPolyShapes :: Tree -> [PolyShape]
svgToPolyShapes = cmdsToPolyShapes . toLineCommands . extractPath

-- | Extract all polygons from SVG nodes. Curves are approximated to
--   within the given tolerance.
svgToPolygons :: Double -> SVG -> [Polygon]
svgToPolygons tol = map (toPolygon . plPolygonify tol) . svgToPolyShapes
  where
    toPolygon :: [RPoint] -> Polygon
    toPolygon = mkPolygon .
      V.fromList . nub . map (fmap realToFrac)

cmdsToPolyShapes :: [LineCommand] -> [PolyShape]
cmdsToPolyShapes [] = []
cmdsToPolyShapes cmds =
    case cmds of
      (LineMove dst:cont) -> map PolyShape $ worker dst [] cont
      _                   -> bad
  where
    bad = error $ "Reanimate.PolyShape: Invalid commands: " ++ show cmds
    finalize [] rest  = rest
    finalize acc rest = ClosedPath (reverse acc) : rest
    worker _from acc [] = finalize acc []
    worker _from acc (LineMove newStart : xs) =
      finalize acc $
      worker newStart [] xs
    worker from acc (LineEnd orig:LineMove dst:xs) | from /= orig =
      finalize ((from, JoinLine):acc) $
      worker dst [] xs
    worker _from acc (LineEnd{}:LineMove dst:xs) =
      finalize acc $
      worker dst [] xs
    worker from acc [LineEnd orig] | from /= orig =
      finalize ((from, JoinLine):acc) []
    worker _from acc [LineEnd{}] =
      finalize acc []
    worker from acc (LineBezier [x]:xs) =
      worker x ((from, JoinLine) : acc) xs
    worker from acc (LineBezier [a,b]:xs) =
      let quad = QuadBezier from a b
          CubicBezier _ a' b' c' = quadToCubic quad
      in worker from acc (LineBezier [a',b',c']:xs)
    worker from acc (LineBezier [a,b,c]:xs) =
      worker c ((from, JoinCurve a b) : acc) xs
    worker _ _ _ = bad

-- | Merge overlapping shapes.
unionPolyShapes :: [PolyShape] -> [PolyShape]
unionPolyShapes shapes =
    map PolyShape $
    union (map unPolyShape shapes) FillNonZero (polyShapeTolerance/10000)

-- | Merge overlapping shapes to within given tolerance.
unionPolyShapes' :: Double -> [PolyShape] -> [PolyShape]
unionPolyShapes' tol shapes =
    map PolyShape $
    union (map unPolyShape shapes) FillNonZero tol

-- | True iff lhs is inside of rhs.
--   lhs and rhs may not overlap.
--   Implementation: Trace a vertical line through the origin of A and check
--   of this line intersects and odd number of times on both sides of A.
isInsideOf :: PolyShape -> PolyShape -> Bool
lhs `isInsideOf` rhs =
    odd (length upHits) && odd (length downHits)
  where
    (upHits, downHits) = polyIntersections origin rhs
    origin = polyShapeOrigin lhs

polyIntersections :: RPoint -> PolyShape -> ([RPoint],[RPoint])
polyIntersections origin rhs =
    (nub $ concatMap (intersections rayUp) curves
    ,nub $ concatMap (intersections rayDown) curves)
  where
    curves = plCurves rhs

    intersections line bs =
      map (evalBezier bs . fst) (bezierIntersection bs line polyShapeTolerance)
    limit = 1000
    rayUp = CubicBezier origin origin origin (V2 limit limit)
    rayDown = CubicBezier origin origin origin (V2 (-limit) (-limit))

polyShapeOrigin :: PolyShape -> V2 Double
polyShapeOrigin (PolyShape closedPath) =
  case closedPath of
    ClosedPath []            -> V2 0 0
    ClosedPath ((start,_):_) -> start

-- | Find holes and group them with their parent.
plGroupShapes :: [PolyShape] -> [PolyShapeWithHoles]
plGroupShapes = worker
  where
    worker (s:rest)
      | null (parents s rest) =
        let isOnlyChild x = parents x (s:rest) == [s]
            (holes, nonHoles) = partition isOnlyChild rest
            prime = PolyShapeWithHoles
              { polyShapeParent = s
              , polyShapeHoles  = holes }
        in prime : worker nonHoles
      | otherwise = worker (rest ++ [s])
    worker [] = []

    parents :: PolyShape -> [PolyShape] -> [PolyShape]
    parents self = filter (self `isInsideOf`) . filter (/=self)

instance Eq PolyShape where
  a == b = plCurves a == plCurves b

-- | Cut out holes.
mergePolyShapeHoles :: PolyShapeWithHoles -> PolyShape
mergePolyShapeHoles (PolyShapeWithHoles parent []) = parent
mergePolyShapeHoles (PolyShapeWithHoles parent (child:children)) =
  mergePolyShapeHoles $
    PolyShapeWithHoles (mergePolyShapeHole parent child) children

-- Merge
mergePolyShapeHole :: PolyShape -> PolyShape -> PolyShape
mergePolyShapeHole parent child =
  snd $ head $
  sortOn fst
  [ cutSingleHole newParent child
  | newParent <- polyShapePermutations parent ]

{-
parent:
  (a,b)
  (b,c)
  (c,a)

child:
  (x,y)
  (y,z)
  (z,x)

P = split (a,b)
new:
  (P,b) p2b
  (b,c) pTail
  (c,a) pTail
  (a,P) a2p

  (P,x) p2x

  (x,y) childCurves
  (y,z) childCurves
  (z,x) childCurves

  (x,P) x2p

-}
cutSingleHole :: PolyShape -> PolyShape -> (Double, PolyShape)
cutSingleHole parent child =
    (score, PolyShape $ curvesToClosed $
      p2b:pTail ++ [a2p] ++
      [p2x] ++ childCurves ++
      [x2p]
    )
  where
    -- vect = (childOrigin - p) * 0 -- 0.0001
    vectL = 0 -- rotate90L $* vect
    vectR = 0 -- rotate90R $* vect
    score = vectorDistance childOrigin p
    childOrigin = polyShapeOrigin child
    childOrigin' = childOrigin - vectL
    (pHead:pTail) = plCurves parent
    childCurves = plCurves child

    pParam = closest pHead childOrigin polyShapeTolerance

    (a2p, p2b') = splitBezier pHead pParam
    p2b = case p2b' of
      CubicBezier a b c d -> CubicBezier (a - vectL) b c d

    p = evalBezier pHead pParam
    -- straight line to child origin
    p2x = lineBetween (p - vectR) childOrigin
    -- straight line from child origin
    x2p = lineBetween childOrigin' p

    lineBetween a = CubicBezier a a a

-- | Destruct a polyshape into constituent curves.
plCurves :: PolyShape -> [CubicBezier Double]
plCurves = closedPathCurves . unPolyShape

polyShapePermutations :: PolyShape -> [PolyShape]
polyShapePermutations =
    map (PolyShape . curvesToClosed) . cycleList . plCurves
  where
    cycleList lst =
      let n = length lst in
      [ take n $ drop i $ cycle lst
      | i <- [0.. n-1] ]
