{-# LANGUAGE MultiWayIf #-}
module Reanimate.Math.Compatible where

import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Linear.V2
import           Linear.Vector
import           Reanimate.Animation
import           Reanimate.Debug
import           Reanimate.Math.Common
import           Reanimate.Math.Polygon
import           Reanimate.Math.Render
import           Reanimate.Svg
import           Reanimate.LaTeX
import Data.Hashable

import           Debug.Trace
import GHC.Stack

truncateP :: V2 Rational -> V2 Rational
truncateP = fmap (realToFrac . (realToFrac :: Rational -> Double))

mkSteinerPoints :: V2 Rational -> V2 Rational -> Int -> [V2 Rational]
mkSteinerPoints a b s_ = [ lerp (i / (s + 1)) b a | i <- [1 .. s] ]
  where s = fromIntegral s_

-- 0..i,j..n-1
-- i..j
split1Link :: Polygon -> Int -> Int -> Int -> (Polygon, Polygon)
split1Link p i j s | j < i = split1Link p j i s
split1Link p i j s =
  (mkPolygon $ V.fromList left, mkPolygon $ V.fromList right)
 where
  n     = pSize p
  sp    = mkSteinerPoints (pAccess p i) (pAccess p j) s
  left  = map (pAccess p) [0 .. i] ++ sp ++ map (pAccess p) [j .. n - 1]
  right = map (pAccess p) [i .. j] ++ reverse sp

steiner2Link :: Polygon -> Int -> Int -> V2 Rational
steiner2Link p i j | j < i = steiner2Link p j i
steiner2Link p i j
  | isNeighbour
  = error "steiner2Link: Points are neighbours"
  | isParent
  = error "steiner2Link: Points can directly see each other."
  | not (isStraightLine || isGrandparent || oneBendBetween p i j)
  = error
    $  "steiner2Link: Cannot construct 2-link chain between points: "
    ++ show (i, j, pParent p i j, pParent p i (pParent p i j))
  | trigger && traceSVG info False = undefined
  | otherwise
  = let pI = ssspVisibility $ pSetOffset p i
        pJ = ssspVisibility $ pSetOffset p j
    in truncateP $ pCentroid $ pOverlap pI pJ
  | otherwise
  = truncateP $ lerp 0.5 windowCommon (intersects !! 0)
 where
  trigger = True -- i == 4 && j == 8
  info = mkGroup
    [ scale 2 $ mkGroup
      [ mkGroup [withFillColor "grey" $ polygonShape p, polygonNumDots p]
      , let V2 x1 y1 = realToFrac <$> windowCommon
        in translate x1 y1 $ withFillColor "red" $ mkCircle 0.08
      , let V2 x1 y1 = realToFrac <$> iWindow
        in translate x1 y1 $ withFillColor "blue" $ mkCircle 0.04
      , let V2 x1 y1 = realToFrac <$> jWindow
        in translate x1 y1 $ withFillColor "green" $ mkCircle 0.04
      ]
    , translate 5 0 $ withFillColor "white" $ latex $ T.pack $ show (i, j) ]
  distToV       = approxDist windowCommon
  isNeighbour   = i == pNext p j || i == pPrev p j
  isParent      = pParent p i j == i
  isGrandparent = pParent p i (pParent p i j) == i
  isStraightLine =
    direction (pAccess p j) (pAccess p $ pParent p i j) (pAccess p i) == 0
  intersects =
    sortOn distToV
      $ snd windowDirection
      : [ u
        | n <- [0 .. pSize p - 1]
        , let edge = (pAccess p n, pAccess p $ pNext p n)
        , u <- case rayIntersect windowDirection edge of
          Nothing -> []
          Just u  -> [u]
        , isBetween u edge
        , u /= windowCommon
        , isForward windowDirection u
        ] ++
        [ u
        | not isStraightLine
        , ray <- [(pAccess p i, jWindow), (pAccess p j, iWindow)]
        , u <- maybeToList $ rayIntersect windowDirection ray ]
  iP = pAdjustOffset p i
  jP = pAdjustOffset p j
  windowDirection = (windowCommon, windowOpposite)
  (windowCommon, iWindow, jWindow, windowOpposite)
    | isStraightLine
    = let p1 = lerp 0.5 (pAccess p i) (pAccess p j)
          p2 = case p1 - pAccess p i of
            V2 x y -> p1 + V2 (-y) x -- rotate 90 degrees.
      in  (p1, undefined, undefined, p2)
    | otherwise
    = fromMaybe
        (error $ "No window overlap: " ++ show
          (isStraightLine, isGrandparent, oneBendBetween p i j)
        )
      $ listToMaybe
      $ [ (p1, p2, p3, p2+p3-p1)
        | (a, b)       <- ssspWindows iP
        , (c, d)       <- ssspWindows jP
        , (p1, p2, p3) <- if
          | a == c    -> pure (a, b, d)
          | a == d    -> pure (a, b, c)
          | b == c    -> pure (b, a, d)
          | b == d    -> pure (b, a, c)
          | otherwise -> []
        ]
  isForward (a, b) v = not (isBetween a (b, v))

-- 0..i,s,j..n-1
-- i..j,s
split2Link :: Polygon -> Int -> Int -> (Polygon, Polygon)
split2Link p i j | j < i = split2Link p j i
split2Link p i j = (mkPolygon $ V.fromList left, mkPolygon $ V.fromList right)
 where
  s     = steiner2Link p i j
  n     = pSize p
  left  = map (pAccess p) [0 .. i] ++ [s] ++ map (pAccess p) [j .. n - 1]
  right = map (pAccess p) [i .. j] ++ [s]

data Link = OneLink | TwoLink

splitNLink :: Polygon -> Int -> [(Link, Int)] -> (Polygon, Polygon)
splitNLink p i js = (mkPolygon $ V.fromList left, mkPolygon $ V.fromList right)
 where
  n        = pSize p
  left = map (pAccess p) [0 .. i] ++ steiners ++ map (pAccess p) [j .. n - 1]
  right    = map (pAccess p) [i .. j] ++ reverse steiners
  j        = snd (last js)
  steiners = splitNLinks p i js
-- 0, [(TwoLink,2),(OneLink,3)]
-- 0, [(OneLink,5),(TwoLink,3)]
splitNLinks :: Polygon -> Int -> [(Link, Int)] -> [V2 Rational]
splitNLinks _p _i []              = []
splitNLinks p  i  [(TwoLink, j )] = [steiner2Link p i j]
splitNLinks _p _i [(OneLink, _j)] = []
splitNLinks p i ((TwoLink, j) : (OneLink, j') : xs) =
  let
    (l, r) = split2Link p i j
    p'     = selectContains l r (pAccess p j')
    s      = steiner2Link p i j
    sIdx =
      fromMaybe (error "missing steiner") $ V.elemIndex s (polygonPoints p')
  in
    s : splitNLinks p' sIdx ((TwoLink, j') : xs)
splitNLinks p i ((TwoLink, j) : (TwoLink, j') : xs) =
  let
    (l, r) = split2Link p i j
    p'     = selectContains l r (pAccess p j')
    s      = steiner2Link p i j
    sIdx =
      fromMaybe (error "missing steiner") $ V.elemIndex s (polygonPoints p')
  in
    s : splitNLinks p' sIdx ((OneLink, j) : (TwoLink, j') : xs)
splitNLinks p i ((OneLink, j) : (TwoLink, j') : xs) =
  let
    (l, r) = split2Link p j j'
    p'     = selectContains l r (pAccess p i)
    p''    = selectContains l r (pAccess p j')
    s      = steiner2Link p j j'
    s'     = steiner2Link p' i sIdx
    sIdx   = fromMaybe (error "missing steiner sIdx")
      $ V.elemIndex s (polygonPoints p')
    sIdx' = fromMaybe (error "missing steiner sIdx'")
      $ V.elemIndex s' (polygonPoints p'')
  in
    s' : s : splitNLinks p'' sIdx' ((OneLink, j') : xs)
splitNLinks _p _i _ = error "splitNLinks: invalid input"

selectContains :: Polygon -> Polygon -> V2 Rational -> Polygon
selectContains p1 p2 elt | V.elem elt (polygonPoints p1) = p1
                         | V.elem elt (polygonPoints p2) = p2
                         | otherwise = error "elt not member of either polygons"
-- [(P, Vis)] -> Polygon -> [P]
{-
If there is a 1-link path, connect it.
  Recurse into the two new polygon pairs.

If there is a 2-link path, connect it, possibly add bogus steiner point.
  Recurse into the two new polygon pairs.

If there is a n-link path, connect it, add bogus steiner points.
  Recurse into the two new polygon pairs.
-}

-- a 2 b 1 c
-- ab = 2-link of (a,b)
-- abc = 2-link of (cut-a-ab-b, c)
-- steps: a ab abc c
-- a, [(2,b), (1,c)]
-- [(ab, p1,p2)]

{-
, mkGroup
  [ withStrokeColor "green" $mkGroup
    [ mkLine (x1,y1) (x2,y2)
    , mkLine (x1',y1') (x2',y2') ]
  | (a,b) <- mWins
  , (i,j) <- oWins
  , a == i || a == j || b == i || b == j
  , not (sort [a,b] == sort [i,j])
  , let V2 x1 y1 = realToFrac <$> a
        V2 x2 y2 = realToFrac <$> b
        V2 x1' y1' = realToFrac <$> i
        V2 x2' y2' = realToFrac <$> j
  ]
-}



type Points = V.Vector (V2 Rational)
type Edges = [(Int, Int, Int)]
data Mesh = Mesh { meshPoints :: Points, meshEdges :: Edges }
data MeshPair = MeshPair Points Points Edges
-- The points in a RelMesh are:
--   relMeshStatic ++ x where Ax = B
-- data RelMesh = RelMesh
--   { relMeshStatic :: Points
--   , relMeshEdges  :: Edges
--   , relMeshA      :: Matrix Double
--   , relMeshB      :: Matrix Double
--   }
-- data RelMeshPair = RelMeshPair Points Edges (Matrix Double) (Matrix Double) (Matrix Double) (Matrix Double)
--
--

-- Use SSSP to find 1-link connections
-- Compute visibility windows for each node
-- Check windows for ovelap, giving 2-link connections.
-- Use floyd to generate all other connections
-- (a,b,1)
-- (a,b,2)
-- floyd
compatiblyTriangulateP :: Polygon -> Polygon -> [(Polygon, Polygon)]
compatiblyTriangulateP a b
  | pSize a /= pSize b = error "polygon size mismatch"
  | otherwise = traceSVG (showStep a b) $ compatiblyTriangulateP'
    (pSetOffset a 0)
    (pSetOffset a 0)
    (pSetOffset b 0)

showStep :: Polygon -> Polygon -> SVG
showStep a b = mkGroup
  [ translate (-3) 0 $ scale 2
    $ mkGroup [withFillColor "grey" $ polygonShape a, polygonNumDots a]
  , translate 3    0 $ scale 2
    $ mkGroup [withFillColor "grey" $ polygonShape b, polygonNumDots b]
  ]

showSolution :: Polygon -> Polygon -> (Int, Int) -> SVG
showSolution a b (nodeL, nodeR) = mkGroup
  [ showStep a b
  -- , translate (-3) 0 $ scale 2 $
  --   translate xA yA $ withFillColor "red" $
  --   mkCircle 0.05
  , withFillColor "white" $ latex $ T.pack $ show (nodeL, nodeR)
  ]
  where
    -- V2 xA yA = realToFrac <$> steiner2Link a nodeL nodeR
    -- V2 xB yB = steiner2Link b nodeL nodeR

compatiblyTriangulateP' :: HasCallStack => Polygon -> Polygon -> Polygon -> [(Polygon, Polygon)]
compatiblyTriangulateP' aOrigin a b
  | n == 3 = traceSVG (showStep a b) $ {- trace ("Done") $ -} [(a, b)]
  | otherwise =
    -- trace ("PolygonA " ++ show a) $
    -- trace ("PolygonB " ++ show b) $
    -- trace ("pSize: " ++ show (pSize a)) $
    -- trace ("aOneLink: " ++ show aOneLink) $
    -- trace ("bOneLink: " ++ show bOneLink) $
    -- trace ("aTwoLink: " ++ show aTwoLink) $
    -- trace ("bTwoLink: " ++ show bTwoLink) $
    -- trace ("bestOneLink: " ++ show bestOneLink) $
    -- trace ("bestTwoLink: " ++ show bestTwoLink) $
    traceSVG (showStep a b) $ case bestOneLink of
    Nothing -> case bestTwoLink of
      Nothing -> error $ "no 2-links"
      Just (nodeL, nodeR) -> -- trace "Creating steiner node" $
            let isTwolink =
                  (nodeL, nodeR) `notElem` aOneLink ||
                  (nodeL, nodeR) `notElem` bOneLink
                (aL, aR) = if (nodeL, nodeR) `elem` aOneLink
                  then split1Link a nodeL nodeR 1
                  else split2Link a nodeL nodeR
                (bL, bR) = if (nodeL, nodeR) `elem` bOneLink
                  then split1Link b nodeL nodeR 1
                  else split2Link b nodeL nodeR
            in (if isTwolink then traceSVG (showSolution a b (nodeL, nodeR)) else id) $
             (compatiblyTriangulateP' aOrigin aL bL
             ++ compatiblyTriangulateP' aOrigin aR bR)
    Just (nodeL, nodeR) ->
      {-trace
          (show ("one link" :: String, toOriginIndex nodeL, toOriginIndex nodeR)
          )
        $ -}
          let (aL, aR) = split1Link a nodeL nodeR 0
              (bL, bR) = split1Link b nodeL nodeR 0
          in  compatiblyTriangulateP' aOrigin aL bL
                ++ compatiblyTriangulateP' aOrigin aR bR
 where
  -- toOriginIndex idx =
  --   ( idx
  --   , fromMaybe (-1) (V.elemIndex (pAccess a idx) (polygonPoints aOrigin))
  --     + polygonOffset aOrigin
  --   )
  n = pSize a
  bestOneLink =
    listToMaybe (sortOn (Down . nodeDist) (aOneLink `intersect` bOneLink))
  bestTwoLink = listToMaybe
    (sortOn (Down . nodeDist)
            ((aOneLink ++ aTwoLink) `intersect` (bOneLink ++ bTwoLink))
    )
  aOneLink = polygonOneLinks a
  bOneLink = polygonOneLinks b
  aTwoLink = polygonTwoLinks a
  bTwoLink = polygonTwoLinks b
  nodeDist (i, j) = (min (j - i) (n - j + i), hash (i,j))

oneBendBetween :: Polygon -> Int -> Int -> Bool
oneBendBetween _p _a _b = False
-- oneBendBetween p a b =
--     direction
--       (pAccess p b)
--       (pAccess p (pParent p a b))
--       (pAccess p $ obstructedBy b) == 0 &&
--     (pParent p a b) /= obstructedBy b
--   where
--     obstructedBy n =
--       case pParent p a n of
--         i -> if i == a then n else obstructedBy i

polygonTwoLinks :: Polygon -> [(Int, Int)]
polygonTwoLinks p =
  [ (i, j)
  | i <- [0 .. n - 1]
  , j <- [i + 2 .. n - 1]
  , not (i == 0 && j == n - 1)
  , pParent p i j /= i -- check for 1-link
  , let isTwoLink      = pParent p i (pParent p i j) == i
        isStraightLine = oneBendBetween p i j
-- Points on a straight line should be 2-link even though they are not
-- direct grandparents.
  , isTwoLink || isStraightLine
  ]
  where n = pSize p

polygonOneLinks :: Polygon -> [(Int, Int)]
polygonOneLinks p =
  [ (i, j)
  | i <- [0 .. pSize p - 1]
  , j <- [i + 2 .. pSize p - 1]
  , not (i == 0 && j == pSize p - 1)
  , pParent p i j == i
  ]
