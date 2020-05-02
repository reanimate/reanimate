{-# LANGUAGE MultiWayIf #-}
module Reanimate.Math.Balloon where

import           Control.Lens
import qualified Data.Vector            as V
import           Graphics.SvgTree       (drawAttributes)
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           Reanimate.Svg.Constructors
import           Reanimate.Animation
import           Reanimate.Math.Polygon
import           Reanimate.Math.EarClip
import           Reanimate.Math.SSSP
import           Reanimate.Morph.Common (toShapes)

import           Debug.Trace

balloon :: SVG -> (Double -> SVG)
balloon svg = \t ->
    mkGroup
    [ polygonShape (gen t) & drawAttributes .~ attr
    | (attr, gen) <- lst ]
  where
    polygonShape :: Polygon -> SVG
    polygonShape p = mkLinePathClosed
      [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList (polygonPoints p) ++ [pAccess p 0] ]
    lst =
      [ (attr, balloonP $ shiftLongestDiameter poly)
      | (attr, poly) <- toShapes 0.01 svg
      ]

-- x <= 1
-- diameter (balloonP x p) = diameter p * x
balloonP :: Polygon -> Double -> Polygon
balloonP p = \t ->
    let targetLength = d * t
        nodeVisible x = ds V.! x <= targetLength
        -- Move 'a' closer to 'target' such that the length from point 0 to 'a'
        -- is targetLength.
        moveCloser :: Int -> V2 Rational -> V2 Rational
        moveCloser target a =
          let targetDist = ds V.! target
              aDist = distance' (pAccess p target) a
              frac = min 1 $ realToFrac $ (targetLength - targetDist) / aDist
          in lerp frac a (pAccess p target)
        worker 0 = [pAccess p 0]
        worker a =
          let b = pNext p a in
          if nodeVisible a && nodeVisible b
            then pAccess p a : pAccess p b : []
            else
              chunkRight a b (pAccess p a) (pAccess p b) (fst $ getFunnel a b) ++
              chunkCenter a b ++
              chunkLeft a b (pAccess p a) (pAccess p b) (snd $ getFunnel a b)
        chunkRight ai bi a b (x:y:xs) =
          case rayIntersect (a,b) (pAccess p x,pAccess p y) of
            Just u ->
              if nodeVisible x
                then
                    map (moveCloser x) (split a u) ++
                    chunkRight ai bi u b (y:xs)
                else chunkRight ai bi u b (y:xs)
            _ -> -- error $ "chunkRight: urk: " ++ show (ai,bi,x,y)
              if nodeVisible x
                then map (moveCloser x) [a]
                else []
        chunkRight ai bi a b _ = []
        chunkLeft ai bi a b (x:y:xs) =
          case rayIntersect (a,b) (pAccess p x,pAccess p y) of
            Just u ->
              if nodeVisible x
                then
                    chunkLeft ai bi a u (y:xs) ++
                    map (moveCloser x) (split u b)
                else chunkLeft ai bi a u (y:xs)
            _ -> -- error $ "chunkLeft: urk: " ++ show (ai,bi, x,y)
              if nodeVisible x
                then map (moveCloser x) [b]
                else []
        chunkLeft ai bi a b _ = []
        chunkCenter a b =
          let (aF, bF) = getFunnel a b
              aP = pAccess p a
              bP = pAccess p b in
          case (reverse aF, reverse bF) of
            ([x], [_]) | nodeVisible x ->
                map (moveCloser x) (split aP bP)
            ([x], _:left:_) | nodeVisible x ->
              case rayIntersect (aP,bP) (pAccess p x,pAccess p left) of
                Just v  -> map (moveCloser x) (split aP v)
                Nothing -> map (moveCloser x) [aP,bP]
            (x:right:_, [_]) | nodeVisible x ->
              case rayIntersect (aP,bP) (pAccess p x,pAccess p right) of
                Just u  -> map (moveCloser x) (split u bP)
                Nothing -> map (moveCloser x) [aP,bP] -- error $ "urk: " ++ show (a,b, right)
            (x:right:_, _:left:_) | nodeVisible x ->
              case rayIntersect (aP,bP) (pAccess p x,pAccess p right) of
                Just u ->
                  case rayIntersect (aP,bP) (pAccess p x,pAccess p left) of
                    Just v  -> map (moveCloser x) (split u v)
                    Nothing -> map (moveCloser x) [aP,bP]
                Nothing -> map (moveCloser x) [aP,bP]
            _ -> []
    in mkPolygon $ V.fromList $ clearDups $ concatMap worker [0..polygonSize p-1]
  where
    clearDups (x:y:xs)
      | x == y = clearDups (x:xs)
    clearDups (x:xs) = x : clearDups xs
    clearDups [] = []

    getParents 0 = []
    getParents x = ssspTree V.! x : getParents (ssspTree V.! x)
    getFunnel a b =
      let aP = getParents a
          bP = getParents b in
      (takeUntil (`elem` bP) aP
      ,takeUntil (`elem` aP) bP)
    split aP bP =
      let steps = 50 in
      [ lerp (t/steps) bP aP
      | t <- [0 .. steps]
      ]
    ssspTree = sssp (polygonRing p) (dual 0 $ earClip $ polygonPoints  p)
    d = V.maximum ds
    ds = ssspDistances p ssspTree

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil fn [] = []
takeUntil fn (x:xs)
  | fn x = [x]
  | otherwise = x : takeUntil fn xs

diameter :: Polygon -> Double
diameter p = V.maximum (ssspDistances p ssspTree)
  where
    ssspTree = sssp (polygonRing p) (dual 0 $ earClip $ polygonPoints p)

shiftLongestDiameter :: Polygon -> Polygon
shiftLongestDiameter p = findBest 0 p (cyclePolygons p)
  where
    margin = 0.25
    findBest score elt [] = elt
    findBest score elt (x:xs) =
      let newScore = 0 in --diameter x in
      if
        -- | newScore-score > score * margin    -> findBest newScore x xs
        -- | score-newScore > newScore * margin -> findBest score elt xs
        | isTopLeft x elt                    -> findBest newScore x xs
        | otherwise                          -> findBest score elt xs
    isTopLeft a b =
      case V.head (polygonPoints a)-V.head (polygonPoints b) of
        V2 x y -> y > x

-- Shortest distances from point 0 to all other points.
ssspDistances :: Polygon -> SSSP -> V.Vector Double
ssspDistances p sssp = arr
  where
    arr = V.generate (polygonSize p) $ \i ->
      case i of
        0 -> 0
        _ ->
          let parent = sssp V.! i in
          arr V.! parent + distance' (pAccess p parent) (pAccess p i)

-- sssp :: Polygon -> Dual -> SSSP
-- dual :: Triangulation -> Dual
-- earClip :: Polygon -> Triangulation
