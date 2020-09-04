{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Reanimate.Math.Balloon
  ( balloon
  , balloon'
  ) where

import           Control.Lens
import qualified Data.Vector                as V
import           Graphics.SvgTree           (drawAttributes)
import           Linear.V2
import           Linear.Vector
import           Reanimate.Animation
import           Reanimate.Math.Common
import           Reanimate.Math.Polygon
import           Reanimate.Morph.Common     (toShapes)
import           Reanimate.Svg.Constructors

-- import           Debug.Trace

-- | Inflate SVG shapes like a balloon. This works by hiding corners
--   that are more than @t@ percent distant from the starting point
--   relative to the maximum diameter of the shape.
--
--   Example:
--
-- @
-- 'animate' $ 'balloon' ('scale' 8 $ 'center' $ 'Reanimate.LaTeX.latex' \"X\")
-- @
--
--   <<docs/gifs/doc_balloon.gif>>
balloon :: SVG -> (Double -> SVG)
balloon = balloon' 0.01

-- | Same as @balloon'@ but with a given tolerance for converting
--   SVG shapes to polygons.
balloon' :: Double -> SVG -> (Double -> SVG)
balloon' tol svg = \t ->
    mkGroup
    [ polygonShape (gen t) & drawAttributes .~ attr
    | (attr, gen) <- lst ]
  where
    polygonShape :: Polygon -> SVG
    polygonShape p = mkLinePathClosed
      [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList (polygonPoints p) ]
    lst =
      [ (attr, balloonP $ shiftLongestDiameter poly)
      | (attr, poly) <- toShapes tol svg
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
            then [pAccess p a, pAccess p b]
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
        chunkRight _ai _bi _a _b _ = []
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
        chunkLeft _ai _bi _a _b _ = []
        chunkCenter a b =
          let (aF, bF) = getFunnel a b
              aP = pAccess p a
              bP = pAccess p b in
          case (reverse aF, reverse bF) of
            ([x], [_]) | nodeVisible x ->
                map (moveCloser x) (split aP bP)
            ([x], _:left:_) | nodeVisible x ->
              case rayIntersect (aP,bP) (pAccess p x,pAccess p left) of
                Just v  ->
                  map (moveCloser x) (split aP v)
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
    in mkPolygon $ V.fromList $ clearDups $
        concatMap worker [0..pSize p-1]
  where
    clearDups (x:y:xs)
      | x == y = clearDups (x:xs)
    clearDups (x:xs) = x : clearDups xs
    clearDups [] = []

    getParents 0 = []
    getParents x =
      let parent = pParent p 0 x
      in parent : getParents parent
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
    d = V.maximum ds
    ds = ssspDistances p

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _fn [] = []
takeUntil fn (x:xs)
  | fn x = [x]
  | otherwise = x : takeUntil fn xs

diameter :: Polygon -> Double
diameter p = V.maximum (ssspDistances p)

shiftLongestDiameter :: Polygon -> Polygon
shiftLongestDiameter p = findBest 0 p (pCycles p)
  where
    margin = 0.01
    findBest _score elt [] = elt
    findBest score elt (x:xs) =
      let newScore = diameter x in
      if
        | newScore-score > score * margin    -> findBest newScore x xs
        | score-newScore > newScore * margin -> findBest score elt xs
        | isTopLeft x elt                    -> findBest newScore x xs
        | otherwise                          -> findBest score elt xs
    isTopLeft a b =
      case pAccess a 0-pAccess b 0 of
        V2 x y -> y > x

-- Shortest distances from point 0 to all other points.
ssspDistances :: Polygon -> V.Vector Double
ssspDistances p = arr
  where
    arr = V.generate (pSize p) $ \i ->
      case i of
        0 -> 0
        _ ->
          let parent = pParent p 0 i in
          arr V.! parent + distance' (pAccess p i) (pAccess p parent)
