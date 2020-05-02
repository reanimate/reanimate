module Reanimate.Math.Visibility where

-- import qualified Data.Set              as Set
import           Linear.V2
import Data.Maybe

import           Reanimate.Math.Polygon

-- import           Debug.Trace

visibility :: [P] -> [P]
visibility (z:v:vs) = reverse $ go z [v,z] vs
visibility _        = undefined

-- visibility (z:v0:v1:rest)
--   | isLeftTurn z v0 v1 = left z [v1,v0] rest
--   | otherwise = scanA

-- Three cases:
--   v is visible: zsv is a left-turn
--   v moves in front of the stack: s'sv is right turn
--   v moves behind stack:
go :: (Ord a, Fractional a) => V2 a -> [V2 a] -> [V2 a] -> [V2 a]
go _z stack [] = stack
go z stack@(s:s':_ss) (v:vs)
  -- | isLeftTurn z s v && isRightTurn s' s v = trace ("FF: " ++ show (z,s,s',v)) $ fastForward z stack s (v:vs)
  | isLeftTurn z s v  = {-trace ("Left: " ++ show (z,s,v)) $ -}go z (v:stack) vs
  | isLeftTurn s' s v = {-trace ("Right: " ++ show (s',s,v,vs)) $ -}rightTurn z stack v vs
  | otherwise         = {-trace ("FF: " ++ show (z,s,s',v)) $ -}fastForward z stack s (v:vs)
go _ _ _ = undefined

{-
z: 2,2
stack: [1,1  2,6]
v: 0,1
-}

rightTurn :: (Ord a, Fractional a) => V2 a -> [V2 a] -> V2 a -> [V2 a] -> [V2 a]
rightTurn z stack' v (v1:vs)
  | isRightTurn z v v1 = {-trace ("Double right: " ++ show (v,v1)) $ -}rightTurn z stack v1 vs
  | isLeftTurn z v v1 && isRightTurn (head stack') v v1
    = {-trace ("Right->Left: " ++ show (v,v1)) $ -}go z (v:stack) (v1:vs)
  | otherwise
    = {-trace ("Scan: " ++ show (v, stack')) $ -}scanc z stack v (v1:vs)
  where
    stack@(_s1:_ss) = unwindStack z stack' v (v1:vs)
rightTurn z stack v [] = unwindStack z stack v []

-- scan forwards until edge intersects zv ray
scanc :: (Ord a, Fractional a) => V2 a -> [V2 a] -> V2 a -> [V2 a] -> [V2 a]
scanc z stack v (v1:v2:vs)
  | isBetween u (v1,v2) = -- trace ("Found: " ++ show (u, stack)) $
    go z (u:stack) (v2:vs)
    --rightTurn z stack u (v2:vs)
  | otherwise           = scanc z stack v (v2:vs)
  where
    Just u = rayIntersect (z,v) (v1,v2)
scanc _z stack _v _vs = stack

unwindStack :: (Ord a, Fractional a) => V2 a -> [V2 a] -> V2 a -> t -> [V2 a]
unwindStack z (s1:s2:ss) v vs
  | isRightTurn z s1 v && isLeftTurn z s2 v = (u:s2:ss)
  | otherwise                               = unwindStack z (s2:ss) v vs
  where
    Just u = rayIntersect (z,v) (s1,s2)
unwindStack _z stack _v _vs = stack

-- We've moved into shadow. There are three ways out:
--  1. Find edge that crosses zv from right to left.
--     In this case, push intersection point to the stack and continue as normal.
--  2. Find edge that crosses zv from left to right and is above v
--     unwind stack
--  3. Find edge that crosses zv from left to right and is below v

fastForward :: (Ord a, Fractional a) => V2 a -> [V2 a] -> V2 a -> [V2 a] -> [V2 a]
fastForward z stack v (v1:v2:vs)
  | isNothing i || not (isBetween u (v1, v2))        = {-trace ("FF past: " ++ show (z,v,v1,v2)) $ -}fastForward z stack v (v2:vs)
  | distSquared v u > distSquared z u = {-trace ("FF skip: " ++ show (z,v,v1,v2)) $ -}fastForward z stack v (v2:vs)
  | isLeftTurn z u v2                 = {-trace ("FF to: " ++ show u) $ -}go z (v2:u:stack) vs
  | distSquared z v < distSquared z u = {-trace ("FF unwind: " ++ show (z,v,v1,v2,u)) $ -}unwindStack z stack v2 (vs)
  | otherwise                         = fastForward z stack v (v2:vs)
  where
    i = rayIntersect (z, v) (v1, v2)
    Just u = rayIntersect (z, v) (v1, v2)
fastForward _z stack _v _vs = stack

{-
v2: 2,1
v1: 1,1
v: 1,0
-}

{-
left z [] [] = finish
left z stack@(s1:s2:ss) (v1:vs)
  | isLeftTurn z s1 v1
    = left z (v1:stack) rest
  | isRightTurn z s1 v1 && isRightTurn s2 s1 v1
    = scanA
  | otherwise
    = right

-- We've made a right turn and need to find the edge in the stack that
-- intersects the zv ray.
-- zv intersects the segment ab iff zav is right-turn and zbv is a left-turn.
right z [] v vs = []
right z (s1:s2:ss) v1 (v2:vs)
  | isRightTurn z s1 v1 && isLeftTurn z s2 v1
  = let u = lineIntersect (z,v) (s1,s2)
    in if isRightTurn z v1 v2
      then right
      else if isLeftTurn z v1 v2 && isRightTurn
  -}

-- Joe and Simpson.
-- vispol (z:v0:v1:vs)
--   | isLeftTurn z v0 v1 = left z [v1,v0] vs
--   | otherwise = scana z [v0] (v1:vs)
