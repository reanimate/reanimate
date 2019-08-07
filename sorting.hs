{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as GV
import System.Random.Shuffle
import System.Random
import Debug.Trace

main :: IO ()
main = print $ length $ runSort bubbleSort 2560

-- [3,4,1,2]
-- [

-- [1,2,3,4]
-- 0 3
-- half: 0 + 3`div`2 = 1
-- mergeSort 0 (0+1)
-- mergeSort (0+1) 3

data Env s = Env
  { envHistory :: [Vector Int]
  , envState :: V.MVector s Int }

type S s a = StateT (Env s) (ST s) a

runSort :: (forall s. S s ()) -> Int -> [Vector Int]
runSort sortFn len = reverse $ runST (do
    arr <- V.thaw (V.fromList lst)
    let env = Env [] arr
    envHistory <$> execStateT sortFn env)
  where
    lst = shuffle' [1 .. len] len (mkStdGen 0xDEADBEEF)
    skipDups (x:y:xs) | x == y = skipDups (x:xs)
    skipDups (x:xs) = x : skipDups xs
    skipDups [] = []

readS :: Int -> S s Int
readS idx = do
  arr <- gets envState
  GV.unsafeRead arr idx

writeS :: Int -> Int -> S s ()
writeS idx val = do
  arr <- gets envState
  GV.unsafeWrite arr idx val

swapS :: Int -> Int -> S s ()
swapS a b = do
  arr <- gets envState
  GV.unsafeSwap arr a b

inputLength :: S s Int
inputLength = GV.length <$> gets envState

snapshot :: S s ()
snapshot = do
  arr <- gets envState
  vec <- V.freeze arr
  modify $ \st -> st { envHistory = vec : envHistory st }

mergeSort :: S s ()
mergeSort = do
  snapshot
  len <- inputLength
  mergeSort' 0 (len-1)

mergeSort' :: Int -> Int -> S s ()
mergeSort' start end | start == end = return ()
mergeSort' start end = do
  let half = start + (end-start) `div` 2
  mergeSort' start half
  mergeSort' (half+1) end
  leftVals <- mapM readS [start .. half]
  rightVals <- mapM readS [half+1 .. end]
  zipWithM_ writeS [start..] (merge leftVals rightVals)
  snapshot
 
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys


mergeSortUp :: S s ()
mergeSortUp = do
  snapshot
  len <- inputLength
  let chunkSizes = takeWhile (< len) $ map (2^) [0..]
  forM_ chunkSizes $ bottomUpMergeSort'

bottomUpMergeSort' :: Int -> S s ()
bottomUpMergeSort' chunkSize = do
  len <- inputLength
  forM_ [0, chunkSize*2 .. len-1] $ \idx -> do
    leftVals <- mapM readS (take chunkSize [idx .. len-1])
    rightVals <- mapM readS (take chunkSize (drop chunkSize [idx .. len-1]))
    zipWithM_ writeS [idx..] (merge leftVals rightVals)
    snapshot

insertSort :: S s ()
insertSort = do
  snapshot
  len <- inputLength
  forM_ [1 .. len-1] $ \j -> do
    a <- readS j
    worker a j
    snapshot
  where
    worker a 0 = writeS 0 a
    worker a j = do
      b <- readS (j-1)
      if (a < b)
        then do
          writeS j b
          worker a (j-1)
        else
          writeS j a


bubbleSort :: S s ()
bubbleSort = do
  worker True 0
  where
    worker True 0 = do
      snapshot
      len <- inputLength
      worker False (len-1)
    worker False 0 = snapshot
    worker changed n = do
      a <- readS n
      b <- readS (n-1)
      if a < b
        then do
          writeS n b
          writeS (n-1) a
          worker True (n-1)
        else worker changed (n-1)

quicksort :: S s ()
quicksort = do
    snapshot
    len <- inputLength
    worker 0 (len-1)
  where
    worker lo hi | lo >= hi = return ()
    worker lo hi = do
      pivot <- readS hi
      p <- partition pivot lo lo hi
      snapshot
      worker lo (p-1)
      worker (p+1) hi
    partition pivot i lo hi | lo==hi = do
      swapS i hi
      return i
    partition pivot i lo hi = do
      jVal <- readS lo
      if jVal < pivot
        then do
          swapS i lo
          partition pivot (i+1) (lo+1) hi
        else
          partition pivot i (lo+1) hi

