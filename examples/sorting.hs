#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE PackageImports    #-}
module Main (main) where

import           Codec.Picture
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Text                   (Text)
import qualified Data.Vector.Generic.Mutable as GV
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as V
import           Reanimate
import           System.Random
import "random-shuffle" System.Random.Shuffle

main :: IO ()
main = reanimate $
  demonstrateAlgorithm "Bubble sort" bubbleSort `seqA`
  demonstrateAlgorithm "Merge sort (left leaning)" mergeSort `seqA`
  demonstrateAlgorithm "Merge sort" mergeSortUp `seqA`
  demonstrateAlgorithm "Insertion sort" insertSort `seqA`
  demonstrateAlgorithm "Selection sort" selectionSort `seqA`
  adjustDuration (*3) (demonstrateAlgorithm "Quicksort" quicksort)

demonstrateAlgorithm :: Text -> (forall s. S s ()) -> Animation
demonstrateAlgorithm name algo = mkAnimation 10 $ \t ->
    let img = generateImage pixelRenderer width height
        seed = round (t * 3000)
        pixelRenderer x y = turbo (fromIntegral num / fromIntegral width)
          where
            num = (sortedDat !! y) V.! x
        sortedDat = runSort' seed algo width
        -- width = 1024
        width = 500
        height = length sortedDat
    in mkGroup
      [ mkBackground "black"

      , translate 0 (-screenWidth*0.03) $ center $ scaleXY (-1) 1 $
        scaleToSize 7.5 7.5 $ embedImage img
      , translate 0 (screenWidth*0.24) $ withFillColor "white" $ scale 1 $ center $
        latex name
      , withFillColor "white" $
        translate (-screenWidth*0.26) (-screenHeight*0.05) $
        rotate (-90) $ scale 0.5 $ center $
        latex "$Time \\rightarrow$"
      , withFillColor "white" $ translate (screenWidth*0.30) 0 $
        mkCircle ((1-t)*0.5)
      ]


-- main :: IO ()
-- main = print $ length $ runSort bubbleSort 2560

-- [3,4,1,2]
-- [

-- [1,2,3,4]
-- 0 3
-- half: 0 + 3`div`2 = 1
-- mergeSort 0 (0+1)
-- mergeSort (0+1) 3

data Env s = Env
  { envHistory :: [Vector Int]
  , envState   :: V.MVector s Int }

type S s a = StateT (Env s) (ST s) a

-- runSort :: (forall s. S s ()) -> Int -> [Vector Int]
-- runSort = runSort' 0xDEADBEEF

runSort' :: Int -> (forall s. S s ()) -> Int -> [Vector Int]
runSort' seed sortFn len = reverse $ runST (do
    arr <- V.thaw (V.fromList lst)
    let env = Env [] arr
    envHistory <$> execStateT sortFn env)
  where
    lst = shuffle' [1 .. len] len (mkStdGen seed)

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

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys


mergeSortUp :: S s ()
mergeSortUp = do
  snapshot
  len <- inputLength
  let chunkSizes = takeWhile (< len) $ map (2^) [0::Int ..]
  forM_ chunkSizes $ bottomUpMergeSort'

bottomUpMergeSort' :: Int -> S s ()
bottomUpMergeSort' chunkSize = do
  len <- inputLength
  forM_ [0, chunkSize*2 .. len-1] $ \idx -> do
    leftVals <- mapM readS (take chunkSize [idx .. len-1])
    rightVals <- mapM readS (take chunkSize (drop chunkSize [idx .. len-1]))
    zipWithM_ writeS [idx..] (merge leftVals rightVals)
    snapshot

selectionSort :: S s ()
selectionSort = do
  snapshot
  len <- inputLength
  forM_ [0 .. len-1] $ \j -> do
    i <- findMin j (j+1) len
    swapS j i
    snapshot
  where
    findMin j i len | i >= len = return j
    findMin j i len = do
      jVal <- readS j
      iVal <- readS i
      if iVal < jVal
        then findMin i (i+1) len
        else findMin j (i+1) len

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
      if a < b
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
          when (n `mod` 50 == 0) snapshot
          worker True (n-1)
        else worker changed (n-1)

quicksort :: S s ()
quicksort = do
    snapshot
    len <- inputLength
    worker [(0, len-1)]
  where
    worker :: [(Int,Int)] -> S s ()
    worker [] = return ()
    worker ((lo,hi):rest) = do
      pivot <- readS (lo + (hi-lo) `div` 2)
      p <- partition pivot lo hi
      snapshot
      worker $ insertWork (lo, p) $ insertWork (p+1, hi) $ rest

    partition pivot lo hi = do
      loVal <- readS lo
      hiVal <- readS hi
      if loVal < pivot
        then partition pivot (lo+1) hi
        else if hiVal > pivot
          then partition pivot lo (hi-1)
          else if lo >= hi
            then return hi
            else do
              writeS lo hiVal
              writeS hi loVal
              snapshot
              partition pivot (lo+1) (hi-1)

    insertWork :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
    insertWork (lo, hi) rest | lo >= hi = rest
    insertWork (lo, hi) [] = [(lo, hi)]
    insertWork (lo, hi) ((lo', hi'):rest)
      | hi-lo > hi'-lo' = (lo,hi) : (lo', hi') : rest
      | otherwise       = (lo', hi') : insertWork (lo, hi) rest
