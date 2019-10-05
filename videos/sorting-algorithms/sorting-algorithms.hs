#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Control.Lens          ()

import           Codec.Picture.Types
import           Data.Fixed
import           Control.Monad
import           Data.List
import qualified Data.Text             as T
import qualified Geom2D.CubicBezier    as Bezier
import           Graphics.SvgTree      (Number (..), Tree)
import           Numeric
import           Reanimate.Animation
import           Reanimate.ColorMap
import           Reanimate.Driver      (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Signal
import           Reanimate.Svg
import           Reanimate.Constants
import           Reanimate.Scene
import           System.Random
import           System.Random.Shuffle
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V

fixed :: Tree -> Animation -> Animation
fixed svg ani = animate (const svg) `parA` ani

digitWidth = screenWidth/10
digitCount = 10

main :: IO ()
main = reanimate $ fixed bg $ pauseAtEnd 1 $
    -- sceneAnimation (bubbleSort lst) `seqA` sceneAnimation (simpleSort lst)
    sceneAnimation (simpleSort_ lst)
    -- mkAnimation 5 $ \t ->
    --   withFillColor "white" $ translate (negate $ digitWidth*digitCount/2) 0 $
        -- sortingTransition (zip [9,0,1,2,3,4,5,6,8,7] squares) s
        -- sortingTransition (zip [9,0,1,2,3,4,5,6,8,7] digits) s
        -- sortingTransition (zip [1,0,2,3,4,5,6,7,8,9] digiSquares) s
        -- jumpTransition (zip [1,0,2,3,4,5,6,7,8,9] digiSquares) s
        -- renderSortElements (mkJumpSorted lst) t

  where
    seed = 0xDEADBEEF
    lst = shuffle' (zip [0..] digiSquares) 10 (mkStdGen seed)
    bg = mkBackground "black"
    msg = "0 1 2 3 4 5 6 7 8 9"
    digits =
      map (withStrokeColor "black" . withStrokeWidth 0.01 . withFillColor "white") $
      map (lowerTransformations . scale 1 . pathify . center . latex . T.pack . show) [0..9]
    squares = map center [ withFillColorPixel (promotePixel $ viridis (n/9)) $
      mkRect (digitWidth*1.00) digitWidth | n <- [0..9]]

    digiSquares = zipWith (\a b -> mkGroup [a,b]) squares digits

    glyphs = lowerTransformations $ scale 3 $ pathify $ center $ latexAlign msg
    fillText = mkAnimation 1 $ \t ->
      let sat = fromToS 0 0.7 t in
      withFillColor "white" $ withStrokeColor "white" $ withStrokeWidth (0.4 * (1-t)) $
        withFillOpacity t glyphs
        -- withSubglyphs [0] (withFillColorPixel $ toRGBString sat 0.0) $
        -- withSubglyphs [1] (withFillColorPixel $ toRGBString sat 0.1) $
        -- withSubglyphs [2] (withFillColorPixel $ toRGBString sat 0.2) $
        -- withSubglyphs [3] (withFillColorPixel $ toRGBString sat 0.3) $
        -- withSubglyphs [4] (withFillColorPixel $ toRGBString sat 0.4) $
        -- withSubglyphs [5] (withFillColorPixel $ toRGBString sat 0.5) $
        -- withSubglyphs [6] (withFillColorPixel $ toRGBString sat 0.6) $
        -- withSubglyphs [7] (withFillColorPixel $ toRGBString sat 0.7) $
        -- withSubglyphs [8] (withFillColorPixel $ toRGBString sat 0.8) $
        -- withSubglyphs [9] (withFillColorPixel $ toRGBString sat 0.9) $
        -- glyphs
    drawText = mkAnimation 2 $ \t ->
      withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth 0.4 $
        partialSvg t glyphs

data Direction = Up | Down | Sideways
type Delay = Double
type Position = Int
data SortElement = SortElement
  { sortElementDirection     :: Direction
  , sortElementStartTime     :: Double
  , sortElementDuration      :: Double
  , sortElementStartPosition :: Position
  , sortElementEndPosition   :: Position
  , sortElementTree          :: Tree }


mkJumpSorted :: [(Int, Tree)] -> [SortElement]
mkJumpSorted = fixParameters . worker Up . zip [0..]
  where
    worker _ [] = []
    worker dir ((nth, (target, elt)):rest) =
      SortElement
      { sortElementDirection = dir
      , sortElementStartTime = 0
      , sortElementDuration = 1
      , sortElementStartPosition = nth
      , sortElementEndPosition = target
      , sortElementTree = elt
      } : worker (flip dir) (yoink target rest)
    flip Up   = Down
    flip Down = Up

-- 10
-- 1/10
-- 0 -> 0.1
-- 0.05 -> 0.25
-- 0.1
fixParameters :: [SortElement] -> [SortElement]
fixParameters elts = map setDuration $ setStartTime 0 elts
  where
    setStartTime nth [] = []
    setStartTime nth (elt:elts)
      | moving elt = elt{sortElementStartTime = fromIntegral nth * duration / 2} :
                     setStartTime (nth+1) elts
      | otherwise = elt{sortElementDirection=Sideways} : setStartTime nth elts
    setDuration elt =
      elt{sortElementDuration = duration}
    duration = 2 / (moved+1)
    moved = fromIntegral $ length $ filter moving elts
    moving SortElement{..} =
      sortElementStartPosition /= sortElementEndPosition

yoink :: Int -> [(Int,a)] -> [(Int, a)]
yoink n lst =
  [ (nth, elt) | (nth, elt) <- lst, nth == n ] ++
  [ (nth, elt) | (nth, elt) <- lst, nth /= n ]

mkSorted :: [(Int, Tree)] -> [SortElement]
mkSorted lst =
    [ SortElement
      { sortElementDirection = if even nth then Up else Down
      , sortElementStartTime = fromIntegral nth * recip (len*2)
      , sortElementDuration = recip len
      , sortElementStartPosition = nth
      , sortElementEndPosition = target
      , sortElementTree = elt
      }
    | (nth, (target, elt)) <- zip [0..] lst
    ]
  where
    len = fromIntegral (length lst)

renderSortElement :: SortElement -> Double -> Tree
renderSortElement SortElement{..} t
  | t < sortElementStartTime =
    translate (fromIntegral sortElementStartPosition * digitWidth + digitWidth/2) 0 sortElementTree
  | t > sortElementStartTime + sortElementDuration =
    translate (fromIntegral sortElementEndPosition * digitWidth + digitWidth/2) 0 sortElementTree
  | otherwise =
    let pos = curveS 2 $ (t - sortElementStartTime) / sortElementDuration
        from = sortElementStartPosition
        to = sortElementEndPosition
        linear = fromIntegral from + (fromIntegral (to-from))*pos
        y = case sortElementDirection of
               Down     -> (sin (pos*pi) * digitWidth)
               Up       -> negate (sin (pos*pi) * digitWidth)
               Sideways -> 0 in
    translate (linear * digitWidth + digitWidth/2) y sortElementTree

renderSortElements :: [SortElement] -> Double -> Tree
renderSortElements elts t =
    mkGroup $
      [ renderSortElement elt t | elt <- still ] ++
      [ renderSortElement elt t | elt <- notStill ]
  where
    (notStill, still) = partition isMoving elts
    isMoving SortElement{..} =
      t > sortElementStartTime && t < sortElementStartTime + sortElementDuration
      && sortElementStartPosition /= sortElementEndPosition

-- 0 -> 0.5 Move first wrong elt up
-- 0.5 1 -> Move first wrong elt down
--          Move second wrong elt up
-- 1 1.5 -> Move second wrong elt down
jumpTransition :: [(Int, Tree)] -> Double -> Tree
jumpTransition elts s = mkGroup
    [ case () of
        () | n < selfBegin ->
              translate (fromIntegral nth * digitWidth) 0 elt
           | n > selfEnd ->
              translate (fromIntegral target * digitWidth) 0 elt
           | otherwise ->
              translate (pos * digitWidth) (sin (pos*pi) * digitWidth) elt
    | (nth, (target, elt)) <- zip [0..] elts
    , let selfBegin = fromIntegral nth * 0.5
          selfEnd   = fromIntegral nth * 0.5 + 1
          pos       = n - selfBegin
    ]
  where
    linear from to = fromIntegral from + (fromIntegral (to-from))*frac
    n = s * fromIntegral (length elts)
    frac = n `mod'` 1
    fracNext = (n+0.5) `mod'` 1

sortingTransition :: [(Int,Tree)] -> Double -> Tree
sortingTransition elts s = mkGroup
  [ case () of
      ()
        | nth == target ->
          translate (fromIntegral nth*digitWidth) 0 elt
        -- | nth == target || canMoveDirectly nth target ->
        --   translate (linear nth target*digitWidth) 0 elt
        | nth < target ->
          translate (linear nth target*digitWidth) (sin (s*pi) * digitWidth) elt
        | otherwise ->
          translate (linear nth target*digitWidth) (negate $ sin (s*pi) * digitWidth) elt
  | (nth, (target, elt)) <- zip [0..] elts
  -- , canMoveDirectly nth target
  ]
  where
    linear from to = fromIntegral from + (fromIntegral (to-from))*s
    canMoveDirectly from to
      | from == to = False
      | abs (from-to) == 1 = True
      | otherwise =
          let next = from + signum (to-from)
              (target,_) = elts !! next
          in signum (next-target) == signum (from-to) && canMoveDirectly next to

moveDigit :: Int -> Int -> Double -> Tree -> Animation
moveDigit fromX toX dir t =
  signalA (curveS 2) $ animate $ \time ->
    translate (fromToS fromPos toPos time) (sin (time*pi)*digitWidth*dir) t
  where
    fromPos = fromIntegral (fromX-5) * digitWidth + digitWidth/2
    toPos = fromIntegral (toX-5) * digitWidth + digitWidth/2

simpleSort :: [(Int, Tree)] -> Scene s ()
simpleSort lst = do
  objs <- replicateM 10 newObject
  forM_ (zip [0..] lst) $ \(i, (nth, t)) -> do
    withObject (objs!!nth) $
      fork $ playZ i $ animate $ const $
        translate (-digitWidth*5 + fromIntegral i*digitWidth + digitWidth/2) 0 t

  wait 1


  let worker _ [] = return ()
      worker dir ((i, (nth,t)):rest) | i == nth =
        worker dir rest
      worker dir ((i, (nth,t)):rest) = do
        z <- round <$> queryNow
        withObject (objs!!nth) $
          fork $ playZ (10+z) $ moveDigit i nth dir t
        wait 0.5
        worker (negate dir) $ yoink nth rest

  waitAll $ worker 1 (zip [0..] lst)

  forM_ objs dropObject

simpleSort_ :: [(Int, Tree)] -> Scene s ()
simpleSort_ lst = do
    params <- forM (zip [0..] lst) newBlock
    wait 1
    let worker _ [] = return ()
        worker dir ((i, (nth,t)):rest) | i == nth =
          worker dir rest
        worker dir ((i, (nth,t)):rest) = do
          z <- round <$> queryNow
          fork $ tweenParam (params!!i) 1 $ \t (x,y,elt) ->
            let s = curveS 2 t in
            (fromToS x (fromIntegral nth) s, y+sin (pi*s)*dir, elt)
          wait 0.5
          worker (negate dir) $ yoink nth rest

    waitAll $ worker 1 (zip [0..] lst)
  where
    newBlock (i, elt) = simpleParam render (i, 0, elt)
    render (i, y, (_, t)) =
      translate (-digitWidth*5 + i*digitWidth + digitWidth/2)
                (y*digitWidth) t

bubbleSort :: [(Int, Tree)] -> Scene s ()
bubbleSort lst = do
  objs <- replicateM (length lst) newObject
  forM_ (zip [0..] lst) $ \(i, (nth, t)) -> do
    withObject (objs!!nth) $
      fork $ playZ 0 $ animate $ const $
        translate (-digitWidth*5 + fromIntegral i*digitWidth + digitWidth/2) 0 t

  wait 1

  v <- liftST $ V.new (length lst)
  liftST $ mapM_ (\(i,e) -> V.write v i e) (zip [0..] lst)

  let worker n c | n+1 >= length lst =
        when c $ worker 0 False
      worker n c = do
        (nth1, t1) <- liftST $ V.read v n
        (nth2, t2) <- liftST $ V.read v (n+1)
        when (nth1 > nth2) $ waitAll $ do
          liftST $ V.write v n (nth2, t2)
          liftST $ V.write v (n+1) (nth1, t1)
          withObject (objs!!nth1) $
            fork $ playZ 1 $ moveDigit n (n+1) (0.5) t1
          withObject (objs!!nth2) $
            fork $ playZ 1 $ moveDigit (n+1) n (-0.5) t2
        worker (n+1) (c || nth1 > nth2)
  worker 0 False
  forM_ objs dropObject

highlightPair :: Int -> Int -> Animation
highlightPair fromX toX = animate $ \t ->
    withStrokeWidth 0.1 $
    withStrokeColor "white" $
    mkLinePath
      [ (xPos t,yPos-0.3)
      , (xPos t,yPos)
      , (xPos t+digitWidth, yPos)
      , (xPos t+digitWidth, yPos-0.3)]
  where
    yPos = digitWidth*1.1
    xPos = fromToS fromPos toPos
    fromPos = fromIntegral (fromX-5) * digitWidth + digitWidth/2
    toPos = fromIntegral (toX-5) * digitWidth + digitWidth/2
