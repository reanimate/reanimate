#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Chiphunk.Low
import           Control.Lens
import           Control.Monad
import           Data.List
import Data.Ord
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text, pack)
import           Debug.Trace
import           Geom2D.CubicBezier  (ClosedPath (..), CubicBezier (..),
                                      PathJoin (..), bezierIntersection,
                                      bezierLineIntersections, closedPathCurves,
                                      evalBezier, rotateScaleVec, transform, Point(..))
import qualified Geom2D.CubicBezier  as G
import           Graphics.SvgTree    hiding (Text,Point)
import           Linear.V2
import           Numeric
import           Reanimate.Chiphunk
import           Reanimate.Constants
import           Reanimate.PolyShape
import           Reanimate.Driver    (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Animation
import           Reanimate.Signal
import           Reanimate.Svg
import           System.IO.Unsafe

test :: Animation
test = unsafePerformIO $ do
  bodyStore <- newBodyStore
  let gravity = Vect 0 (-1)

  -- Create an empty space.
  space <- spaceNew
  spaceCollisionSlop space $= (screenWidth/2560)
  spaceGravity space $= gravity

  -- Add a static line segment shape for the ground.
  -- We'll make it slightly tilted so the ball will roll off.
  -- We attach it to a static body to tell Chipmunk it shouldn't be movable.
  static <- get $ spaceStaticBody space
  ground <- segmentShapeNew static
    (Vect (-screenWidth/2) 0)
    (Vect (screenWidth/2) (-screenHeight/2)) 0
  -- ground <- polyShapeNewRaw static
  --   [ Vect (-screenWidth/2) (screenHeight/2)
  --   , Vect (screenWidth/2) (-screenHeight/2)
  --   , Vect (-screenWidth/2) (-screenHeight/2)
  --   , Vect (-screenWidth/2) (screenHeight/2) ] 0
  shapeFriction ground $= 1
  spaceAddShape space ground



  -- Now let's make a ball that falls onto the line and rolls off.
  -- First we need to make a cpBody to hold the physical properties of the object.
  -- These include the mass, position, velocity, angle, etc. of the object.
  -- Then we attach collision shapes to the Body to give it a size and shape.

  let toVect (V2 x y) = Vect x y

  let svg = center $ scale 2 $ latex "\\LaTeX"
      poly = svgToPolyShapes svg
      vectGroup = plDecompose poly
            --mkCircle (Num radius)
      -- vects = svgToVects svg
      -- vects' = fst $ convexHull vects 0
      svg' =
        withFillOpacity 0 $ withStrokeColor "white" $
        withStrokeWidth 0.01 $
        renderPolyShapes (map plFromPolygon vectGroup)
  --
  -- ballBody <- polyShapesToBody space poly
  -- bodyPosition ballBody $= Vect (-screenWidth/4) (screenHeight/2)
  --
  -- addToBodyStore bodyStore ballBody $
  --   withFillColor "white" $
  --   mkGroup
  --     [ svg' ]

  let splitPolys = vectGroup
  forM_ vectGroup $ \polygon -> do
    bd <- polygonsToBody space [map toVect polygon]
    bodyPosition bd $= Vect 0 (screenHeight/3)
    addToBodyStore bodyStore bd $
      renderPolyShape $ plFromPolygon polygon
      -- withFillColor "white" $
      -- mkGroup
      --   [ --withStrokeWidth (Num 0.005) $
      --     -- withStrokeWidth (Num 0.00) $
      --     -- withStrokeColor "white" $
      --     -- withFillOpacity 1 $
      --     renderPolyShape $ plFromPolygon polygon
      --   ]

  ani <- simulate space bodyStore 60 60 10
  spaceFreeRecursive space
  return ani

-- data LineCommand
--   = LineMove RPoint
--   -- | LineDraw RPoint
--   | LineBezier [RPoint]
--   | LineEnd

vectsToSVG :: [Vect] -> Tree
vectsToSVG (Vect x y:rest) =
  mkPath $
    MoveTo OriginAbsolute [V2 x y] :
    [ LineTo OriginAbsolute [V2 a b] | Vect a b <- rest ] ++
    [ EndPath ]
  where
    mkPath cmds = PathTree $ defaultSvg & pathDefinition .~ cmds

polygonsToSVG :: [[Vect]] -> Tree
polygonsToSVG = merge . mkGroup . map vectsToSVG
  where
    merge svg = PathTree $ defaultSvg & pathDefinition .~ extractPath svg

svgToVects :: Tree -> [Vect]
svgToVects svg = map worker (lineToPoints 200 cmds)
  where
    worker (V2 x y) = Vect x y
    cmds = toLineCommands $ wibble $ extractPath svg
    wibble xs = takeWhile (/=EndPath) xs ++ [EndPath]

chunkPolyshapes :: Tree -> Tree
chunkPolyshapes t =
  withStrokeColor "white" $
  withStrokeWidth 0.01 $
  withFillColor "white" $ t

plArea :: PolyShape -> Double
plArea pl = areaForPoly (map toVect $ plPolygonify polyShapeTolerance pl) 0
  where
    toVect (Point x y) = Vect x y

reorient = id -- scale 4 . translate 0 (-0.9)

main :: IO ()
main = reanimate $ bg `sim` mapA reorient (line `sim` mapA chunkPolyshapes test)
  where
    bg = animate $ const $ mkBackground "black"
    line = animate $ const $ withStrokeColor "white" $
      withStrokeWidth 0.01 $
      mkLine (-screenWidth/2, 0)
             (screenWidth/2, -screenHeight/2)
