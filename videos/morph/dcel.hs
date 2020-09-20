#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Function
import           Data.List
import           Text.Printf
import           Data.List.NonEmpty                       ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           Data.Ratio
import qualified Data.Text                     as T
import           Data.Tuple
import qualified Data.Vector                   as V
import           Debug.Trace
import           Linear.Matrix                     hiding ( trace )
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Math.Balloon
import           Reanimate.Math.Common
import           Reanimate.Math.Polygon
import           Reanimate.Math.EarClip
import           Reanimate.Math.Smooth
import           Reanimate.Math.Render
import           Reanimate.Math.Compatible
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear
import           Reanimate.Morph.Rigid
import           Reanimate.Debug
import Reanimate.Math.DCEL as DCEL

-- p1 = centerPolygon $ shape2
--p1 = pScale 3.5 $ pAtCenter $ pAddPoints (0+2) (pSetOffset shape13 0)
-- p1 = setOffset (addPoints 2 shape13) 0
-- p2 = scalePolygon 0.5 $ centerPolygon shape20
-- p1 = centerPolygon shape2
--p2 = pScale 3.5 $ pAtCenter $ pAddPoints 0 (pSetOffset shape14 0)
-- p2 = setOffset shape14 0
p1 = pCopy p1'
p2 = pCopy p2'
(p1', p2') =
  -- normalizePolygons
             closestLinearCorrespondence
  -- leastWork defaultStretchCosts defaultBendCosts
  (pAtCenter $ unsafeSVGToPolygon 0.5 $ scale 6 $ latex "X")
  (pAtCenter $ unsafeSVGToPolygon 0.5 $ scale 6 $ latex "S")

(p1s,p2s) = unzip (compatiblyTriangulateP p1 p2)

m2 = buildMesh $ polygonsMesh 
        (map (fmap realToFrac) $ V.toList $ polygonPoints p2)
        (map (map (fmap realToFrac) . V.toList . polygonPoints) p2s)

m1 = buildMesh $ polygonsMesh 
        (map (fmap realToFrac) $ V.toList $ polygonPoints p1)
        (map (map (fmap realToFrac) . V.toList . polygonPoints) p1s)

doSmooth (a,b) = (meshSmoothPosition a, meshSmoothPosition b)
step0 = (m1,m2)
step1 = uncurry delaunayFlip step0
step2 = doSmooth step1

main :: IO ()
main = reanimate $ sceneAnimation $ do
  -- newSpriteSVG_ $ mkBackground "black"
  newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
  -- let m = buildMesh $ polygonMesh $ map (fmap realToFrac) $ V.toList $ polygonPoints p2
  
  -- newSpriteSVG_ $ withStrokeWidth (defaultStrokeWidth*0.5) $ scale 2 $
  --   DCEL.renderMesh m
  s <- newVar 1
  mVar <- newVar (m1, m2)
  let V2 centerX centerY = V2 0 0 -- V2 (-0.2) (-2)-- realToFrac <$> pAccess p2 33
  adjustZ 2 $ newSprite_ $ do
    ~(m1, m2) <- unVar mVar
    pure $ mkGroup
      [ translate 5 3.5 $ scale 0.5 $ renderMeshStats m1
      , translate 5 0.5 $ scale 0.5 $ renderMeshStats m2 ]
  newSprite_ $ do
    ~(m1,m2) <- unVar mVar
    sc <- unVar s
    pure $ mkGroup
      [translate (-4) 0 $ withStrokeWidth (defaultStrokeWidth*1) $ lowerTransformations $ scale sc $
        translate (negate centerX) (negate centerY) $ mkGroup
        [ mkGroup []
        , DCEL.renderMeshColored m1
        , DCEL.renderMesh (0.05/sc) m1
        -- , renderMeshEdges m1
        , withFillColor "grey" $ DCEL.renderMeshSimple (0.15/sc) m1
        ]
      ,translate 2 0 $ withStrokeWidth (defaultStrokeWidth*1) $ lowerTransformations $ scale sc $
        translate (negate centerX) (negate centerY) $ mkGroup
        [ mkGroup []
        , DCEL.renderMeshColored m2
        , DCEL.renderMesh (0.05/sc) m2
        -- , renderMeshEdges m2
        -- , DCEL.renderMeshSimple (0.10/sc) m2
        , withFillColor "grey" $ DCEL.renderMeshSimple (0.15/sc) m2
        ]]
  writeVar s 2
  -- wait (1/60)
  let pipeline1 = last . take 20 . iterate (
          uncurry delaunayFlip .
          -- uncurry splitInternalEdges . 
          doSmooth .
          id
          )
      stages = take 30 $ iterate (
         pipeline1 .
        --  uncurry splitInternalEdges . 
        --  pipeline1 .
        --  uncurry splitLongestEdge .
         --pipeline1
         id
         ) (m1,m2)
  let pipeline = do
        modifyVar mVar (uncurry delaunayFlip)
        modifyVar mVar (uncurry splitInternalEdges)
        modifyVar mVar $ \(a,b) -> (meshSmoothPosition a, meshSmoothPosition b)
  forM_ stages $ \newM -> do
    writeVar mVar newM
    wait (1/60)
  -- replicateM_ 30 $ do
  --   pipeline
  --   wait (1/60)

  -- replicateM_ 200 $ do
  --   modifyVar mVar (uncurry splitOuterEdges)
  --   modifyVar mVar (uncurry splitLongestEdge)
  --   wait (1/60)

  --   replicateM_ 5 $ do
  --     pipeline
  --     wait (1/60)

  -- let m' = execState (flipEdge 39 17) m
  -- writeVar mVar m'
  -- wait 1
  -- tweenVar s 5 $ \v -> fromToS v 10
    
  -- newSpriteSVG_ $ scale 3 $ 
  --   mkGroup $ map polygonShape p2s
  -- newSpriteSVG_ $ scale 0.5 $ withFillColor "red" $
  --   mkGroup $ map (polygonDots . pScale 6) p2s
  -- newSpriteSVG_ $ withFillColor "red" $ scale 3 $ polygonNumDots p2
  -- wait 1
   

drawTrigs :: V.Vector (V2 Double) -> V.Vector RelTrig -> SVG
drawTrigs points trigs = mkGroup
  [ mkGroup
      [ withFillOpacity 1
        $ withStrokeWidth (defaultStrokeWidth * 0)
        $ withStrokeColor "grey"
        $ withFillColor "black"
        $ drawPolygon
        $ map (points V.!) [a, b, c]
      | (a, b, c) <- V.toList trigs
      ]
  ]

drawPoint :: V2 Double -> SVG
drawPoint (V2 x y) = translate x y $ mkCircle 0.1

drawPolygon :: [V2 Double] -> SVG
drawPolygon lst = mkLinePathClosed [ (x, y) | V2 x y <- lst ]
