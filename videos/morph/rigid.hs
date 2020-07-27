#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Codec.Picture.Types
import           Control.Exception
import           Control.Lens                    ()
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe
import           Data.Ratio
import qualified Data.Text                       as T
import           Data.Tuple
import qualified Data.Vector                     as V
import           Debug.Trace
import           Linear.Matrix                   hiding (trace)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Numeric.LinearAlgebra           hiding (polar, scale, (<>))
import qualified Numeric.LinearAlgebra           as Matrix
import           Numeric.LinearAlgebra.HMatrix   hiding (polar, scale, (<>))
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Debug
import           Reanimate.Math.Balloon
import           Reanimate.Math.Common
import           Reanimate.Math.Compatible       (compatiblyTriangulateP)
import qualified Reanimate.Math.DCEL             as DCEL
import           Reanimate.Math.EarClip
import           Reanimate.Math.Polygon
import           Reanimate.Math.Render
import           Reanimate.Math.Smooth
import           Reanimate.Math.SSSP
import           Reanimate.Math.Visibility
import           Reanimate.Morph.Common
import           Reanimate.Morph.LeastDifference
import           Reanimate.Morph.LeastWork
import           Reanimate.Morph.Linear
import           Reanimate.Morph.Rigid
import           Reanimate.PolyShape             (svgToPolygons)
import           Text.Printf

-- p1 = centerPolygon $ shape2
--p1 = pScale 3.5 $ pAtCenter $ pAddPoints (0+2) (pSetOffset shape13 0)
-- p1 = setOffset (addPoints 2 shape13) 0
-- p2 = scalePolygon 0.5 $ centerPolygon shape20
-- p1 = centerPolygon shape2
--p2 = pScale 3.5 $ pAtCenter $ pAddPoints 0 (pSetOffset shape14 0)
-- p2 = setOffset shape14 0
p1 = pCopy $ pSetOffset (pCopy p1') 0
p2 = pCopy $ pSetOffset (pCopy p2') 0
(p1', p2') =
  -- normalizePolygons
  -- closestLinearCorrespondence
  (,)
  -- leastWork zeroStretchCosts defaultBendCosts
  -- leastWork defaultStretchCosts defaultBendCosts
  -- (pAtCenter $ unsafeSVGToPolygon 0.01 $ scale 6 $ latex "S")
  -- (pAtCenter $ unsafeSVGToPolygon 0.01 $ scale 6 $ latex "C")
  (pAtCenter $ unsafeSVGToPolygon 0.1 $ scale 6 $ latex "I")
  (pAtCenter $ unsafeSVGToPolygon 0.1 $ scale 6 $ latex "L")

p1_ = castPolygon p1
p2_ = castPolygon p2
polys = triangulate_ p1_ p2_
p1_circ = castPolygon (circumference (map fst polys') p1_)
p2_circ = castPolygon (circumference (map snd polys') p2_)

polys' = alignPolygons polys p1_ p2_
(p1s, p2s) = unzip $ compatTriagPairs [ (castPolygon a, castPolygon b) | (a,b) <- polys' ]

-- (p1s,p2s) = unzip (compatiblyTriangulateP p1 p2)

m2 = DCEL.buildMesh $ DCEL.polygonsMesh
        (map (fmap realToFrac) $ V.toList $ polygonPoints p2_circ)
        (map (map (fmap realToFrac) . V.toList . polygonPoints) p2s)

m1 = DCEL.buildMesh $ DCEL.polygonsMesh
        (map (fmap realToFrac) $ V.toList $ polygonPoints p1_circ)
        (map (map (fmap realToFrac) . V.toList . polygonPoints) p1s)

pipeline1 = last . take 20 . iterate
  ( uncurry DCEL.delaunayFlip .
    uncurry DCEL.splitInternalEdges .
   (\(a,b) -> (DCEL.meshSmoothPosition a, DCEL.meshSmoothPosition b)))
(m1final, m2final) = last $ take 30 $ iterate
  (pipeline1 .
    uncurry DCEL.splitLongestEdge .
    pipeline1
    ) (m1,m2)

-- (m1good, m2good) = last $ take 9 $ iterate
--   (pipeline1 .
--     -- uncurry DCEL.splitLongestEdge .
--     pipeline1
--     ) (m1,m2)

main :: IO ()
-- main = do
--   evaluate (last $ take 900 $ compatiblyTriangulateP p1 p2)
--   return ()
-- main = reanimate $ playTraces $ length $ take 29 p1s
-- main = reanimate $ playThenReverseA $ pauseAround 0.5 0.5 $ sceneAnimation $ do

main = reanimate $ sceneAnimation $ do
  newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
  -- play
  --   $ playTraces
  --   $ traceA (mapA (withFillOpacity 0 . withStrokeColor "white") drawCircle)
  --   $ traceA (mapA (withFillOpacity 0 . withStrokeColor "white") drawBox)
  --   $ traceSVG ((withFillOpacity 0 . withStrokeColor "white") $ mkCircle 2)
  --   $ traceSVG ((withFillOpacity 0 . withStrokeColor "white") $ mkRect 2 2)
  --   $ 20
  -- play $ playTraces $ last $ take 35 $ compatiblyTriangulateP p1 p2
  -- fork $ newSpriteA $ drawCompatible p1 p2
  -- newSpriteSVG_ $ translate (5) 0 $ lowerTransformations $ scale 2 $
  --   -- DCEL.renderMesh 0.02 m1final
  --   DCEL.renderMeshColored m1final
    -- renderMeshPair optMesh
  -- newSpriteSVG_ $ translate (-5) 0 $ lowerTransformations $ scale 2 $
  --   DCEL.renderMesh 0.02 m1good

  -- newSpriteSVG_ $
  --   translate (-3) 0 $ scale 1 $ mkGroup
  --   [ mkGroup []
  --   , withFillColor "grey" $ polygonShape p1
  --   , withFillColor "grey" $ polygonNumDots p1
  --   ]
  -- newSpriteSVG_ $
  --   translate (3) 0 $ scale 1 $ mkGroup
  --   [ mkGroup []
  --   , withFillColor "grey" $ polygonShape p2
  --   , withFillColor "grey" $ polygonNumDots p2
  --   ]
  -- wait 1
  -- fork $ forM_ p1s $ \p1Piece -> do
  --   newSpriteSVG_ $
  --     translate (-3) 0 $ scale 1 $ mkGroup
  --     [ mkGroup []
  --     , withFillColor "white" $ polygonShape p1Piece
  --     -- , withFillColor "grey" $ polygonNumDots p1Piece
  --     ]
  --   wait (1/60)
  --   -- wait 1
  -- fork $ forM_ p2s $ \p1Piece -> do
  --   newSpriteSVG_ $
  --     translate (3) 0 $ scale 1 $ mkGroup
  --     [ mkGroup []
  --     , withFillColor "white" $ polygonShape p1Piece
  --     -- , withFillColor "grey" $ polygonNumDots p1Piece
  --     ]
  --   wait (1/60)
  --   -- wait 1

  -- fork $ play $ mkAnimation 3 $ \t ->
  --   let points = interpolate prep t
  --   in  translate (-3) 0 $ scale 1 $ mkGroup
  --   [ mkGroup []
  --   , drawTrigs points (meshTriangles myMesh)
  --   -- , DCEL.renderMeshColored m1
  --   ]
  fork $ play $ mkAnimation 3 $ \t ->
    let points = interpolate optPrep t
    in  translate (3) 0 $ lowerTransformations $ scale 1 $ mkGroup
      [ mkGroup []
      , drawTrigs points (meshTriangles optMesh)
      -- , DCEL.renderMeshColored m1final
      -- , withGroupOpacity 0.5 $ DCEL.renderMesh 0.02 m1final
      -- , withGroupOpacity 0.5 $ withFillOpacity 0 $ renderMeshPair optMesh
      -- , let V2 x y = V.head points in
      --   translate x y $ withFillColor "green" $
      --   mkCircle 0.03
      ]
        -- , scale 2 $ polygonNumDots p1
  -- play $ pauseAtEnd 1 $ mkAnimation 3 $ \t ->
  --   let points = interpolate bestPrep t
  --   in  translate (3) 0 $ mkGroup [drawTrigs points (meshTriangles bestMesh)
  --       -- , scale 2 $ polygonNumDots p2
  --                                                                           ]
  -- play $ pauseAtEnd 1 $ mkAnimation 3 $ \t ->
  --   let points = interpolate prepRev t in
  --   mkGroup
  --   [ drawTrigs points (meshTriangles myMeshRev)
  --   -- , polygonNumDots shape2
  --   ]
  -- forM_ (myMesh : smoothMesh myMesh) $ \newMesh -> do
  --   let (minAngA, minAngB) = meshMinAngle newMesh
  --   txt <- newSpriteSVG $ withFillColor "white" $
  --     mkGroup
  --     [translate (5) 2 $  center $
  --       latex $ T.pack $ printf "Min: %.1f" (minAngA/pi*180)
  --     ,translate (5) 1 $  center $
  --       latex $ T.pack $ printf "Min: %.1f" (minAngB/pi*180) ]
  --   s <- newSpriteSVG $ renderAMesh newMesh
  --   wait (recip 60)
  --   destroySprite s
  --   destroySprite txt
  -- newSpriteSVG $ translate (-3) 0 $ scale 2 $ renderMesh p1 (map fst trigs)
  -- newSpriteSVG $ translate 3 0 $ scale 2 $ renderMesh p2 (map snd trigs)
  -- wait 1
 where
  prep      = prepare myMesh
  optPrep   = prepare optMesh
  -- bestPrep  = prepare bestMesh
  myMesh    = mkMesh p1 p2
  optMesh   = toRigidMesh  m1final m2final
  -- optMesh   = DCEL.toRigidMesh  m1good m2good
  -- bestMesh  = last $ smoothMesh myMesh
  prepRev   = prepare myMeshRev
  myMeshRev = reverseMesh myMesh



reverseMesh :: Mesh -> Mesh
reverseMesh mesh =
  mesh { meshPointsA = meshPointsB mesh, meshPointsB = meshPointsA mesh }

mkMesh :: Polygon -> Polygon -> Mesh
mkMesh a b = Mesh
  { meshPointsA   = V.map (fmap realToFrac) pointsA
  , meshPointsB   = V.map (fmap realToFrac) pointsB
  , meshOutline = V.map (fromJust . flip V.elemIndex pointsA) (polygonPoints a)
  , meshTriangles = relTrigs
  }
 where
  pointsA =
    V.fromList $ nub $ V.toList $ V.concat [ polygonPoints a | (a, b) <- trigs ]
  pointsB =
    V.fromList $ nub $ V.toList $ V.concat [ polygonPoints b | (a, b) <- trigs ]
  trigs = compatiblyTriangulateP a b
  mkRel arr p =
    ( (fromJust $ V.elemIndex (pAccess p 0) arr)
    , (fromJust $ V.elemIndex (pAccess p 1) arr)
    , (fromJust $ V.elemIndex (pAccess p 2) arr)
    )
  relTrigs = V.fromList [ (mkRel pointsA a) | (a, b) <- trigs ]
    -- data Mesh = Mesh (Vector P) (Vector (RelTrig, RelTrig))

testMesh2 :: Mesh
testMesh2 = mkMesh (pTranslate (V2 0 0) shape2) shape20

testPrep = prepare testMesh2

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
    -- , a == 0
  -- , mkGroup
  --   [ withFillColor "red" $ mkGroup
  --     [ drawPoint (points V.! a), drawPoint (points V.! b), drawPoint (points V.! c)]
  --   | (a,b,c) <- V.toList trigs
  --   -- , a == 0
  --   ]
  ]

drawTrigsLines :: V.Vector (V2 Double) -> V.Vector RelTrig -> SVG
drawTrigsLines points trigs = withFillOpacity 0 $ withStrokeColor "black" $ mkGroup
  [ mkLinePathClosed
    [ (aPx, aPy)
    , (bPx, bPy)
    , (cPx, cPy)]
  | (a,b,c) <- V.toList trigs
  -- , a==2 || b == 2 || c == 2
  , let V2 aPx aPy = points V.! a
        V2 bPx bPy = points V.! b
        V2 cPx cPy = points V.! c
  ]

drawPoint :: V2 Double -> SVG
drawPoint (V2 x y) = translate x y $ mkCircle 0.1

drawPolygon :: [V2 Double] -> SVG
drawPolygon lst = mkLinePathClosed [ (x, y) | V2 x y <- lst ]

drawCompatible :: Polygon -> Polygon -> Animation
drawCompatible a b = sceneAnimation $ do
  let left  = -6
      right = 4
  newSpriteSVG $ translate left 0 $ mkGroup
    [ withFillColor "grey" $ polygonShape a
    , withFillColor "grey" $ polygonNumDots a
    ]
  newSpriteSVG $ translate right 0 $ mkGroup
    [ withFillColor "grey" $ polygonShape b
    , withFillColor "grey" $ polygonNumDots b
    ]
  return ()
  -- let compat = compatiblyTriangulateP a b
  -- forM_ compat $ \(l, r) -> do
  --   fork $ play $ staticFrame 1 $
  --     translate left 0 $ withStrokeColor "white" $ withStrokeWidth (defaultStrokeWidth*0.2) $
  --     withFillOpacity 0 $ polygonShape l
  --   fork $ play $ staticFrame 1 $
  --     translate right 0 $ withStrokeColor "white" $ withStrokeWidth (defaultStrokeWidth*0.2) $
  --     withFillOpacity 0 $ polygonShape r
