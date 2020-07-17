#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Codec.Picture.Types
import           Control.Lens                  ()
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           Data.Ratio
import qualified Data.Text                     as T
import           Data.Tuple
import qualified Data.Vector                   as V
import           Debug.Trace
import           Linear.Matrix                 hiding (trace)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Numeric.LinearAlgebra         hiding (polar, scale, (<>))
import qualified Numeric.LinearAlgebra         as Matrix
import           Numeric.LinearAlgebra.HMatrix hiding (polar, scale, (<>))
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Math.Balloon
import           Reanimate.Math.Common
import           Reanimate.Math.Triangulate
import           Reanimate.Math.Polygon
import           Reanimate.Math.EarClip
import           Reanimate.Math.SSSP
import           Reanimate.Math.Render
import           Reanimate.Math.Visibility
import           Reanimate.Math.Compatible
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear
import           Reanimate.Morph.LeastDifference
import           Reanimate.PolyShape           (svgToPolygons)
import           Reanimate.Debug

extraPoints = 0

-- p1 = pScale 2 $ pAtCenter $ pAddPoints extraPoints (pSetOffset shape13 0)
-- p1 = pSetOffset (addPoints 2 shape13) 0
-- p2 = pScale 0.5 $ centerPolygon shape20
-- p1 = centerPolygon shape2
-- p2 = pScale 2 $ pAtCenter $ pAddPoints extraPoints (pSetOffset shape14 0 )

p1 = pAddPoints extraPoints $ (pAtCenter $ unsafeSVGToPolygon 0.01 $ scale 6 $ latex "S")
p2 = pAddPoints extraPoints $ (pAtCenter $ unsafeSVGToPolygon 0.01 $ scale 6 $ latex "C")

-- (p1s, p2s) = unzip $ triangulate p1 p2

main :: IO ()
-- main = reanimate $ playTraces $ seq (last $ triangulate [] p1 p2) ()
main = reanimate $ sceneAnimation $ do
  bg <- newSpriteSVG $ mkBackground "black"
  spriteZ bg (-1)

  let leastDiffTrig = triangulate p1 p2
  forM_ (zip [0..] leastDiffTrig) $ \(n,(l, r)) -> do
    let c = promotePixel $ turbo (n/fromIntegral (length leastDiffTrig-1))
    newSpriteSVG_ $ 
      -- translate (-3) 0 $ withFillColor "grey" $ polygonShape l
      translate (-5) 0 $ withFillColorPixel c $ mkGroup
      [ polygonShape l
      --, polygonNumDots l
      ]
    newSpriteSVG_ $ 
      -- translate (3) 0 $ withFillColor "grey" $ polygonShape r
      translate (-2) 0 $ withFillColorPixel c $ mkGroup
      [ polygonShape r
      --, polygonNumDots r
      ]
    wait (1/60)
  let (p1l, p2l) = closestLinearCorrespondence p1 p2
      compatTrig = [] -- compatiblyTriangulateP (pCopy p1l) (pCopy p2l)
  forM_ (zip [0..] compatTrig) $ \(n,(l, r)) -> do
    let c = promotePixel $ turbo (n/fromIntegral (length compatTrig-1))
    newSpriteSVG_ $ 
      -- translate (-3) 0 $ withFillColor "grey" $ polygonShape l
      translate (2) 0 $ withFillColorPixel c $ mkGroup
      [ polygonShape l
      , polygonNumDots l ]
    newSpriteSVG_ $ 
      -- translate (3) 0 $ withFillColor "grey" $ polygonShape r
      translate (5) 0 $ withFillColorPixel c $ mkGroup
      [ polygonShape r
      , polygonNumDots r ]
    wait (1/60)
  -- wait 1
  let worker l r offset len | pSize l == 3 || pSize r == 3 = return ()
      worker l r offset len = do
        fork $ play $ animate $ \t ->
          translate (-3) offset $ showP l
        fork $ play $ animate $ \t ->
          translate (3) offset $ showP r
        wait 1
        let (ll, lr) = pCutEqual l
            (rl, rr) = pCutEqual r
        fork $ play $ animate $ \t ->
          translate (-3) (offset+t*len) $ showP ll
        fork $ play $ animate $ \t ->
          translate (-3) (offset-t*len) $ showP lr
        fork $ play $ animate $ \t ->
          translate (3) (offset+t*len) $ showP rl
        fork $ play $ animate $ \t ->
          translate (3) (offset-t*len) $ showP rr

        wait 1

        fork $ worker ll rl (offset+len) (len/3)
        fork $ worker lr rr (offset-len) (len/3)

  -- fork $ worker p1 p2 0 1

  -- fork $ play $ staticFrame 1 $ 
  --   translate (-6) 0 $ mkGroup
  --   [ withFillColor "grey" $ polygonShape p1
  --   , polygonNumDots p1 ]
  -- fork $ play $ staticFrame 1 $ 
  --   translate (6) 0 $ mkGroup
  --   [ withFillColor "grey" $ polygonShape p2
  --   , polygonNumDots p2 ]
  
  -- fork $ play $ staticFrame 1 $ 
  --   translate (2) 1 $ mkGroup
  --   [ withFillColor "grey" $ polygonShape p2l
  --   , polygonNumDots p2l ]
  -- fork $ play $ staticFrame 1 $ 
  --   translate (2) 0 $ mkGroup
  --   [ withFillColor "grey" $ polygonShape p2r
  --   , polygonNumDots p2r ]
  
  -- fork $ play $ staticFrame 1 $ 
  --   translate (-2) 1 $ mkGroup
  --   [ withFillColor "grey" $ polygonShape p1l
  --   , polygonNumDots p1l ]
  -- fork $ play $ staticFrame 1 $ 
  --   translate (-2) 0 $ mkGroup
  --   [ withFillColor "grey" $ polygonShape p1r
  --   , polygonNumDots p1r ]
  return ()


showP p = mkGroup
  [ withFillColor "grey" $ polygonShape p
  , polygonNumDots p ]
