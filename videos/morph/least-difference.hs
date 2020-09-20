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
import qualified Reanimate.Math.Compatible as Compat

extraPoints = 0

-- p1 = pScale 2 $ pAtCenter $ pAddPoints extraPoints (pSetOffset shape13 0)
-- p1 = pSetOffset (addPoints 2 shape13) 0
-- p2 = pScale 0.5 $ centerPolygon shape20
-- p1 = centerPolygon shape2
-- p2 = pScale 2 $ pAtCenter $ pAddPoints extraPoints (pSetOffset shape14 0 )

-- p1 = pAddPoints extraPoints $ (pAtCenter $ unsafeSVGToPolygon 0.1 $ scale 6 $ latex "S")
-- p2 = pAddPoints extraPoints $ (pAtCenter $ unsafeSVGToPolygon 0.1 $ scale 6 $ latex "C")
p1 = pAddPoints extraPoints $ (pAtCenter $ unsafeSVGToPolygon 0.1 $ scale 6 $ latex "X")
p2 = pAddPoints extraPoints $ (pAtCenter $ unsafeSVGToPolygon 0.1 $ scale 6 $ latex "I")

p1_ = castPolygon p1
p2_ = castPolygon p2
polys = triangulate_ p1_ p2_
p1_circ = circumference (map fst polys') p1_
p2_circ = circumference (map snd polys') p2_

polys' = alignPolygons polys p1_ p2_
polys'' = compatTriagPairs polys'

-- (p1_circ, p2_circ) = closestLinearCorrespondence p1_circ' p2_circ'

-- (p1s, p2s) = unzip $ triangulate p1 p2

main :: IO ()
-- main = reanimate $ playTraces $ seq (last $ triangulate [] p1 p2) ()
main = reanimate $ sceneAnimation $ do
  bg <- newSpriteSVG $ mkBackground "black"
  spriteZ bg (-1)

  -- let leastDiffTrig = triangulate p1 p2
  let lst = polys'
  -- let lst = take 10 $ uncurry Compat.compatiblyTriangulateP (polys'!!3) -- polys''
  -- let lst = polys''
  forM_ (zip [0..] lst {-([polys''!!2]++p_snd)-}) $ \(n,(l, r)) -> do
    let c = promotePixel $ turbo (n/fromIntegral (length lst-1))
    newSpriteSVG_ $ 
      translate (-2) 0 $ withFillColorPixel c $ mkGroup
      [ polygonShape (castPolygon l)
      -- , polygonNumDots (castPolygon l)
      ]
    newSpriteSVG_ $ 
      translate 2 0 $ withFillColorPixel c $ mkGroup
      [ polygonShape (castPolygon r)
      -- , polygonNumDots (castPolygon r)
      ]
    nums <- newSpriteSVG $ mkGroup
      [ translate (-2) 0 $ polygonNumDots (castPolygon l)
      , translate 2 0 $ polygonNumDots (castPolygon r)
      ]
    wait (1/60)
    destroySprite nums
  
  -- fork $ play $ staticFrame (1/60) $ mkGroup
  --   [ translate 2 0 $ mkGroup
  --     [ withFillColor "grey" $ polygonShape p2_circ
  --     , polygonNumDots p2_circ
  --     ]
  --   , translate 6 0 $ mkGroup
  --     [ withFillColor "grey" $ polygonShape p2
  --     , polygonNumDots p2
  --     ]
  --   ]
  -- play $ staticFrame (1/60) $ mkGroup
  --   [ translate (-2) 0 $ mkGroup
  --     [ withFillColor "grey" $ polygonShape p1_circ
  --     , polygonNumDots p1_circ
  --     ]
  --   , translate (-6) 0 $ mkGroup
  --     [ withFillColor "grey" $ polygonShape p1
  --     , polygonNumDots p1
  --     ]
  --   ]

  
  return ()


showP p = mkGroup
  [ withFillColor "grey" $ polygonShape p
  , polygonNumDots p ]
