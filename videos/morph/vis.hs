#!/usr/bin/env stack
-- stack --resolver lts-15.04 runghc --package reanimate
module Main where

import Data.Ratio
import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.NonEmpty                                         (NonEmpty)
import qualified Data.List.NonEmpty                                         as NE
import           Data.Maybe
import qualified Data.Text                                                  as T
import           Data.Tuple
import qualified Data.Vector                                                as V
import           Debug.Trace
import           Linear.Matrix                                              hiding
                                                                             (trace)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Numeric.LinearAlgebra                                      hiding
                                                                             (polar,
                                                                             scale,
                                                                             (<>))
import qualified Numeric.LinearAlgebra                                      as Matrix
import           Numeric.LinearAlgebra.HMatrix                              hiding
                                                                             (polar,
                                                                             scale,
                                                                             (<>))
import           Reanimate
import           Reanimate.Math.Common
import           Reanimate.Math.Visibility
import           Reanimate.Math.SSSP
import           Reanimate.Math.EarClip

main :: IO ()
main = reanimate $ sceneAnimation $ do
  _ <- newSpriteSVG $ mkBackground "black"
  -- play $ drawVisibleFrom triangle
  -- play $ drawVisibleFrom shape1
  -- play $ drawVisibleFrom shape2
  -- play $ drawVisibleFrom shape3
  -- play $ drawVisibleFrom shape4
  -- play $ drawVisibleFrom shape5
  -- play $ drawVisibleFrom shape6
  -- play $ drawSSSP triangle naive
  -- play $ drawSSSP shape5 naive
  -- play $ drawSSSP shape2 naive
  -- play $ drawSSSP shape3 naive
  -- play $ drawSSSP shape4 naive
  -- play $ drawSSSP shape5 naive
  -- play $ drawSSSP shape6 naive
  -- play $ drawSSSP shape1 (\p -> sssp p (dual (earClip p)))
  -- play $ drawTriangulation shape5 earClip'
  -- play $ mkAnimation 1 $ \t ->
  --   -- let p = scalePolygon 5 $ shape11
  --   -- let p = shape7
  --   let p = shape4
  --   in withStrokeWidth (defaultStrokeWidth*0.6) $ renderTriangulation p (earClip p)
  play $ drawVisibility shape2
  -- play $ drawTriangulation shape1 earClip'
  -- play $ staticFrame 1 $ renderTriangulation shape2 earClip
  -- play $ staticFrame 1 $ renderTriangulation shape3 earClip
  -- play $ staticFrame 1 $ renderTriangulation shape4 earClip
  -- play $ staticFrame 1 $ renderTriangulation shape5 earClip
  -- play $ staticFrame 1 $ renderTriangulation shape6 earClip
  return ()

drawVisibility :: Polygon -> Animation
drawVisibility p' = addStatic (mkBackground "black") $ mkAnimation 5 $ \t ->
  let p = cyclePolygon p' (t::Double) in
  centerUsing (polygonShape p) $
  mkGroup
  [ withFillColor "grey" $ polygonShape p
  , withFillColor "grey" $ polygonDots p
  , withFillColor "white" $ mkLinePathClosed
    [ (x,y) | V2 x y <- visibility (map (fmap realToFrac) $ V.toList p) ]
  , let V2 x y = fmap realToFrac $ pAccess p 0 in
    translate x y $ withFillColor "red" $ mkCircle 0.1
  -- , withFillColor "blue" $ latex $ T.pack $ show (t)
  ]

polygonShape :: Polygon -> SVG
polygonShape p = mkLinePathClosed
  [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList p  ++ [pAccess p 0] ]

polygonDots :: Polygon -> SVG
polygonDots p = mkGroup
  [ translate x y $ mkCircle 0.1 | V2 x y <- V.toList $ V.map (fmap realToFrac) p ]

polygonNumDots :: Polygon -> SVG
polygonNumDots p = mkGroup $ reverse
    [ mkGroup
      [ colored n $
        translate x y $ mkCircle 0.1
      , withFillColor "black" $
        translate x y $ ppNum n ]
    | (n, V2 x y) <- zip [0..] (V.toList $ V.map (fmap realToFrac) p) ]
  where
    circR = 0.1
    colored n =
      let c = promotePixel $ turbo (fromIntegral (n+2) / fromIntegral (length p-1+2))
      in withStrokeColorPixel c . withFillColorPixel c
    ppNum n = scaleToHeight (circR*1.5) $ center $ latex $ T.pack $ "\\texttt{" ++ show n ++ "}"

drawSSSP :: Polygon -> (Polygon -> SSSP) -> Animation
drawSSSP p gen = mkAnimation 5 $ \t -> centerUsing outline $ mkGroup
  [ outline
  , renderSSSP (cyclePolygon p t) (gen (cyclePolygon p t))
  -- , let V2 x y = fmap realToFrac $ pAccess (cyclePolygon p t) 0 in
  --   translate x y $ withFillColor "red" $ mkCircle 0.1
  , withFillColor "grey" $ polygonNumDots $ cyclePolygon p t
  ]
  where
    outline =
      withFillColor "grey" $ mkLinePathClosed
        [ (x,y) | V2 x y <- map (fmap realToFrac) (V.toList p  ++ [pAccess p 0]) ]

renderSSSP :: Polygon -> SSSP -> SVG
renderSSSP p s = withFillOpacity 0 $ withStrokeColor "white" $ mkGroup
  [ mkLinePath (lineFrom i)
  | i <- [0 .. length s-1] ]
  where
    lineFrom 0 =
      let V2 ax ay = fmap realToFrac $ pAccess p 0
      in [(ax,ay)]
    lineFrom i =
      let V2 ax ay = fmap realToFrac $ pAccess p i
      in (ax,ay) : lineFrom (s V.! i)

drawVisibleFrom :: Polygon -> Animation
drawVisibleFrom p = mkAnimation 5 $ \t -> centerUsing (polygonShape p) $ mkGroup
  [ withFillColor "grey" $ polygonShape p
  , withFillColor "grey" $ polygonDots p
  , renderVisibleFrom (cyclePolygon p t)
  , let V2 x y = fmap realToFrac $ pAccess (cyclePolygon p t) 0 in
    translate x y $ withFillColor "red" $ mkCircle 0.1
  ]


renderVisibleFrom :: Polygon -> SVG
renderVisibleFrom p = withStrokeColor "white" $ withFillColor "white" $ mkGroup
  [ mkGroup
    [ mkLine (ax,ay) (bx,by)
    , translate bx by $ mkCircle 0.1 ]
  | i <- visibilityArray p V.! 0
  , let V2 ax ay = fmap realToFrac $ pAccess p 0
        V2 bx by = fmap realToFrac $ pAccess p i ]

drawTriangulation :: Polygon -> (Polygon -> [Triangulation]) -> Animation
drawTriangulation p gen = sceneAnimation $ do
  forM_ (gen p) $ \t -> play $ staticFrame 1 $ renderTriangulation p t

renderTriangulation :: Polygon -> Triangulation -> SVG
renderTriangulation p t = center $ mkGroup
  [ withFillColor "grey" $ polygonShape p
  , withStrokeColor "white" $ mkGroup $ concat
    [ [ mkLine (ax,ay) (bx,by) ]
    | i <- [0..length p-1]
    , y <- t V.! i
    , let V2 ax ay = fmap realToFrac $ pAccess p i
          V2 bx by = fmap realToFrac $ pAccess p y
    ]
  , withFillColor "grey" $ polygonNumDots p
  ]
