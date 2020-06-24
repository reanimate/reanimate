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
import           Reanimate.PolyShape           (svgToPolygons)

p1 = pScale 2 $ pAtCenter $ pAddPoints (0+2) (pSetOffset shape13 0)
-- p1 = pSetOffset (addPoints 2 shape13) 0
-- p2 = pScale 0.5 $ centerPolygon shape20
-- p1 = centerPolygon shape2
p2 = pScale 2 $ pAtCenter $ pAddPoints 0 (pSetOffset shape14 0)

main :: IO ()
main = reanimate $ sceneAnimation $ do
  bg <- newSpriteSVG $ mkBackground "black"
  spriteZ bg (-1)
  -- play $ drawVisibleFrom triangle
  -- play $ drawVisibleFrom shape1
  -- play $ drawVisibleFrom shape2
  -- play $ drawVisibleFrom shape3
  -- play $ drawVisibleFrom shape4
  -- play $ drawVisibleFrom shape5
  -- play $ drawVisibleFrom shape6
  -- play $ drawOverlap shape20
  -- play $ drawSSSP triangle naive
  -- play $ drawSSSPFast shape5
  -- fork $ play $ mapA (translate (-3) 0) $ drawSSSPVisibilityFast shape2
  -- fork $ play $ mapA (translate (3) 0) $ drawSSSPVisibilityFast shape7
  -- play $ drawOverlap $ fst $ split1Link (fst (split1Link shape7 3 7)) 0 2
  -- fork $ play $ mapA (translate (-4) 0) $ drawSSSPVisibilityFast $ pSetOffset (addPoints 0 shape14) 0
  -- fork $ play $ mapA (translate (4) 0) $ drawSSSPVisibilityFast $ pSetOffset (addPoints 2 shape13) 0
  fork $ play $ staticFrame 1 $ 
    translate (4) 0 $ mkGroup
    [ withFillColor "grey" $ polygonShape p1
    , polygonNumDots p1 ]
  fork $ play $ staticFrame 1 $
    let -- pOrigin = (addPoints 10 $ pSetOffset shape14 0)
        pOrigin = pScale 2 $ pAtCenter $ pAddPoints (0+2) (pSetOffset shape13 0)
        --pOrigin = (addPoints 2 shape13)
        p1 = pScale 1 $ pSetOffset pOrigin 0
        p2 = snd $ split1Link p1 1 11 0
        p3 = fst $ split1Link p2 0 2 1
        -- p4 = fst $ split2Link p3 2 5
        p4 = snd $ split1Link p3 0 9 1
        p5 = fst $ split1Link p4 1 3 0
        p6 = fst $ split2Link p5 0 2
        p7 = fst $ split1Link p6 1 3 0
        p8 = fst $ split2Link p7 0 2
        p9 = fst $ split1Link p7 1 3 0
        p = p4
    in mkGroup
    [ withFillColor "grey" $ polygonShape p
    , polygonNumDots p ]
  -- newSpriteSVG $
  --   let p1 = pSetOffset shape14 0
  --       V2 x y = realToFrac <$> steiner2Link p1 2 6
  --   in translate (-4) 0 $
  --     translate x y $ withFillColor "red" $
  --     mkCircle 0.1
  -- newSpriteSVG $
  --   let p1 = pSetOffset shape14 0
  --   in translate (-4) 0 $
  --     mkGroup
  --     [ mkGroup
  --       [ withStrokeColor "purple" $
  --         mkLinePath [(x1,y1),(x2,y2)]
  --       , translate x2 y2 $ withFillColor "red" $ mkCircle 0.1 ]
  --     | (eA, eB) <- take 1 $ steiner2Edges p1 4 11
  --     , let V2 x1 y1 = realToFrac <$> eA
  --           V2 x2 y2 = realToFrac <$> eB
  --     ]
  -- play $ staticFrame 1 $
  --   let p1 = pSetOffset shape14 0
  --   in mkGroup
  --   [ withFillColor "grey" $ polygonShape p1
  --   , polygonNumDots p1
  --   , drawWindowOverlap p1 1 6
  --   -- , withStrokeColor "red" $ drawWindow (pSetOffset p1 2)
  --   -- , withStrokeColor "blue" $ drawWindow (pSetOffset p1 6)
  --   ]
  -- play $ drawCompatible (pSetOffset (addPoints 2 shape13) 0) (pSetOffset shape14 0)

  -- play $ drawSSSP shape2 naive
  -- play $ drawSSSP shape3 naive
  -- play $ drawSSSP shape4 naive
  -- play $ drawSSSP shape5 naive
  -- play $ drawSSSP (pScale 0.5 $ winding 10) (\p -> sssp p (dual (earClip p)))
  -- play $ drawSSSP shape1 (\p -> sssp p (dual (earClip p)))
  -- play $ drawTriangulation (pCycle shape5 0.2910962834555265) earClip'
  -- play $ drawTriangulation shape5 earClip'
  -- play $ mkAnimation 1 $ \t ->
  --   let p = shape4
  --   in polygonNumDots (pCycle p t)
  -- play $ setDuration 20 $ drawSSSPVisibility $ pScale 1 $ shape7
  -- let shapeI = head $ svgToPolygons 0.1 $ scale 8 $ center $ latex "I"
  -- play $ animate $ \_ -> withFillColor "grey" $ polygonNumDots shapeI
  -- play $ drawTriangulation (pScale 0.5 $ winding 10) earClip'
  -- play $ staticFrame 1 $
  --   mkGroup
  --   [ withFillColor "grey" $ polygonShape shape13
  --   , withFillColor "grey" $ polygonDots shape13 ]
  -- let p = balloonP origin
  --     origin = centerPolygon $ pScale 1 $ shiftLongestDiameter shapeI
  --     mkB = balloon (scale 8 $ center $ latex "C")
  --     inf = unsafeSVGToPolygon 0.1 $ (scale 8 $ center $ latex "$\\infty$")
  --     cShape = shiftLongestDiameter $ unsafeSVGToPolygon 0.01 $ (scale 8 $ center $ latex "C")
  -- _ <- newSpriteSVG $
  --   translate (0) (3) $ withFillColor "white" $
  --   center $ latex $ T.pack $ show (isSimple inf)
  -- play $ pauseAtEnd 1 $ mkAnimation 3 $ \t ->
  --   let inflated = p t in
  --   translate (0) (0) $ withFillColor "white" $ withStrokeColor "red" $
  --   withStrokeWidth (defaultStrokeWidth*0) $
    -- polygonShape inf
    -- renderTriangulation inf (earClip inf)
    -- mkB t
    -- scale 3 $ -- translate (-2.2) (-1) $
    -- mkGroup
    -- [ mkGroup []
    -- , withStrokeWidth (defaultStrokeWidth*0.05) $
    --   renderDual (pRing shape22) (dual (polygonOffset shape22) $ polygonTriangulation shape22)
    -- , polygonNumDots shape22
    --   -- renderTriangulation cShape (polygonTriangulation cShape)
    -- ]
    -- mkGroup
    -- [ -- translate (-2) 0 $ withFillColor "white" $ polygonShape $ balloonP (min 1 $ t+0.1) origin
    --   if False then mkGroup [] else translate (0) 0 $ mkGroup
    --   [ withFillColor "white" $ polygonShape inflated
    --   -- , polygonNumDots inflated
    --   ]
    -- -- , translate (2) 0 $ mkGroup
    -- --   [ withFillColor "grey" $ polygonShape origin
    -- --   , polygonNumDots origin
    -- --   ]
    -- ]
  -- play $ mapA (withStrokeWidth (defaultStrokeWidth*0.2)) $ drawTriangulation cShape earClip'
  -- play $ staticFrame 1 $ renderTriangulation shape3 earClip
  -- play $ staticFrame 1 $ renderTriangulation shape4 earClip
  -- play $ staticFrame 1 $ renderTriangulation shape5 earClip
  -- play $ staticFrame 1 $ renderTriangulation shape6 earClip
  -- let p = deoverlapPolygon shape23
  -- newSpriteSVG $ translate (-3) 0 $ scale 2 $ center $ mkGroup
  --   [ withFillColor "grey" $ polygonShape p
  --   , polygonNumDots p
  --   ]
  -- wait 1
  -- play $ mapA (scale 2 . withStrokeWidth (defaultStrokeWidth*0.2)) $
  --   drawTriangulation p (earClip')
  return ()

drawVisibility :: Polygon -> Animation
drawVisibility p' = mkAnimation 5 $ \t ->
  let p = pCycle p' (t::Double) in
  centerUsing (polygonShape p) $
  mkGroup
  [ withFillColor "grey" $ polygonShape p
  , withFillColor "grey" $ polygonDots p
  , withFillColor "white" $ mkLinePathClosed
    [ (x,y) | V2 x y <- visibility (map (fmap realToFrac) $ V.toList $ polygonPoints p) ]
  , let V2 x y = fmap realToFrac $ pAccess p 0 in
    translate x y $ withFillColor "red" $ mkCircle 0.1
  -- , withFillColor "blue" $ latex $ T.pack $ show (t)
  ]

drawSSSPVisibility :: Polygon -> Animation
drawSSSPVisibility p' = mkAnimation 5 $ \t ->
  let p = pSetOffset p' (round $ t*(fromIntegral $ pSize p'-1))
      vis = ssspVisibility p in
  centerUsing (polygonShape p) $
  mkGroup
  [ withFillColor "grey" $ polygonShape p
  -- , withFillColor "grey" $ polygonDots p
  -- , withFillColor "white" $ polygonShape vis
  , let V2 x y = fmap realToFrac $ pAccess p 0 in
    translate x y $ withFillColor "red" $ mkCircle 0.1
  -- , withFillColor "blue" $ latex $ T.pack $ show (t)
  ]

drawSSSPVisibilityFast :: Polygon -> Animation
drawSSSPVisibilityFast p' = mkAnimation 5 $ \t ->
  let root = min (pSize p-1) $ (floor $ t*(fromIntegral $ pSize p))
      p = pSetOffset p' root
      vis = ssspVisibility p in
  -- centerUsing (polygonShape p) $
  mkGroup
  [ withFillColor "grey" $ polygonShape p
  , withFillColor "white" $ polygonShape vis
  , withFillColor "grey" $ polygonNumDots p
  , let V2 x y = fmap realToFrac $ pAccess p 0 in
    translate x y $ withFillColor "red" $ mkCircle 0.09
  -- , withFillColor "blue" $ latex $ T.pack $ show (t)
  ]

drawCompatible :: Polygon -> Polygon -> Animation
drawCompatible a b = sceneAnimation $ do
  newSpriteSVG $ translate (-3) 0 $ mkGroup
    [ withFillColor "grey" $ polygonShape a
    , withFillColor "grey" $ polygonNumDots a
    ]
  newSpriteSVG $ translate (3) 0 $ mkGroup
    [ withFillColor "grey" $ polygonShape b
    , withFillColor "grey" $ polygonNumDots b
    ]
  let compat = compatiblyTriangulateP a b
  forM_ compat $ \(l, r) -> do
    fork $ play $ staticFrame 1 $
      translate (-3) 0 $ withStrokeColor "white" $ withStrokeWidth (defaultStrokeWidth*0.2) $
      withFillOpacity 0 $ polygonShape l
    fork $ play $ staticFrame 1 $
      translate (3) 0 $ withStrokeColor "white" $ withStrokeWidth (defaultStrokeWidth*0.2) $
      withFillOpacity 0 $ polygonShape r
  -- forM_ compat $ \(l, r) -> waitOn $ do
  --   fork $ play $ staticFrame 1 $
  --     translate (-3) 0 $
  --     withFillColor "white" $ polygonShape l
  --   fork $ play $ staticFrame 1 $
  --     translate (3) 0 $
  --     withFillColor "white" $ polygonShape r

drawOverlap :: Polygon -> Animation
drawOverlap p' = mkAnimation 5 $ \t ->
  let p = pCycle p' (t::Double)
      vis = ssspVisibility p
      vis' = ssspVisibility p'
      mWins = ssspWindows p
      oWins = ssspWindows p'
      -- (left, right) = split1Link p' 0 3
      -- (left, right) = split2Link p' 0 2
      -- (left, right) = splitNLink p' 0 [(TwoLink,2),(OneLink,3)]
      -- (left, right) = splitNLink p' 0 [(OneLink,5),(TwoLink,3)]
      -- sPoly = if t < 0.5 then left else right
      in
  centerUsing (polygonShape p) $
  mkGroup
  [ withFillColor "grey" $ polygonShape p
  -- , withFillColor "grey" $ polygonDots p
  , withFillOpacity 0.5 $ withFillColor "lightgreen" $ polygonShape vis
  -- , withFillOpacity 0.5 $ withFillColor "cyan" $ polygonShape vis'
  , let V2 x y = fmap realToFrac $ pAccess p 0 in
    translate x y $ withFillColor "red" $ mkCircle 0.1
  , let V2 x y = fmap realToFrac $ pAccess p' 0 in
    translate x y $ withFillColor "red" $ mkCircle 0.1
  -- , withFillColor "blue" $ latex $ T.pack $ show (t)
  , mkGroup
    [ withStrokeColor "red" $
      mkLine (x1,y1) (x2,y2)
    | (a,b) <- mWins
    , let V2 x1 y1 = realToFrac <$> a
          V2 x2 y2 = realToFrac <$> b
    ]
  -- , mkGroup
  --   [ withStrokeColor "blue" $
  --     mkLine (x1,y1) (x2,y2)
  --   | (a,b) <- oWins
  --   , let V2 x1 y1 = realToFrac <$> a
  --         V2 x2 y2 = realToFrac <$> b
  --   ]
  , mkGroup
    [ withStrokeColor "green" $mkGroup
      [ mkLine (x1,y1) (x2,y2)
      , mkLine (x1',y1') (x2',y2') ]
    | (a,b) <- mWins
    , (i,j) <- oWins
    , a == i || a == j || b == i || b == j
    , not (sort [a,b] == sort [i,j])
    , let V2 x1 y1 = realToFrac <$> a
          V2 x2 y2 = realToFrac <$> b
          V2 x1' y1' = realToFrac <$> i
          V2 x2' y2' = realToFrac <$> j
    ]
  -- , mkGroup
  --   [ let V2 x y = realToFrac <$> link in
  --     translate x y $
  --     withFillColor "red" $
  --     mkCircle 0.1
  --   | link <- [steiner2Link p' 0 2, steiner2Link p' 5 3] ]
  -- , withFillColor "blue" $ polygonShape sPoly
  ]

drawWindow :: Polygon -> SVG
drawWindow p =
  let mWins = ssspWindows p
      in
  mkGroup
  [ mkGroup
    [ mkLine (x1,y1) (x2,y2)
    | (a,b) <- mWins
    , let V2 x1 y1 = realToFrac <$> a
          V2 x2 y2 = realToFrac <$> b
    ]
  ]

drawWindowOverlap :: Polygon -> Int -> Int -> SVG
drawWindowOverlap p a b =
  let aWins = ssspWindows (pAdjustOffset p a)
      bWins = ssspWindows (pAdjustOffset p b)
      in
  mkGroup
  [ mkGroup $
    [ withStrokeColor "green" $mkGroup
      [ mkLine (x1,y1) (x2,y2)
      , mkLine (x1',y1') (x2',y2') ]
    | (a,b) <- aWins
    , (i,j) <- bWins
    , a == i || a == j || b == i || b == j
    , not (sort [a,b] == sort [i,j])
    , let V2 x1 y1 = realToFrac <$> a
          V2 x2 y2 = realToFrac <$> b
          V2 x1' y1' = realToFrac <$> i
          V2 x2' y2' = realToFrac <$> j
    ]
  ]

drawSSSP :: Polygon -> (Polygon -> SSSP) -> Animation
drawSSSP p gen = mkAnimation 5 $ \t -> centerUsing outline $
  let p' = pCycles p !! (round $ t*(fromIntegral $ pSize p-1)) in
  mkGroup
  [ outline
  , renderSSSP p' (gen p')
  -- , let V2 x y = fmap realToFrac $ pAccess (pCycle p t) 0 in
  --   translate x y $ withFillColor "red" $ mkCircle 0.1
  , withFillColor "grey" $ polygonNumDots $ p'
  ]
  where
    outline =
      withFillColor "grey" $ mkLinePathClosed
        [ (x,y) | V2 x y <- map (fmap realToFrac) (V.toList (polygonPoints p)  ++ [pAccess p 0]) ]

{-# INLINE drawSSSPFast #-}
drawSSSPFast :: Polygon -> Animation
drawSSSPFast p = mkAnimation 5 $ \t -> centerUsing outline $
  let root = (round $ t*(fromIntegral $ pSize p-1))
      d = dual root triangulation
      sTree = sssp (pRing p) d in
  mkGroup
  [ outline
  , renderSSSP p sTree
  -- , let V2 x y = fmap realToFrac $ pAccess (pCycle p t) 0 in
  --   translate x y $ withFillColor "red" $ mkCircle 0.1
  , withFillColor "grey" $ polygonNumDots $ p
  ]
  where
    triangulation = earClip $ pRing p
    outline =
      withFillColor "grey" $ mkLinePathClosed
        [ (x,y) | V2 x y <- map (fmap realToFrac) (V.toList (polygonPoints p) ++ [pAccess p 0]) ]

drawVisibleFrom :: Polygon -> Animation
drawVisibleFrom p = mkAnimation 5 $ \t -> centerUsing (polygonShape p) $ mkGroup
  [ withFillColor "grey" $ polygonShape p
  , withFillColor "grey" $ polygonDots p
  , renderVisibleFrom (pCycle p t)
  , let V2 x y = fmap realToFrac $ pAccess (pCycle p t) 0 in
    translate x y $ withFillColor "red" $ mkCircle 0.1
  ]


renderVisibleFrom :: Polygon -> SVG
renderVisibleFrom p = withStrokeColor "white" $ withFillColor "white" $ mkGroup
  [ mkGroup
    [ mkLine (ax,ay) (bx,by)
    , translate bx by $ mkCircle 0.1 ]
  | i <- visibilityArray (pRing p) V.! 0
  , let V2 ax ay = fmap realToFrac $ pAccess p 0
        V2 bx by = fmap realToFrac $ pAccess p i ]

drawTriangulation :: Polygon -> (Ring Rational -> [Triangulation]) -> Animation
drawTriangulation p gen = sceneAnimation $ do
  forM_ (gen $ pRing p) $ \t -> play $ staticFrame 1 $ renderTriangulation p t

renderDual :: Ring Rational -> Dual -> SVG
renderDual ring d = case d of
    Dual (a,b,c) l r -> mkGroup
      [ withFillColor "blue" $ mkTrig a b c
      , worker c a l
      , worker b c r
      ]
  where
    mkTrig a b c =
      let V2 x1 y1 = realToFrac <$> ringAccess ring a
          V2 x2 y2 = realToFrac <$> ringAccess ring b
          V2 x3 y3 = realToFrac <$> ringAccess ring c
      in mkLinePathClosed [ (x1, y1), (x2,y2), (x3,y3) ]
    worker p1 p2 EmptyDual = mkGroup []
    worker p1 p2 (NodeDual x l r) = mkGroup
      [ mkTrig p1 p2 x
      , worker x p2 l
      , worker p1 x r
      ]
-- data Dual = Dual (Int,Int,Int) -- (a,b,c)
--                   DualTree -- borders ca
--                   DualTree -- borders bc
--   deriving (Show)
--
-- data DualTree
--   = EmptyDual
--   | NodeDual Int -- axb triangle, a and b are from parent.
--       DualTree -- borders xb
--       DualTree -- borders ax
--   deriving (Show)
