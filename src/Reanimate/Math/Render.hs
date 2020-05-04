module Reanimate.Math.Render where

import           Codec.Picture.Types
import           Control.Monad
-- import           Data.List
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Linear.V2
import           Reanimate.Animation
import           Reanimate.ColorMap
-- import           Reanimate.Constants
import           Reanimate.LaTeX
-- import           Reanimate.Math.Compatible
-- import           Reanimate.Math.EarClip
import           Reanimate.Math.Polygon
import           Reanimate.Math.Common
import           Reanimate.Math.SSSP
import           Reanimate.Math.Triangulate
-- import           Reanimate.Math.Visibility
import           Reanimate.Scene
import           Reanimate.Svg


-- drawVisibility :: Polygon -> Animation
-- drawVisibility p' = mkAnimation 5 $ \t ->
--   let p = cyclePolygon p' (t::Double) in
--   centerUsing (polygonShape p) $
--   mkGroup
--   [ withFillColor "grey" $ polygonShape p
--   , withFillColor "grey" $ polygonDots p
--   , withFillColor "white" $ mkLinePathClosed
--     [ (x,y) | V2 x y <- visibility (map (fmap realToFrac) $ V.toList $ polygonPoints p) ]
--   , let V2 x y = fmap realToFrac $ pAccess p 0 in
--     translate x y $ withFillColor "red" $ mkCircle 0.1
--   -- , withFillColor "blue" $ latex $ T.pack $ show (t)
--   ]

-- drawSSSPVisibility :: Polygon -> Animation
-- drawSSSPVisibility p' = mkAnimation 5 $ \t ->
--   let p = pSetOffset p' (round $ t*(fromIntegral $ pSize p'-1))
--       vis = ssspVisibility p in
--   centerUsing (polygonShape p) $
--   mkGroup
--   [ withFillColor "grey" $ polygonShape p
--   -- , withFillColor "grey" $ polygonDots p
--   -- , withFillColor "white" $ polygonShape vis
--   , let V2 x y = fmap realToFrac $ pAccess p 0 in
--     translate x y $ withFillColor "red" $ mkCircle 0.1
--   -- , withFillColor "blue" $ latex $ T.pack $ show (t)
--   ]

-- drawSSSPVisibilityFast :: Polygon -> Animation
-- drawSSSPVisibilityFast p' = mkAnimation 5 $ \t ->
--   let root = min (pSize p-1) $ (floor $ t*(fromIntegral $ pSize p))
--       p = pSetOffset p' root
--       vis = ssspVisibility p in
--   -- centerUsing (polygonShape p) $
--   mkGroup
--   [ withFillColor "grey" $ polygonShape p
--   , withFillColor "white" $ polygonShape vis
--   , withFillColor "grey" $ polygonNumDots p
--   , let V2 x y = fmap realToFrac $ pAccess p 0 in
--     translate x y $ withFillColor "red" $ mkCircle 0.09
--   -- , withFillColor "blue" $ latex $ T.pack $ show (t)
--   ]

-- drawCompatible :: Polygon -> Polygon -> Animation
-- drawCompatible a b = sceneAnimation $ do
--   newSpriteSVG $ translate (-3) 0 $ mkGroup
--     [ withFillColor "grey" $ polygonShape a
--     , withFillColor "grey" $ polygonNumDots a
--     ]
--   newSpriteSVG $ translate (3) 0 $ mkGroup
--     [ withFillColor "grey" $ polygonShape b
--     , withFillColor "grey" $ polygonNumDots b
--     ]
--   let compat = compatiblyTriangulateP a b
--   forM_ compat $ \(l, r) -> do
--     fork $ play $ staticFrame 1 $
--       translate (-3) 0 $ withStrokeColor "white" $ withStrokeWidth (defaultStrokeWidth*0.2) $
--       withFillOpacity 0 $ polygonShape l
--     fork $ play $ staticFrame 1 $
--       translate (3) 0 $ withStrokeColor "white" $ withStrokeWidth (defaultStrokeWidth*0.2) $
--       withFillOpacity 0 $ polygonShape r

-- drawWindow :: Polygon -> SVG
-- drawWindow p =
--   let mWins = ssspWindows p
--       in
--   mkGroup
--   [ mkGroup
--     [ mkLine (x1,y1) (x2,y2)
--     | (a,b) <- mWins
--     , let V2 x1 y1 = realToFrac <$> a
--           V2 x2 y2 = realToFrac <$> b
--     ]
--   ]

-- drawWindowOverlap :: Polygon -> Int -> Int -> SVG
-- drawWindowOverlap p a b =
--   let aWins = ssspWindows (modOffset p a)
--       bWins = ssspWindows (modOffset p b)
--       in
--   mkGroup
--   [ mkGroup $
--     [ withStrokeColor "green" $mkGroup
--       [ mkLine (x1,y1) (x2,y2)
--       , mkLine (x1',y1') (x2',y2') ]
--     | (a,b) <- aWins
--     , (i,j) <- bWins
--     , a == i || a == j || b == i || b == j
--     , not (sort [a,b] == sort [i,j])
--     , let V2 x1 y1 = realToFrac <$> a
--           V2 x2 y2 = realToFrac <$> b
--           V2 x1' y1' = realToFrac <$> i
--           V2 x2' y2' = realToFrac <$> j
--     ]
--   ]

polygonShape :: Polygon -> SVG
polygonShape p = mkLinePathClosed
  [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList (polygonPoints p) ]

polygonDots :: Polygon -> SVG
polygonDots p = mkGroup
  [ translate x y $ mkCircle 0.1 | V2 x y <- V.toList $ V.map (fmap realToFrac) $ polygonPoints p ]

polygonNumDots :: Polygon -> SVG
polygonNumDots p = mkGroup $ reverse
    [ mkGroup
      [ colored n $
        translate x y $ mkCircle circR
      , withFillColor "black" $
        translate x y $ ppNum n ]
    | n <- [0..pSize p-1]
    , let V2 x y = realToFrac <$> pAccess p n ]
  where
    circR = 0.05
    colored n =
      let c = promotePixel $ turbo (fromIntegral (n+2) / fromIntegral (pSize p-1+2))
      in withStrokeColorPixel c . withFillColorPixel c
    ppNum n =
      scaleToSize (circR*1.7) (circR*1.5) $
      center $ latex $ T.pack $ "\\texttt{" ++ show n ++ "}"

-- drawSSSP :: Polygon -> (Polygon -> SSSP) -> Animation
-- drawSSSP p gen = mkAnimation 5 $ \t -> centerUsing outline $
--   let p' = cyclePolygons p !! (round $ t*(fromIntegral $ pSize p-1)) in
--   mkGroup
--   [ outline
--   , renderSSSP p' (gen p')
--   , withFillColor "grey" $ polygonNumDots $ p'
--   ]
--   where
--     outline =
--       withFillColor "grey" $ mkLinePathClosed
--         [ (x,y) | V2 x y <- map (fmap realToFrac) (V.toList (polygonPoints p)  ++ [pAccess p 0]) ]

drawSSSP :: Polygon -> Animation
drawSSSP p = mkAnimation 5 $ \t -> centerUsing (polygonShape p) $
  let root = (round $ t*(fromIntegral $ pSize p-1))
      sTree = polygonSSSP p V.! root in
  mkGroup
  [ polygonShape p
  , renderSSSP p sTree
  , polygonNumDots p
  ]

-- Inline such that 'trees' is only computed once.
{-# INLINE drawSSSPNaive #-}
drawSSSPNaive :: Polygon -> Animation
drawSSSPNaive p = mkAnimation 5 $ \t -> centerUsing (polygonShape p) $
  let root = (round $ t*(fromIntegral $ pSize p-1)) in
  mkGroup
  [ polygonShape p
  , renderSSSP p (trees !! root)
  , polygonNumDots p
  ]
  where
    trees =
      [ naive $ pRing $ pCopy $ pSetOffset p i
      | i <- [0 .. pSize p-1]]

renderSSSP :: Polygon -> SSSP -> SVG
renderSSSP p s = withFillOpacity 0 $ withStrokeColor "white" $ mkGroup
  [ mkLinePath (lineFrom i)
  | i <- [0 .. length s-1] ]
  where
    lineFrom i =
      let V2 ax ay = fmap realToFrac $ pAccess p i
          next = (s V.! i)
      in (ax,ay) : if next == i then [] else lineFrom next

drawTriangulation :: Polygon -> (Ring Rational -> [Triangulation]) -> Animation
drawTriangulation p gen = sceneAnimation $ do
  forM_ (gen $ pRing p) $ \t -> play $ staticFrame 1 $ renderTriangulation p t

renderTriangulation :: Polygon -> Triangulation -> SVG
renderTriangulation p t = center $ mkGroup
  [ withFillColor "grey" $ polygonShape p
  , withStrokeColor "white" $ mkGroup $ concat
    [ [ mkLine (ax,ay) (bx,by) ]
    | i <- [0..pSize p-1]
    , y <- t V.! i
    , let V2 ax ay = fmap realToFrac $ rawAccess i
          V2 bx by = fmap realToFrac $ rawAccess y
    ]
  , withFillColor "grey" $ polygonNumDots p
  ]
  where
    rawAccess x = polygonPoints p V.! x

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
    worker _p1 _p2 EmptyDual = mkGroup []
    worker p1 p2 (NodeDual x l r) = mkGroup
      [ mkTrig p1 p2 x
      , worker x p2 l
      , worker p1 x r
      ]
