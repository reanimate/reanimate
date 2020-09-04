{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -w      #-}
{-# LANGUAGE TemplateHaskell      #-}
module Properties where

import qualified Data.Vector            as V
import           Linear.V2
import           Linear.Vector
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Reanimate.Math.Common
import           Reanimate.Math.Polygon
import           Reanimate.Math.SSSP
import           Reanimate.Math.Triangulate
import           Helpers

import Debug.Trace

prop_pGenerate (PolyParam a) (PolyParam b) (PolyParam c) (PolyParam d) =
  pIsSimple $ pGenerate [a,b,c,d]

prop_pIsSimple p = pIsSimple p

prop_isBetween a b = percent $ \t ->
    a /= b ==>
    isBetween (lerp t a b) (a,b) &&
    isBetween a (a,b) &&
    isBetween b (a,b)

prop_isBetweenAxis a = percent $ \t ->
    let b = a + V2 0 1
        c = a + V2 1 0 in
    isBetween (lerp t a b) (a,b) &&
    isBetween (lerp t a c) (a,c)

prop_isNotBetween a b = forAll (choose (1.1,100)) $ \t ->
    a /= 0 && b /= 0 ==>
    not (isBetween (lerp (realToFrac (t::Double)) a b :: V2 Rational) (a,b))

prop_winding = forAll (choose (1,100)) $ \n -> pIsSimple (pMkWinding n)

prop_ccw p = label (if pIsConvex p then "convex" else "concave") $ pIsCCW p
prop_rev_ccw p = not $ pIsCCW (mkPolygon $ V.reverse $ polygonPoints p)
prop_pCycle p = forAll (choose (0,1)) $ \t ->
  pIsSimple (pCycle p t)

-- prop_validEarClip p = isValidTriangulation p (earClip $ pRing p)

-- prop_validEarCut p = isValidTriangulation p (earCut $ pRing p)

prop_validMonotone p = isValidTriangulation p (triangulate $ pRing p)

-- dualToTrangulation . dual = id
prop_dualInv p =
  let t = polygonTriangulation p in
  dualToTriangulation (pRing p) (dual (polygonOffset p) t) == t

prop_ssspEq p =
  pSize p < 20 ==>
  naive (pRing p) == polygonSSSP p V.! 0

prop_ssspEq2 :: Polygon -> Bool
prop_ssspEq2 p =
  let t = polygonTriangulation p
      d = dual (polygonOffset p) t
  in sssp (pRing p) d == ssspFinger (pRing p) d

prop_ssspSize :: Polygon -> Bool
prop_ssspSize p =
  length (sssp (pRing p) (dual 0 $ polygonTriangulation p)) == pSize p

prop_pCuts p =
  pSize p < 20 ==>
  all (\(l,r) -> pIsSimple l && pIsSimple r) (pCuts p)

prop_ssspVisibilityLength (Parameters xs) =
  let p = pGenerate xs in
  pSize (ssspVisibility p) <= pSize p

prop_dualCycle p = forAll (choose (0,pSize p-1)) $ \n ->
  let p1 = pSetOffset p n
      p2 = pCopy p1
  in and [ pParent p1 0 i == pParent p2 0 i | i <- [0.. pSize p-1] ]

return []
all_props :: TestTree
all_props = testProperties "properties" $allProperties
