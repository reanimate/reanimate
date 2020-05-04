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
import           Reanimate.Math.EarClip
import           Reanimate.Math.SSSP
import           Helpers

import Debug.Trace

prop_genPolygon (PolyParam a) (PolyParam b) (PolyParam c) (PolyParam d) =
  isSimple $ genPolygon [a,b,c,d]

prop_isSimple p = isSimple p

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

prop_winding = forAll (choose (1,100)) $ \n -> isSimple (winding n)

prop_ccw p = label (if isConvex p then "convex" else "concave") $ isCCW p
prop_rev_ccw p = not $ isCCW (mkPolygon $ V.reverse $ polygonPoints p)
prop_cyclePolygon p = forAll (choose (0,1)) $ \t ->
  isSimple (cyclePolygon p t)

prop_validEarClip p = isValidTriangulation p (earClip $ polygonRing p)

-- dualToTrangulation . dual = id
prop_dualInv p =
  let t = polygonTriangulation p in
  dualToTriangulation (polygonRing p) (dual (polygonOffset p) t) == t

prop_ssspEq p =
  polygonSize p < 20 ==>
  naive (polygonRing p) == polygonSSSP p V.! 0

prop_ssspVisibilityLength (Parameters xs) =
  let p = genPolygon xs in
  polygonSize (ssspVisibility p) <= polygonSize p

prop_dualCycle p = forAll (choose (0,polygonSize p-1)) $ \n ->
  let p1 = setOffset p n
      p2 = polygonCopy p1
  in and [ polygonParent p1 0 i == polygonParent p2 0 i | i <- [0.. polygonSize p-1] ]

return []
all_props :: TestTree
all_props = testProperties "properties" $allProperties
