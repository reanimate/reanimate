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
import           Reanimate.Math.EarClip
import           Reanimate.Math.SSSP
import           Helpers

prop_genPolygon (PolyParam a) (PolyParam b) (PolyParam c) (PolyParam d) =
  isSimple $ genPolygon 1 [a,b,c,d]

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

prop_ccw p = label (if isConvex p then "convex" else "concave") $ isCCW p
prop_rev_ccw p = not $ isCCW (V.reverse p)
prop_cyclePolygon_ccw p = forAll (choose (0,1)) $ \t ->
  isSimple (cyclePolygon p t)

prop_validEarClip p = isValidTriangulation p (earClip p)

-- prop_ssspEq (Parameters xs) =
--   let p = genPolygon 1 xs
--   in naive p == sssp p (dual (earClip p))

return []
all_props :: TestTree
all_props = testProperties "properties" $allProperties
