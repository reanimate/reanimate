{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Properties where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified Data.Vector as V

import           Reanimate.Math.Common
import           Reanimate.Math.EarClip

-- [(0.8,1),(0.04,0.1),(0.05,1)]

newtype PolyParam = PolyParam (Double, Double)
  deriving (Show)
instance Arbitrary PolyParam where
  arbitrary = fmap PolyParam $
    (,) <$> choose (0.01,0.99)
              <*> choose (0.01,1)
  shrink (PolyParam arg) =
    [ PolyParam (a, b)
    | (a,b) <- shrink arg
    , a >= 0.1, b >= 0.1 ]

prop_genPolygon (PolyParam a) (PolyParam b) (PolyParam c) (PolyParam d) =
  isSimple $ genPolygon 1 [a,b,c,d]

instance Arbitrary Polygon where
  arbitrary =
    frequency
      [ (1, elements premade)
      , (99, randomPoly) ]
    where
      premade = [shape1, shape2, shape3, shape4, shape5, shape6]
      randomPoly = do
        r <- choose (0.1, 1)
        let genParameters = do
              angMod <- choose (0.01, 0.99)
              rMod   <- choose (0.01, 0.99)
              return (angMod, rMod)
        PolyParam e1 <- arbitrary
        PolyParam e2 <- arbitrary
        PolyParam e3 <- arbitrary
        PolyParam e4 <- arbitrary
        es <- listOf genParameters
        pure $ genPolygon r (e1:e2:e3:e4:es)
  -- shrink p = sublistOf (V.toList p)

prop_ccw p = label (if isConvex p then "convex" else "concave") $ isCCW p
prop_rev_ccw p = not $ isCCW (V.reverse p)
prop_cyclePolygon_ccw p = forAll (choose (0,1)) $ \t ->
  isSimple (cyclePolygon p t)

prop_validEarClip p = isValidTriangulation p (earClip p)

return []
all_props :: TestTree
all_props = testProperties "properties" $allProperties
