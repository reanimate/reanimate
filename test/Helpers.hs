{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -w      #-}
-- {-# LANGUAGE TemplateHaskell      #-}
module Helpers where

import qualified Data.Vector            as V
import           Linear.V2
import           Linear.Vector
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Reanimate.Math.Common
import           Reanimate.Math.EarClip
import           Reanimate.Math.SSSP

-- [(0.8,1),(0.04,0.1),(0.05,1)]

newtype PolyParam = PolyParam { unParam :: (Double, Double) }
  deriving (Show)
instance Arbitrary PolyParam where
  arbitrary = fmap PolyParam $
    (,) <$> choose (0.01,0.99)
              <*> choose (0.01,1)
  shrink (PolyParam arg) =
    [ PolyParam (a, b)
    | (a,b) <- shrink arg
    , a >= 0.1, b >= 0.1 ]

data Parameters = Parameters [(Double, Double)]
  deriving (Show)
instance Arbitrary Parameters where
  arbitrary = do
    PolyParam e1 <- arbitrary
    PolyParam e2 <- arbitrary
    PolyParam e3 <- arbitrary
    PolyParam e4 <- arbitrary
    rest <- arbitrary
    return $ Parameters $ e1:e2:e3:e4:map unParam rest
  shrink (Parameters xs) =
    map Parameters $
    sequence $ map (map unParam . shrink . PolyParam) xs

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
  -- shrink p = filter isSimple $
  --   map V.fromList (sequence (map shrinkV2 (V.toList p)))

shrinkV2 (V2 a b) = V2 <$> take 1 (shrinkRealFrac a) <*> take 1 (shrinkRealFrac b)

instance (RealFrac a, Arbitrary a) => Arbitrary (V2 a) where
  arbitrary = do
    (a,b) <- arbitrary
    pure $ V2 a b
  shrink = shrinkV2
  -- shrink (V2 a b) =
  --   [ V2 a b | (a',b') <- shrink (a,b) ]

percent :: (Testable prop) => (Rational -> prop) -> Property
percent gen = forAll (choose (0,1::Double)) $ gen . realToFrac
