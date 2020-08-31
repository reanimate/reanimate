{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -w      #-}
-- {-# LANGUAGE TemplateHaskell      #-}
module Helpers where

import qualified Data.Vector            as V
import           Linear.V2
import           Linear.Vector
import           Numeric
import           Data.List
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Reanimate.Math.Common
import           Reanimate.Math.Polygon
import           Reanimate.Math.SSSP

-- [(0.8,1),(0.04,0.1),(0.05,1)]

newtype PolyParam = PolyParam { unParam :: (Double, Double) }
  deriving (Show)
instance Arbitrary PolyParam where
  arbitrary = fmap PolyParam $
    (,) <$> choose (0.01,0.99)
              <*> choose (0.01,1)
  shrink (PolyParam (a,b)) =
    [ PolyParam (a', b')
    | a' <- a : shrinkDouble a
    , b' <- if a==a' then b : shrinkDouble b else b : shrinkDouble b
    , a' >= 0.01, b' >= 0.01
    ]

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
    map Parameters (delete xs $ shrinkListByOne xs) ++
    map Parameters (delete xs $
    sequence $ map (map unParam . shrink . PolyParam) xs)

shrinkListByOne :: [a] -> [[a]]
shrinkListByOne x | length x <= 4 = [x]
shrinkListByOne x = x : zipWith (++) (inits x) (tails $ drop 1 x)

shrinkDouble :: Double -> [Double]
shrinkDouble x =
  let s = showFFloat Nothing x "" in
  if length s < 4
    then []
    else [read (take (length s-1) s)]

instance Arbitrary Polygon where
  arbitrary =
    frequency
      [ (1, elements premade)
      , (2, pMkWinding <$> choose (1,100))
      , (99, randomPoly)
      ]
    where
      premade = [shape1, shape2, shape3, shape4, shape5, shape6
                ,shape7, shape8, shape9, shape11 ]
      randomPoly = do
        let genParameters = do
              angMod <- choose (0.01, 0.99)
              rMod   <- choose (0.01, 0.99)
              return (angMod, rMod)
        PolyParam e1 <- arbitrary
        PolyParam e2 <- arbitrary
        PolyParam e3 <- arbitrary
        PolyParam e4 <- arbitrary
        es <- listOf genParameters
        pure $ pGenerate (e1:e2:e3:e4:es)
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
