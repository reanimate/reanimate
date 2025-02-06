#!/usr/bin/env cabal
{- cabal:
build-depends: base
            , reanimate
            , reanimate-svg
            , vector
            , random
            , JuicyPixels
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
module Main
  ( main
  )
where

import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.ColorComponents
import           System.Random
import           Data.List
import           Codec.Picture.Types
import qualified Data.Vector                   as V

main :: IO ()
main = reanimate $ scene $ do
  newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
  play $ trails 0.05 starAnimation

starAnimation :: Animation
starAnimation = mkAnimation 10 $ \t ->
  let camZ = t * 4
      dropInvisible = dropWhile $ \(_, _, z) -> z < camZ
      placeDot (x, y, z) = 
        let newZ = z - camZ
        in  translate (x / newZ) (y / newZ) $ dot (1 - newZ) 
      starSVGs = map placeDot . reverse . take nStars . dropInvisible $ allStars
  in  withStrokeWidth 0 $ rotate (t * 360) $ mkGroup starSVGs 
 where
  black = PixelRGB8 0x0 0x0 0x0
  dot o =
    withFillColorPixel
        ( promotePixel
        $ interpolateRGB8 labComponents (dropTransparency rtfdBackgroundColor) black o
        )
      $ mkCircle 0.05

{-# INLINE trails #-}
trails :: Double -> Animation -> Animation
trails trailDur raw = mkAnimation (duration raw) $ \t ->
  let idx = round (t * fromIntegral nFrames)
  in  construct $ reverse [idx - trailFrames .. idx]
 where
  fps = 200

  construct :: [Int] -> SVG
  construct = foldr go (mkGroup []) where
    go idx acc = mkGroup $ [reduceOpacity acc, getFrame idx]
    reduceOpacity = withGroupOpacity (fromIntegral trailFrames / fromIntegral (trailFrames + 1))
    getFrame idx = frames V.! (idx `mod` nFrames)

  trailFrames = round (trailDur * fps)
  nFrames     = round (duration raw * fps)
  frames = V.fromList
    [ frameAt (fromIntegral i / fromIntegral nFrames * duration raw) raw
    | i <- [0 .. nFrames]
    ]

nStars :: Int
nStars = 1000

stars, allStars :: [(Double, Double, Double)]
allStars = [ (x, y, z + n) | n <- [0 ..], (x, y, z) <- stars ]
stars = sortOn takeZ $ take nStars
  [ (x, y, z)
  | x <- randomRs (-screenWidth/2, screenWidth/2) seedX
  | y <- randomRs (-screenWidth/2, screenWidth/2) seedY
  | z <- randomRs (0, 1) seedZ ]
  where takeZ (_,_,z) = z

seedX, seedY, seedZ :: StdGen
seedX = mkStdGen 0xDEAFBEEF
seedY = mkStdGen 0x12345678
seedZ = mkStdGen 0x87654321
