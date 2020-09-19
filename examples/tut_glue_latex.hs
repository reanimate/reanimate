#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Main (main) where

import           Reanimate

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens          ((&), (^.))
import           Control.Monad
import           Data.Monoid
import           Graphics.SvgTree
import           System.Random
import "random-shuffle" System.Random.Shuffle

main :: IO ()
main = reanimate $ parA bg latexExample
  where
    bg = animate $ const $ mkBackgroundPixel (PixelRGBA8 252 252 252 0xFF)

latexExample :: Animation
latexExample = scene $ do
    -- Draw equation
    play $ drawAnimation strokedSvg
    sprites <- forM glyphs $ \(fn, _, elt) ->
      newSpriteSVG $ fn elt
    -- Yoink each glyph
    forM_ (reverse sprites) $ \sprite -> do
      spriteE sprite (overBeginning 1 $ aroundCenterE $ highlightE)
      wait 0.5
    -- Flash glyphs randomly with color
    forM_ (shuffleList (sprites++sprites)) $ \sprite -> do
      spriteE sprite (overBeginning 0.5 $ aroundCenterE $ flashE)
      wait 0.1
    wait 0.5
    mapM_ destroySprite sprites
    -- Undraw equations
    play $ drawAnimation' (Just 0xdeadbeef) 1 0.1 strokedSvg
      & reverseA
  where
    glyphs = svgGlyphs svg
    strokedSvg =
      withStrokeWidth (defaultStrokeWidth*0.5) $
      withStrokeColor "black" svg
    svg = lowerTransformations $ simplify $ scale 2 $ center $
      latexAlign "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
    shuffleList lst = shuffle' lst (length lst) (mkStdGen 0xdeadbeef)

highlightE :: Effect
highlightE d t =
  scale (1 + bellS 2 (t/d)*0.5) . rotate (wiggleS (t/d) * 20)

flashE :: Effect
flashE d t =
  withStrokeColor "black" .
  withStrokeWidth (defaultStrokeWidth*0.5*bellS 2 (t/d)) .
  withFillColorPixel (promotePixel $ turbo (t/d))

-- s-curve, sin, s-curve
wiggleS :: Signal
wiggleS t
  | t < 0.25  = curveS 2 (t*4)
  | t < 0.75  = sin ((t-0.25)*2*pi+pi/2)
  | otherwise = curveS 2 ((t-0.75)*4)-1

--

drawAnimation :: SVG -> Animation
drawAnimation = drawAnimation' Nothing 0.5 0.3

drawAnimation' :: Maybe Int -> Double -> Double -> SVG -> Animation
drawAnimation' mbSeed fillDur step svg = scene $ do
  forM_ (zip [0..] $ shuf $ svgGlyphs svg) $ \(n, (fn, attr, tree)) -> do
    let sWidth =
          case toUserUnit defaultDPI <$> getLast (attr ^. strokeWidth) of
            Just (Num d) -> d
            _            -> defaultStrokeWidth
    fork $ do
      wait (n*step)
      play $ mapA fn $ (animate (\t -> withFillOpacity 0 $ partialSvg t tree)
        & applyE (overEnding fillDur $ fadeLineOutE sWidth))
    fork $ do
      wait (n*step+(1-fillDur))
      newSprite $ do
        t <- spriteT
        return $
          withStrokeWidth 0 $ fn $ withFillOpacity (min 1 $ t/fillDur) tree
  where
    shuf lst =
      case mbSeed of
        Nothing   -> lst
        Just seed -> shuffle' lst (length lst) (mkStdGen seed)
