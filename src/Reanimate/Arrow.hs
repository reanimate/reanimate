{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Reanimate.Arrow where

import           Control.Arrow
import qualified Control.Category as C
import           Data.Fixed
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           Lucid.Svg

type Duration = Double
type Time = Double

data Animation a b = Animation Duration (Duration -> Time -> a -> Svg b)

instance Functor (Animation a) where
  fmap f (Animation d g) = Animation d (\dur t a -> fmap f (g dur t a))

instance C.Category Animation where
  id = Animation 0 (\_ _ -> pure)
  Animation a fn1 . Animation b fn2 = Animation (max a b) (\d t a -> fn2 d t a >>= fn1 d t)

instance Arrow Animation where
  -- arr :: (b -> c) -> Animation b c
  arr fn = Animation 0 (\d t -> pure . fn)
  -- first :: Animation b c -> Animation (b, d) (c, d)
  first (Animation duration fn) =
    Animation duration (\t dur (b,d) -> do c <- fn t dur b; pure (c, d))

type Ani a = Animation () a

duration :: Double -> Animation a ()
duration duration = Animation duration (\_ _ _ -> pure ())

freezeAtEnd :: Animation a b -> Animation a b
freezeAtEnd (Animation d fn) = Animation d (\_ t -> fn d (min t d))

animationDuration :: Animation a b -> Double
animationDuration (Animation d _) = d

frameAt :: Double -> Ani () -> Svg ()
frameAt n (Animation d fn) = svg $ fn d (n `mod'` d) ()
  where
    svg :: Svg () -> Svg ()
    svg content = do
      doctype_
      with (svg11_ content) [width_ "320" , height_ "180", viewBox_ "0 0 320 180"]
      -- with (svg11_ content) [width_ "1280" , height_ "720", viewBox_ "0 0 320 180"]

emit :: Animation (Svg ()) ()
emit = Animation 0 (\d t svg -> svg)

getTime :: Ani Double
getTime = Animation 0 (\d t () -> pure t)

getDuration :: Ani Duration
getDuration = Animation 0 (\d t () -> pure d)
