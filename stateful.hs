{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Stateful where

import Control.Monad.ST
import Data.STRef
import System.Mem.StableName
import Debug.Trace
import System.IO.Unsafe
import Control.Monad.ST.Unsafe
import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)

import Reanimate.Monad

data World
type ZIndex = Int

-- (seq duration, par duration)
-- [(Time, Animation, ZIndex)]
-- Map Time [(Animation, ZIndex)]
type Timeline = [(Time, Animation, ZIndex)]
newtype M s a = M { unM :: Time -> ST s (a, Duration, Duration, Timeline) }

unionTimeline :: Timeline -> Timeline -> Timeline
unionTimeline = (++)

emptyTimeline :: Timeline
emptyTimeline = []

instance Functor (M s) where
  fmap f action = M $ \t -> do
    (a, d1, d2, tl) <- unM action t
    return (f a, d1, d2, tl)

instance Applicative (M s) where
  pure a = M $ \_ -> return (a, 0, 0, emptyTimeline)
  f <*> g = M $ \t -> do
    (f', s1, p1, tl1) <- unM f t
    (g', s2, p2, tl2) <- unM g (t+s1)
    return (f' g', s1+s2, max p1 p2, unionTimeline tl1 tl2)

instance Monad (M s) where
  return = pure
  f >>= g = M $ \t -> do
    (a, s1, p1, tl1) <- unM f t
    (b, s2, p2, tl2) <- unM (g a) (t+s1)
    return (b, s1+s2, p1+p2, unionTimeline tl1 tl2)

sceneAnimation :: (forall s. M s a) -> Animation
sceneAnimation _ = undefined

debug :: String -> M s ()
debug msg = M $ \t -> trace msg (return ((), 0, 0, emptyTimeline))

someaction :: M s ()
someaction = debug "someaction"

fork :: M s a -> M s a
fork (M action) = M $ \t -> do
  (a, s, p, tl) <- action t
  return (a, 0, max s p, tl)

play :: Animation -> M s ()
play = playZ 0

playZ :: ZIndex -> Animation -> M s ()
playZ z ani = M $ \t -> do
  let d = duration ani
  return ((), d, 0, [(t, ani, z)])

queryNow :: M s Time
queryNow = M $ \t -> return (t, 0, 0, emptyTimeline)

-- Wait until all forked and sequential animations have finished.
waitAll :: M s a -> M s a
waitAll (M action) = M $ \t -> do
  (a, s, p, tl) <- action t
  return (a, max s p, 0, tl)

waitUntil :: Time -> M s ()
waitUntil tNew = M $ \t ->
  return ((), max 0 (tNew-t), 0, emptyTimeline)

{-
blackness
show numbers
fade in colormap
slide to middle

do fork $ play $ setDuration 5 showNumbers
      # fadeOut 1 # fadeIn 1
   wait 3

   fork $ do
    playZ (-1) $ setDuration 5 $ revealImage 0 0.5
      # pauseAround 1 1
    playZ (-1) $ setDuration 5 $ revealImage 0.5 1
      # pauseAtEnd 1

   play $ setDuration 5 $ colormap 0 0.5
     # fadeIn 1 # pauseAround 1 1
   play $ setDuration 5 $ colormap 0.5 1
     # fadeOut 1 # pauseAtEnd 1

-}
