{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-|
Module      : Reanimate.Scene
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

Scenes are an imperative way of defining animations.

-}
module Reanimate.Scene
  ( -- * Scenes
    Scene
  , ZIndex
  , scene             -- :: (forall s. Scene s a) -> Animation
  , sceneAnimation    -- :: (forall s. Scene s a) -> Animation
  , play              -- :: Animation -> Scene s ()
  , fork              -- :: Scene s a -> Scene s a
  , queryNow          -- :: Scene s Time
  , wait              -- :: Duration -> Scene s ()
  , waitUntil         -- :: Time -> Scene s ()
  , waitOn            -- :: Scene s a -> Scene s a
  , adjustZ           -- :: (ZIndex -> ZIndex) -> Scene s a -> Scene s a
  , withSceneDuration -- :: Scene s () -> Scene s Duration
  -- * Variables
  , Var
  , newVar            -- :: a -> Scene s (Var s a)
  , readVar           -- :: Var s a -> Scene s a
  , writeVar          -- :: Var s a -> a -> Scene s ()
  , modifyVar         -- :: Var s a -> (a -> a) -> Scene s ()
  , tweenVar          -- :: Var s a -> Duration -> (a -> Time -> a) -> Scene s ()
  , tweenVarUnclamped -- :: Var s a -> Duration -> (a -> Time -> a) -> Scene s ()
  , simpleVar         -- :: (a -> SVG) -> a -> Scene s (Var s a)
  , findVar           -- :: (a -> Bool) -> [Var s a] -> Scene s (Var s a)
  -- * Sprites
  , Sprite
  , Frame
  , unVar             -- :: Var s a -> Frame s a
  , spriteT           -- :: Frame s Time
  , spriteDuration    -- :: Frame s Duration
  , newSprite         -- :: Frame s SVG -> Scene s (Sprite s)
  , newSprite_        -- :: Frame s SVG -> Scene s ()
  , newSpriteA        -- :: Animation -> Scene s (Sprite s)
  , newSpriteA'       -- :: Sync -> Animation -> Scene s (Sprite s)
  , newSpriteSVG      -- :: SVG -> Scene s (Sprite s)
  , newSpriteSVG_     -- :: SVG -> Scene s ()
  , destroySprite     -- :: Sprite s -> Scene s ()
  , applyVar          -- :: Var s a -> Sprite s -> (a -> SVG -> SVG) -> Scene s ()
  , spriteModify      -- :: Sprite s -> Frame s ((SVG,ZIndex) -> (SVG, ZIndex)) -> Scene s ()
  , spriteMap         -- :: Sprite s -> (SVG -> SVG) -> Scene s ()
  , spriteTween       -- :: Sprite s -> Duration -> (Double -> SVG -> SVG) -> Scene s ()
  , spriteVar         -- :: Sprite s -> a -> (a -> SVG -> SVG) -> Scene s (Var s a)
  , spriteE           -- :: Sprite s -> Effect -> Scene s ()
  , spriteZ           -- :: Sprite s -> ZIndex -> Scene s ()
  , spriteScope       -- :: Scene s a -> Scene s a

  -- * Object API
  , Object
  , ObjectData
  , oNew
  , newObject
  , oModify
  , oModifyS
  , oRead
  , oTween
  , oTweenS
  , oTweenV
  , oTweenVS
  , Renderable(..)
  -- ** Object Properties
  , oTranslate
  , oSVG
  , oContext
  , oMargin
  , oMarginTop
  , oMarginRight
  , oMarginBottom
  , oMarginLeft
  , oBB
  , oBBMinX
  , oBBMinY
  , oBBWidth
  , oBBHeight
  , oOpacity
  , oShown
  , oZIndex
  , oEasing
  , oScale
  , oScaleOrigin
  , oTopY
  , oBottomY
  , oLeftX
  , oRightX
  , oCenterXY
  , oValue

  -- ** Graphics object methods
  , oShow
  , oHide
  , oFadeIn
  , oFadeOut
  , oGrow
  , oShrink
  , oTransform

  -- ** Pre-defined objects
  , Circle(..)
  , circleRadius
  , Rectangle(..)
  , rectWidth
  , rectHeight
  , Morph(..)
  , morphDelta
  , morphSrc
  , morphDst
  , Camera(..)
  , cameraAttach
  , cameraFocus
  , cameraSetZoom
  , cameraZoom
  , cameraSetPan
  , cameraPan

  -- * ST internals
  , liftST
  , transitionO
  , evalScene

  , efficiencyTesterVar
  , efficiencyTesterEVar
  , testVarsEqual
  )
where

import           Control.Lens
import           Control.Monad              (void)
import           Control.Monad.Fix
import           Control.Monad.ST
import           Control.Monad.State (execState, State)
import           Data.List
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.STRef
import           Graphics.SvgTree           (Tree (None))
import           Reanimate.Animation
import           Reanimate.Ease             (Signal, curveS, fromToS)
import           Reanimate.Effect
import           Reanimate.Svg.Constructors
import           Reanimate.Svg.BoundingBox
import           Reanimate.Transition
import           Reanimate.Morph.Common (morph)
import           Reanimate.Morph.Linear (linear)

import Debug.Trace

-- | The ZIndex property specifies the stack order of sprites and animations. Elements
--   with a higher ZIndex will be drawn on top of elements with a lower index.
type ZIndex = Int


-- (seq duration, par duration)
-- [(Time, Animation, ZIndex)]
-- Map Time [(Animation, ZIndex)]
type Gen s = ST s (Duration -> Time -> (SVG, ZIndex))
-- | A 'Scene' represents a sequence of animations and variables
--   that change over time.
newtype Scene s a = M { unM :: Time -> ST s (a, Duration, Duration, [Gen s]) }

instance Functor (Scene s) where
  fmap f action = M $ \t -> do
    (a, d1, d2, gens) <- unM action t
    return (f a, d1, d2, gens)

instance Applicative (Scene s) where
  pure a = M $ \_ -> return (a, 0, 0, [])
  f <*> g = M $ \t -> do
    (f', s1, p1, gen1) <- unM f t
    (g', s2, p2, gen2) <- unM g (t + s1)
    return (f' g', s1 + s2, max p1 (s1 + p2), gen1 ++ gen2)

instance Monad (Scene s) where
  return = pure
  f >>= g = M $ \t -> do
    (a, s1, p1, gen1) <- unM f t
    (b, s2, p2, gen2) <- unM (g a) (t + s1)
    return (b, s1 + s2, max p1 (s1 + p2), gen1 ++ gen2)

instance MonadFix (Scene s) where
  mfix fn = M $ \t -> mfix (\v -> let (a, _s, _p, _gens) = v in unM (fn a) t)

-- | Lift an ST action into the Scene monad.
liftST :: ST s a -> Scene s a
liftST action = M $ \_ -> action >>= \a -> return (a, 0, 0, [])

-- | Evaluate the value of a scene.
evalScene :: (forall s . Scene s a) -> a
evalScene action = runST $ do
  (val, _, _ , _) <- unM action 0
  return val

-- | Render a 'Scene' to an 'Animation'.
scene :: (forall s . Scene s a) -> Animation
scene = sceneAnimation

-- | Render a 'Scene' to an 'Animation'.
sceneAnimation :: (forall s . Scene s a) -> Animation
sceneAnimation action = runST
  (do
    (_, s, p, gens) <- unM action 0
    let dur = max s p
    genFns <- sequence gens
    return $ mkAnimation
      dur
      (\t -> mkGroup $ map fst $ sortOn
        snd
        [ spriteRender dur (t * dur) | spriteRender <- genFns ]
      )
  )

-- | Execute actions in a scene without advancing the clock. Note that scenes do not end before
--   all forked actions have completed.
--
--   Example:
--
-- @
-- do 'fork' $ 'play' 'Reanimate.Builtin.Documentation.drawBox'
--    'play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_fork.gif>>
fork :: Scene s a -> Scene s a
fork (M action) = M $ \t -> do
  (a, s, p, gens) <- action t
  return (a, 0, max s p, gens)

-- | Play an animation once and then remove it. This advances the clock by the duration of the
--   animation.
--
--   Example:
--
-- @
-- do 'play' 'Reanimate.Builtin.Documentation.drawBox'
--    'play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_play.gif>>
play :: Animation -> Scene s ()
play ani = newSpriteA ani >>= destroySprite

-- | Query the current clock timestamp.
--
--   Example:
--
-- @
-- do now \<- 'play' 'Reanimate.Builtin.Documentation.drawCircle' *\> 'queryNow'
--    'play' $ 'staticFrame' 1 $ 'scale' 2 $ 'withStrokeWidth' 0.05 $
--      'mkText' $ "Now=" <> T.pack (show now)
-- @
--
--   <<docs/gifs/doc_queryNow.gif>>
queryNow :: Scene s Time
queryNow = M $ \t -> return (t, 0, 0, [])

-- | Advance the clock by a given number of seconds.
--
--   Example:
--
-- @
-- do 'fork' $ 'play' 'Reanimate.Builtin.Documentation.drawBox'
--    'wait' 1
--    'play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_wait.gif>>
wait :: Duration -> Scene s ()
wait d = M $ \_ -> return ((), d, 0, [])

-- | Wait until the clock is equal to the given timestamp.
waitUntil :: Time -> Scene s ()
waitUntil tNew = do
  now <- queryNow
  wait (max 0 (tNew - now))

-- | Wait until all forked and sequential animations have finished.
--
--   Example:
--
-- @
-- do 'waitOn' $ 'fork' $ 'play' 'Reanimate.Builtin.Documentation.drawBox'
--    'play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_waitOn.gif>>
waitOn :: Scene s a -> Scene s a
waitOn (M action) = M $ \t -> do
  (a, s, p, gens) <- action t
  return (a, max s p, 0, gens)

-- | Change the ZIndex of a scene.
adjustZ :: (ZIndex -> ZIndex) -> Scene s a -> Scene s a
adjustZ fn (M action) = M $ \t -> do
  (a, s, p, gens) <- action t
  return (a, s, p, map genFn gens)
 where
  genFn gen = do
    frameGen <- gen
    return $ \d t -> let (svg, z) = frameGen d t in (svg, fn z)

-- | Query the duration of a scene.
withSceneDuration :: Scene s () -> Scene s Duration
withSceneDuration s = do
  t1 <- queryNow
  s
  t2 <- queryNow
  return (t2 - t1)

addGen :: Gen s -> Scene s ()
addGen gen = M $ \_ -> return ((), 0, 0, [gen])





-- Efficient time variable
newtype EVar s a = EVar (STRef s (EVarData a))


-- Note: We must ensure that upon transforming an EVarData, 
--       1. evarDefault old == evarDefault new
--       2. isNothing (evarLastTime old) || isJust (evarLastTime new) i.e. once evarLastValue has a Just value,
--          it shouldn't be Nothing again.
--       3. isNothing (evarLastTime var) => M.null (evarTimeline var)
data EVarData a = EVarData
  { evarDefault   :: a
  , evarTimeline  :: Timeline a
  , evarLastTime  :: Maybe Time
  , evarLastValue :: a
  }

data Modifier a = StaticValue a | TweenValue Duration (a -> Time -> a)

type Timeline a = M.Map Time (Modifier a)

newEVar :: a -> Scene s (EVar s a)
newEVar def = EVar <$> liftST (newSTRef $ EVarData def M.empty Nothing def)

readEVar :: EVar s a -> Scene s a
readEVar (EVar ref) = readEVarData <$> liftST (readSTRef ref) <*> queryNow

writeEVar :: EVar s a -> a -> Scene s ()
writeEVar (EVar ref) val = do
  now <- queryNow
  liftST $ modifySTRef ref $ writeEVarData now val

modifyEVar :: EVar s a -> (a -> a) -> Scene s ()
modifyEVar (EVar ref) fn = do
  now <- queryNow
  liftST $ modifySTRef ref $ modifyEVarData now fn

tweenEVar :: EVar s a -> Duration -> (a -> Time -> a) -> Scene s ()
tweenEVar (EVar ref) dur fn = do
  now <- queryNow
  liftST $ modifySTRef ref $ tweenEVarData now dur fn
  wait dur

readEVarData :: EVarData a -> Time -> a
readEVarData (EVarData def _ Nothing _) _ = def
readEVarData (EVarData def timeline (Just lastTime) lastValue) now
  | now < lastTime = lookupTimeline timeline def now
  | otherwise = lastValue

lookupTimeline :: Timeline a -> a -> Time -> a
lookupTimeline timeline def now = case M.lookupLE now timeline of
  Just (_, StaticValue sVal) -> sVal
  Just (t, TweenValue dur f)
    | t + dur > now -> f def now
  _ -> def

writeEVarData :: Time -> a -> EVarData a -> EVarData a
writeEVarData now x var =
  let before = keepBefore now var
      after = EVarData (evarDefault var) M.empty (Just now) x
  in after `elseVar` before

modifyEVarData :: Time -> (a -> a) -> EVarData a -> EVarData a
modifyEVarData now fn var =
  let before = keepBefore now var
      after = keepFrom now var
      timeline = flip M.map (evarTimeline after) $ \case
        StaticValue s -> StaticValue $ fn s
        TweenValue dur f -> TweenValue dur $ \a t -> fn (f a t)
   in  after {evarTimeline = timeline, evarLastValue = fn $ evarLastValue after} `elseVar` before

-- Note: The function passed here takes time on the scale 0 to 1
--       while the function in `TweenValue` takes time on an absolute scale.
tweenEVarData :: Time -> Duration -> (a -> Time -> a) -> EVarData a -> EVarData a
tweenEVarData st dur fn var@EVarData{..} =
  let nd = st + dur
      before = keepBefore st var
      during = keepInRange (Just st) (Just nd) var
      tweenFn a t = fn (readEVarData (during {evarDefault = a}) t) $ (t - st) / dur
      valueTweenEnd = tweenFn evarDefault nd -- we'll never use the def here, replace with error?
      after = EVarData evarDefault (M.singleton st $ TweenValue dur tweenFn) (Just nd) valueTweenEnd
  in  after `elseVar` before

-- Returns the union of two vars such that we use the second var if first var doesn't have a value.
-- Assumes both vars have same default value.
elseVar :: EVarData a -> EVarData a -> EVarData a
elseVar var1 var2
  | Just t <- evarLastTime var1 =
    let afterTimeline = evarTimeline var1
        joinAt = fromMaybe t . fmap fst $ M.lookupMin afterTimeline
        beforeTimeline = case keepBefore joinAt var2 of
          x | Just lastTime <- evarLastTime x, lastTime < joinAt -> M.insert lastTime (StaticValue $ evarLastValue x) $ evarTimeline x
            | otherwise -> evarTimeline x
     in var1 {evarTimeline = M.union afterTimeline beforeTimeline}
  | otherwise = var2

-- Restrict a var to a given time interval.
keepInRange :: Maybe Time -> Maybe Time -> EVarData a -> EVarData a
keepInRange st nd = fromMaybe id (keepFrom <$> st) . fromMaybe id (keepBefore <$> nd)

-- Restrict a var to start at given timestamp.
keepFrom :: Time -> EVarData a -> EVarData a
keepFrom st EVarData{..} =
  let timeline' = M.dropWhileAntitone (< st) evarTimeline
      -- if there is no modifier in timeline starting at st,
      -- we must get the modifier that starts before and truncate it to start at st.
      timeline'' = case M.lookupLE st evarTimeline of
        Just (t, val@(StaticValue _))
          | t < st -> M.insert st val timeline'
        Just (t, TweenValue dur fn)
          | t < st, t + dur > st -> M.insert st (TweenValue (t + dur - st) fn) timeline'
        _ -> timeline'
  in EVarData evarDefault timeline'' (max evarLastTime $ Just st) evarLastValue

-- Restrict a var to end(clamp) at given timestamp.
keepBefore :: Time -> EVarData a -> EVarData a
keepBefore nd var@EVarData{..} =
  let timeline' = M.takeWhileAntitone (< nd) evarTimeline
      lastModifier = M.lookupMax timeline'
      timeline'' = case lastModifier of
        Just (t, TweenValue dur fn)
          | t + dur > nd -> M.insert t (TweenValue (nd - t) fn) timeline'
        _ -> timeline'
      lastTime = case lastModifier of
        Just (t, TweenValue dur _) -> Just $ min nd (t + dur)
        _ -> min nd <$> evarLastTime
  in EVarData evarDefault timeline'' lastTime (fromMaybe evarDefault $ fmap (readEVarData var) lastTime)

-- If this prints 'expensive' twice then Var is not efficient.
efficiencyTesterVar :: Int
efficiencyTesterVar = evalScene $ do
  v <- newVar 0
  modifyVar v $ \old -> trace "expensive" 1 + old
  at0 <- readVar v
  wait 1
  at1 <- readVar v
  pure $ at0 + at1

-- If this prints 'expensive' twice then EVar is not efficient.
efficiencyTesterEVar :: Int
efficiencyTesterEVar = evalScene $ do
  v <- newEVar 0
  modifyEVar v $ \old -> trace "expensive" 1 + old
  at0 <- readEVar v
  wait 1
  at1 <- readEVar v
  pure $ at0+at1

-- | Time dependent variable.
newtype Var s a = Var (STRef s (Time -> a))

-- | Create a new variable with a default value.
--   Variables always have a defined value even if they are read at a timestamp that is
--   earlier than when the variable was created. For example:
--
-- @
-- do v \<- 'fork' ('wait' 10 \>\> 'newVar' 0) -- Create a variable at timestamp '10'.
--    'readVar' v                       -- Read the variable at timestamp '0'.
--                                    -- The value of the variable will be '0'.
-- @
newVar :: a -> Scene s (Var s a)
newVar def = Var <$> liftST (newSTRef (const def))

-- | Read the value of a variable at the current timestamp.
readVar :: Var s a -> Scene s a
readVar (Var ref) = liftST (readSTRef ref) <*> queryNow

-- | Write the value of a variable at the current timestamp.
--
--   Example:
--
-- @
-- do v \<- 'newVar' 0
--    'newSprite' $ 'mkCircle' \<$\> 'unVar' v
--    'writeVar' v 1; 'wait' 1
--    'writeVar' v 2; 'wait' 1
--    'writeVar' v 3; 'wait' 1
-- @
--
--   <<docs/gifs/doc_writeVar.gif>>
writeVar :: Var s a -> a -> Scene s ()
writeVar var val = modifyVar var (const val)

-- | Modify the value of a variable at the current timestamp and all future timestamps.
modifyVar :: Var s a -> (a -> a) -> Scene s ()
modifyVar (Var ref) fn = do
  now <- queryNow
  liftST $ modifySTRef ref $ \prev t -> if t < now then prev t else fn (prev t)

-- | Modify a variable between @now@ and @now+duration@.
--   Note: The modification function is invoked for past timestamps (with a time value of 0) and
--         for timestamps after @now+duration@ (with a time value of 1). See 'tweenVarUnclamped'.
tweenVar :: Var s a -> Duration -> (a -> Time -> a) -> Scene s ()
tweenVar (Var ref) dur fn = do
  now <- queryNow
  liftST $ modifySTRef ref $ \prev t ->
    if t < now
      then prev t
      else fn (prev t) (max 0 (min dur $ t - now) / dur)
  wait dur

-- | Modify a variable between @now@ and @now+duration@.
--   Note: The modification function is invoked for past timestamps (with a negative time value) and
--         for timestamps after @now+duration@ (with a time value greater than 1).
tweenVarUnclamped :: Var s a -> Duration -> (a -> Time -> a) -> Scene s ()
tweenVarUnclamped (Var ref) dur fn = do
  now <- queryNow
  liftST $ modifySTRef ref $ \prev t -> fn (prev t) ((t - now) / dur)
  wait dur

-- | Create and render a variable. The rendering will be born at the current timestamp
--   and will persist until the end of the scene.
--
--   Example:
--
-- @
-- do var \<- 'simpleVar' 'mkCircle' 0
--    'tweenVar' var 2 $ \val -> 'fromToS' val ('Reanimate.Constants.screenHeight'/2)
-- @
--
--   <<docs/gifs/doc_simpleVar.gif>>
simpleVar :: (a -> SVG) -> a -> Scene s (Var s a)
simpleVar render def = do
  v <- newVar def
  _ <- newSprite $ render <$> unVar v
  return v

-- | Helper function for filtering variables.
findVar :: (a -> Bool) -> [Var s a] -> Scene s (Var s a)
findVar _cond []       = error "Variable not found."
findVar cond  (v : vs) = do
  val <- readVar v
  if cond val then return v else findVar cond vs

-- | Sprites are animations with a given time of birth as well as a time of death.
--   They can be controlled using variables, tweening, and effects.
data Sprite s = Sprite Time (STRef s (Duration, ST s (Duration -> Time -> SVG -> (SVG, ZIndex))))

-- | Sprite frame generator. Generates frames over time in a stateful environment.
newtype Frame s a = Frame { unFrame :: ST s (Time -> Duration -> Time -> a) }

instance Functor (Frame s) where
  fmap fn (Frame gen) = Frame $ do
    m <- gen
    return (\real_t d t -> fn $ m real_t d t)

instance Applicative (Frame s) where
  pure v = Frame $ return (\_ _ _ -> v)
  Frame f <*> Frame g = Frame $ do
    m1 <- f
    m2 <- g
    return $ \real_t d t -> m1 real_t d t (m2 real_t d t)

-- | Dereference a variable as a Sprite frame.
--
--   Example:
--
-- @
-- do v \<- 'newVar' 0
--    'newSprite' $ 'mkCircle' \<$\> 'unVar' v
--    'tweenVar' v 1 $ \val -> 'fromToS' val 3
--    'tweenVar' v 1 $ \val -> 'fromToS' val 0
-- @
--
--   <<docs/gifs/doc_unVar.gif>>
unVar :: Var s a -> Frame s a
unVar (Var ref) = Frame $ do
  fn <- readSTRef ref
  return $ \real_t _d _t -> fn real_t


-- | Dereference seconds since sprite birth.
spriteT :: Frame s Time
spriteT = Frame $ return (\_real_t _d t -> t)

-- | Dereference duration of the current sprite.
spriteDuration :: Frame s Duration
spriteDuration = Frame $ return (\_real_t d _t -> d)

-- | Create new sprite defined by a frame generator. Unless otherwise specified using
--   'destroySprite', the sprite will die at the end of the scene.
--
--   Example:
--
-- @
-- do 'newSprite' $ 'mkCircle' \<$\> 'spriteT' -- Circle sprite where radius=time.
--    'wait' 2
-- @
--
--   <<docs/gifs/doc_newSprite.gif>>
newSprite :: Frame s SVG -> Scene s (Sprite s)
newSprite render = do
  now <- queryNow
  ref <- liftST $ newSTRef (-1, return $ \_d _t svg -> (svg, 0))
  addGen $ do
    fn                           <- unFrame render
    (spriteDur, spriteEffectGen) <- readSTRef ref
    spriteEffect                 <- spriteEffectGen
    return $ \d absT ->
      let relD = (if spriteDur < 0 then d else spriteDur) - now
          relT = absT - now
          -- Sprite is live [now;duration[
          -- If we're at the end of a scene, sprites
          -- are live: [now;duration]
          -- This behavior is difficult to get right. See the 'bug_*' examples for
          -- automated tests.
          inTimeSlice = relT >= 0 && relT < relD
          isLastFrame = d==absT && relT == relD
      in  if inTimeSlice || isLastFrame
            then spriteEffect relD relT (fn absT relD relT)
            else (None, 0)
  return $ Sprite now ref

-- | Create new sprite defined by a frame generator. The sprite will die at
--   the end of the scene.
newSprite_ :: Frame s SVG -> Scene s ()
newSprite_ = void . newSprite

-- | Create a new sprite from an animation. This advances the clock by the
--   duration of the animation. Unless otherwise specified using
--   'destroySprite', the sprite will die at the end of the scene.
--
--   Note: If the scene doesn't end immediately after the duration of the
--   animation, the animation will be stretched to match the lifetime of the
--   sprite. See 'newSpriteA'' and 'play'.
--
--   Example:
--
-- @
-- do 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawCircle'
--    'play' 'Reanimate.Builtin.Documentation.drawBox'
--    'play' $ 'reverseA' 'Reanimate.Builtin.Documentation.drawBox'
-- @
--
--   <<docs/gifs/doc_newSpriteA.gif>>
newSpriteA :: Animation -> Scene s (Sprite s)
newSpriteA = newSpriteA' SyncStretch

-- | Create a new sprite from an animation and specify the synchronization policy. This advances
--   the clock by the duration of the animation.
--
--   Example:
--
-- @
-- do 'fork' $ 'newSpriteA'' 'SyncFreeze' 'Reanimate.Builtin.Documentation.drawCircle'
--    'play' 'Reanimate.Builtin.Documentation.drawBox'
--    'play' $ 'reverseA' 'Reanimate.Builtin.Documentation.drawBox'
-- @
--
--   <<docs/gifs/doc_newSpriteA'.gif>>
newSpriteA' :: Sync -> Animation -> Scene s (Sprite s)
newSpriteA' sync animation =
  newSprite (getAnimationFrame sync animation <$> spriteT <*> spriteDuration)
    <* wait (duration animation)

-- | Create a sprite from a static SVG image.
--
--   Example:
--
-- @
-- do 'newSpriteSVG' $ 'mkBackground' "lightblue"
--    'play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_newSpriteSVG.gif>>
newSpriteSVG :: SVG -> Scene s (Sprite s)
newSpriteSVG = newSprite . pure

-- | Create a permanent sprite from a static SVG image. Same as `newSpriteSVG`
--   but the sprite isn't returned and thus cannot be destroyed.
newSpriteSVG_ :: SVG -> Scene s ()
newSpriteSVG_ = void . newSpriteSVG

-- | Change the rendering of a sprite using data from a variable. If data from several variables
--   is needed, use a frame generator instead.
--
--   Example:
--
-- @
-- do s \<- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawBox'
--    v \<- 'newVar' 0
--    'applyVar' v s 'rotate'
--    'tweenVar' v 2 $ \val -> 'fromToS' val 90
-- @
--
--   <<docs/gifs/doc_applyVar.gif>>
applyVar :: Var s a -> Sprite s -> (a -> SVG -> SVG) -> Scene s ()
applyVar var sprite fn = spriteModify sprite $ do
  varFn <- unVar var
  return $ \(svg, zindex) -> (fn varFn svg, zindex)

-- | Destroy a sprite, preventing it from being rendered in the future of the scene.
--   If 'destroySprite' is invoked multiple times, the earliest time-of-death is used.
--
--   Example:
--
-- @
-- do s <- 'newSpriteSVG' $ 'withFillOpacity' 1 $ 'mkCircle' 1
--    'fork' $ 'wait' 1 \>\> 'destroySprite' s
--    'play' 'Reanimate.Builtin.Documentation.drawBox'
-- @
--
--   <<docs/gifs/doc_destroySprite.gif>>
destroySprite :: Sprite s -> Scene s ()
destroySprite (Sprite _ ref) = do
  now <- queryNow
  liftST $ modifySTRef ref $ \(ttl, render) ->
    (if ttl < 0 then now else min ttl now, render)

-- | Low-level frame modifier.
spriteModify :: Sprite s -> Frame s ((SVG, ZIndex) -> (SVG, ZIndex)) -> Scene s ()
spriteModify (Sprite born ref) modFn = liftST $ modifySTRef ref $ \(ttl, renderGen) ->
  ( ttl
  , do
    render    <- renderGen
    modRender <- unFrame modFn
    return $ \relD relT ->
      let absT = relT + born in modRender absT relD relT . render relD relT
  )

-- | Map the SVG output of a sprite.
--
--   Example:
--
-- @
-- do s \<- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawCircle'
--    'wait' 1
--    'spriteMap' s 'flipYAxis'
-- @
--
--   <<docs/gifs/doc_spriteMap.gif>>
spriteMap :: Sprite s -> (SVG -> SVG) -> Scene s ()
spriteMap sprite@(Sprite born _) fn = do
  now <- queryNow
  let tDelta = now - born
  spriteModify sprite $ do
    t <- spriteT
    return $ \(svg, zindex) -> (if (t - tDelta) < 0 then svg else fn svg, zindex)

-- | Modify the output of a sprite between @now@ and @now+duration@.
--
--   Example:
--
-- @
-- do s \<- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawCircle'
--    'spriteTween' s 1 $ \val -> 'translate' ('Reanimate.Constants.screenWidth'*0.3*val) 0
-- @
--
--   <<docs/gifs/doc_spriteTween.gif>>
spriteTween :: Sprite s -> Duration -> (Double -> SVG -> SVG) -> Scene s ()
spriteTween sprite@(Sprite born _) dur fn = do
  now <- queryNow
  let tDelta = now - born
  spriteModify sprite $ do
    t <- spriteT
    return $ \(svg, zindex) -> (fn (clamp 0 1 $ (t - tDelta) / dur) svg, zindex)
  wait dur
 where
  clamp a b v | v < a     = a
              | v > b     = b
              | otherwise = v

-- | Create a new variable and apply it to a sprite.
--
--   Example:
--
-- @
-- do s \<- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawBox'
--    v \<- 'spriteVar' s 0 'rotate'
--    'tweenVar' v 2 $ \val -> 'fromToS' val 90
-- @
--
--   <<docs/gifs/doc_spriteVar.gif>>
spriteVar :: Sprite s -> a -> (a -> SVG -> SVG) -> Scene s (Var s a)
spriteVar sprite def fn = do
  v <- newVar def
  applyVar v sprite fn
  return v

-- | Apply an effect to a sprite.
--
--   Example:
--
-- @
-- do s <- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawCircle'
--    'spriteE' s $ 'overBeginning' 1 'fadeInE'
--    'spriteE' s $ 'overEnding' 0.5 'fadeOutE'
-- @
--
--   <<docs/gifs/doc_spriteE.gif>>
spriteE :: Sprite s -> Effect -> Scene s ()
spriteE (Sprite born ref) effect = do
  now <- queryNow
  liftST $ modifySTRef ref $ \(ttl, renderGen) ->
    ( ttl
    , do
      render <- renderGen
      return $ \d t svg ->
        let (svg', z) = render d t svg
        in  (delayE (max 0 $ now - born) effect d t svg', z)
    )

-- | Set new ZIndex of a sprite.
--
--   Example:
--
-- @
-- do s1 \<- 'newSpriteSVG' $ 'withFillOpacity' 1 $ 'withFillColor' "blue" $ 'mkCircle' 3
--    'newSpriteSVG' $ 'withFillOpacity' 1 $ 'withFillColor' "red" $ 'mkRect' 8 3
--    'wait' 1
--    'spriteZ' s1 1
--    'wait' 1
-- @
--
--   <<docs/gifs/doc_spriteZ.gif>>
spriteZ :: Sprite s -> ZIndex -> Scene s ()
spriteZ (Sprite born ref) zindex = do
  now <- queryNow
  liftST $ modifySTRef ref $ \(ttl, renderGen) ->
    ( ttl
    , do
      render <- renderGen
      return $ \d t svg ->
        let (svg', z) = render d t svg in (svg', if t < now - born then z else zindex)
    )

-- | Destroy all local sprites at the end of a scene.
--
--   Example:
--
-- @
-- do -- the rect lives through the entire 3s animation
--    'newSpriteSVG_' $ 'translate' (-3) 0 $ 'mkRect' 4 4
--    'wait' 1
--    'spriteScope' $ do
--      -- the circle only lives for 1 second.
--      local \<- 'newSpriteSVG' $ 'translate' 3 0 $ 'mkCircle' 2
--      'spriteE' local $ 'overBeginning' 0.3 'fadeInE'
--      'spriteE' local $ 'overEnding' 0.3 'fadeOutE'
--      'wait' 1
--    'wait' 1
-- @
--
--   <<docs/gifs/doc_spriteScope.gif>>
spriteScope :: Scene s a -> Scene s a
spriteScope (M action) = M $ \t -> do
  (a, s, p, gens) <- action t
  return (a, s, p, map (genFn (t+max s p)) gens)
 where
  genFn maxT gen = do
    frameGen <- gen
    return $ \_ t ->
      if t < maxT
        then frameGen maxT t
        else (None, 0)

asAnimation :: (forall s'. Scene s' a) -> Scene s Animation
asAnimation s = do
  now <- queryNow
  return $ dropA now (sceneAnimation (wait now >> s))

-- | Apply a transformation with a given overlap. This makes sure
--   to keep timestamps intact such that events can still be timed
--   by transcripts.
transitionO :: Transition -> Double -> (forall s'. Scene s' a) -> (forall s'. Scene s' b) -> Scene s ()
transitionO t o a b = do
  aA <- asAnimation a
  bA <- fork $ do
    wait (duration aA - o)
    asAnimation b
  play $ overlapT o t aA bA




-------------------------------------------------------
-- Objects

-- | Objects can be any Haskell structure as long as it can be rendered to SVG.
class Renderable a where
  toSVG :: a -> SVG

instance Renderable Tree where
  toSVG = id

-- | Objects are SVG nodes (represented as Haskell values) with
--   identity, location, and several other properties that can
--   change over time.
data Object s a = Object
  { objectSprite :: Sprite s
  , objectData   :: Var s (ObjectData a)
  }

-- | Container for object properties.
data ObjectData a = ObjectData
  { _oTranslate   :: (Double, Double)
  , _oValueRef    :: a
  , _oSVG         :: SVG
  , _oContext     :: SVG -> SVG
  , _oMargin      :: (Double, Double, Double, Double)
      -- ^ Top, right, bottom, left
  , _oBB          :: (Double,Double,Double,Double)
  , _oOpacity     :: Double
  , _oShown       :: Bool
  , _oZIndex      :: Int
  , _oEasing      :: Signal
  , _oScale       :: Double
  , _oScaleOrigin :: (Double, Double)
  }

-- Basic lenses

-- FIXME: Maybe 'position' is a better name.
-- | Object position. Default: \<0,0\>
oTranslate :: Lens' (ObjectData a) (Double, Double)
oTranslate = lens _oTranslate $ \obj val -> obj { _oTranslate = val }

-- | Rendered SVG node of an object. Does not include context
--   or object properties. Read-only.
oSVG :: Getter (ObjectData a) SVG
oSVG = to _oSVG

-- | Custom render context. Is applied to the object for every
--   frame that it is shown.
oContext :: Lens' (ObjectData a) (SVG -> SVG)
oContext = lens _oContext $ \obj val -> obj { _oContext = val  }

-- | Object margins (top, right, bottom, left) in local units.
oMargin :: Lens' (ObjectData a) (Double, Double, Double, Double)
oMargin = lens _oMargin $ \obj val -> obj { _oMargin = val }

-- | Object bounding-box (minimal X-coordinate, minimal Y-coordinate,
--   width, height). Uses `Reanimate.Svg.BoundingBox.boundingBox`
--   and has the same limitations.
oBB :: Getter (ObjectData a) (Double, Double, Double, Double)
oBB = to _oBB

-- | Object opacity. Default: 1
oOpacity :: Lens' (ObjectData a) Double
oOpacity = lens _oOpacity $ \obj val -> obj { _oOpacity = val }

-- | Toggle for whether or not the object should be rendered.
--   Default: False
oShown :: Lens' (ObjectData a) Bool
oShown = lens _oShown $ \obj val -> obj { _oShown = val }

-- | Object's z-index.
oZIndex :: Lens' (ObjectData a) Int
oZIndex = lens _oZIndex $ \obj val -> obj { _oZIndex = val }

-- | Easing function used when modifying object properties.
--   Default: @'Reanimate.Ease.curveS' 2@
oEasing :: Lens' (ObjectData a) Signal
oEasing = lens _oEasing $ \obj val -> obj { _oEasing = val }

-- | Object's scale. Default: 1
oScale :: Lens' (ObjectData a) Double
oScale = lens _oScale $ \obj val -> obj { _oScale = val }

-- | Origin point for scaling. Default: \<0,0\>
oScaleOrigin :: Lens' (ObjectData a) (Double, Double)
oScaleOrigin = lens _oScaleOrigin $ \obj val -> obj { _oScaleOrigin = val }

-- Smart lenses

-- | Lens for the source value contained in an object.
oValue :: Renderable a => Lens' (ObjectData a) a
oValue = lens _oValueRef $ \obj newVal ->
    let svg = toSVG newVal
    in obj
    { _oValueRef = newVal
    , _oSVG      = svg
    , _oBB       = boundingBox svg }

-- | Derived location of the top-most point of an object + margin.
oTopY :: Lens' (ObjectData a) Double
oTopY = lens getter setter
  where
    getter obj = 
      let top  = obj ^. oMarginTop
          miny = obj ^. oBBMinY
          h    = obj ^. oBBHeight
          dy   = obj ^. oTranslate . _2
      in dy+miny+h+top
    setter obj val =
      obj & (oTranslate . _2) +~ val-getter obj

-- | Derived location of the bottom-most point of an object + margin.
oBottomY :: Lens' (ObjectData a) Double
oBottomY = lens getter setter
  where
    getter obj = 
      let bot  = obj ^. oMarginBottom
          miny = obj ^. oBBMinY
          dy   = obj ^. oTranslate . _2
      in dy+miny-bot
    setter obj val = 
      obj & (oTranslate . _2) +~ val-getter obj

-- | Derived location of the left-most point of an object + margin.
oLeftX :: Lens' (ObjectData a) Double
oLeftX = lens getter setter
  where
    getter obj =
      let left = obj ^. oMarginLeft
          minx = obj ^. oBBMinX
          dx   = obj ^. oTranslate . _1
      in dx+minx-left
    setter obj val =
      obj & (oTranslate . _1) +~ val-getter obj

-- | Derived location of the right-most point of an object + margin.
oRightX :: Lens' (ObjectData a) Double
oRightX = lens getter setter
  where
    getter obj =
      let right = obj ^. oMarginRight
          minx  = obj ^. oBBMinX
          w     = obj ^. oBBWidth
          dx    = obj ^. oTranslate . _1
      in dx+minx+w+right
    setter obj val =
      obj & (oTranslate . _1) +~ val-getter obj

-- | Derived location of an object's center point.
oCenterXY :: Lens' (ObjectData a) (Double, Double)
oCenterXY = lens getter setter
  where
    getter obj =
      let minx    = obj ^. oBBMinX
          miny    = obj ^. oBBMinY
          w       = obj ^. oBBWidth
          h       = obj ^. oBBHeight
          (dx,dy) = obj ^. oTranslate
      in (dx+minx+w/2, dy+miny+h/2)
    setter obj (dx, dy) =
      let (x,y) = getter obj in
      obj & (oTranslate . _1) +~ dx-x
          & (oTranslate . _2) +~ dy-y

-- | Object's top margin.
oMarginTop :: Lens' (ObjectData a) Double
oMarginTop = oMargin . _1

-- | Object's right margin.
oMarginRight :: Lens' (ObjectData a) Double
oMarginRight = oMargin . _2

-- | Object's bottom margin.
oMarginBottom :: Lens' (ObjectData a) Double
oMarginBottom = oMargin . _3

-- | Object's left margin.
oMarginLeft :: Lens' (ObjectData a) Double
oMarginLeft = oMargin . _4

-- | Object's minimal X-coordinate..
oBBMinX :: Getter (ObjectData a) Double
oBBMinX = oBB . _1

-- | Object's minimal Y-coordinate..
oBBMinY :: Getter (ObjectData a) Double
oBBMinY = oBB . _2

-- | Object's width without margin.
oBBWidth :: Getter (ObjectData a) Double
oBBWidth = oBB . _3

-- | Object's height without margin.
oBBHeight :: Getter (ObjectData a) Double
oBBHeight = oBB . _4

-------------------------------------------------------------------------------
-- Object modifiers

-- | Modify object properties.
oModify :: Object s a -> (ObjectData a -> ObjectData a) -> Scene s ()
oModify o fn = modifyVar (objectData o) fn

-- | Modify object properties using a stateful API.
oModifyS :: Object s a -> (State (ObjectData a) b) -> Scene s ()
oModifyS o fn = oModify o (execState fn)

-- | Query object property.
oRead :: Object s a -> Getting b (ObjectData a) b -> Scene s b
oRead o l = view l <$> readVar (objectData o)

-- | Modify object properties over a set duration.
oTween :: Object s a -> Duration -> (Double -> ObjectData a -> ObjectData a) -> Scene s ()
oTween o d fn = do
  -- Read 'easing' var here instead of taking it from 'v'.
  -- This allows different easing functions even at the same timestamp.
  ease <- oRead o oEasing
  tweenVar (objectData o) d (\v t -> fn (ease t) v)

-- | Modify object properties over a set duration using a stateful API.
oTweenS :: Object s a -> Duration -> (Double -> State (ObjectData a) b) -> Scene s ()
oTweenS o d fn = oTween o d (\t -> execState (fn t))

-- | Modify object value over a set duration. This is a convenience function
--   for modifying `oValue`.
oTweenV :: Renderable a => Object s a -> Duration -> (Double -> a -> a) -> Scene s ()
oTweenV o d fn = oTween o d (\t -> oValue %~ fn t)

-- | Modify object value over a set duration using a stateful API. This is a
--   convenience function for modifying `oValue`.
oTweenVS :: Renderable a => Object s a -> Duration -> (Double -> State a b) -> Scene s ()
oTweenVS o d fn = oTween o d (\t -> oValue %~ execState (fn t))

-- | Create new object.
oNew :: Renderable a => a -> Scene s (Object s a)
oNew = newObject

-- | Create new object.
newObject :: Renderable a => a -> Scene s (Object s a)
newObject val = do
  ref <- newVar ObjectData
    { _oTranslate = (0,0)
    , _oValueRef = val
    , _oSVG = svg
    , _oContext = id
    , _oMargin = (0.5,0.5,0.5,0.5)
    , _oBB = boundingBox svg
    , _oOpacity = 1
    , _oShown = False
    , _oZIndex = 1
    , _oEasing = curveS 2
    , _oScale = 1
    , _oScaleOrigin = (0,0)
    }
  sprite <- newSprite $ do
    ~ObjectData{..} <- unVar ref
    pure $
      if _oShown
        then
          uncurry translate _oTranslate $
          uncurry translate (_oScaleOrigin & both %~ negate) $
          scale _oScale $
          uncurry translate _oScaleOrigin $
          withGroupOpacity _oOpacity $
          _oContext _oSVG
        else None
  spriteModify sprite $ do
    ~ObjectData{_oZIndex=z} <- unVar ref
    pure $ \(img,_) -> (img,z)
  return Object
    { objectSprite = sprite
    , objectData   = ref }
  where
    svg = toSVG val

-------------------------------------------------------------------------------
-- Graphical transformations

-- | Instantly show object.
oShow :: Object s a -> Scene s ()
oShow o = oModify o $ oShown .~ True

-- | Instantly hide object.
oHide :: Object s a -> Scene s ()
oHide o = oModify o $ oShown .~ False

-- | Fade in object over a set duration.
oFadeIn :: Object s a -> Duration -> Scene s ()
oFadeIn o d = do
  oModify o $ 
    oShown   .~ True
  oTweenS o d $ \t ->
    oOpacity *= t

-- | Fade out object over a set duration.
oFadeOut :: Object s a -> Duration -> Scene s ()
oFadeOut o d = do
  oModify o $ 
    oShown   .~ True
  oTweenS o d $ \t ->
    oOpacity *= 1-t

-- | Scale in object over a set duration.
oGrow :: Object s a -> Duration -> Scene s ()
oGrow o d = do
  oModify o $ 
    oShown .~ True
  oTweenS o d $ \t ->
    oScale *= t

-- | Scale out object over a set duration.
oShrink :: Object s a -> Duration -> Scene s ()
oShrink o d =
  oTweenS o d $ \t ->
    oScale *= 1-t

-- FIXME: Also transform attributes: 'opacity', 'scale', 'scaleOrigin'.
-- | Morph source object into target object over a set duration.
oTransform :: Object s a -> Object s b -> Duration -> Scene s ()
oTransform src dst d = do
    srcSvg <- oRead src oSVG
    srcCtx <- oRead src oContext
    srcEase <- oRead src oEasing
    srcLoc <- oRead src oTranslate
    oModify src $ oShown .~ False
    
    dstSvg <- oRead dst oSVG
    dstCtx <- oRead dst oContext
    dstLoc <- oRead dst oTranslate

    m <- newObject $ Morph 0 (srcCtx srcSvg) (dstCtx dstSvg)
    oModifyS m $ do
      oShown     .= True
      oEasing    .= srcEase
      oTranslate .= srcLoc
    fork $ oTween m d $ \t -> oTranslate %~ moveTo t dstLoc
    oTweenV m d $ \t -> morphDelta .~ t
    oModify m $ oShown .~ False
    oModify dst $ oShown .~ True
  where
    moveTo t (dstX, dstY) (srcX, srcY) =
      (fromToS srcX dstX t, fromToS srcY dstY t)


-------------------------------------------------------------------------------
-- Built-in objects

-- | Basic object mapping to \<circle\/\> in SVG.
newtype Circle = Circle {_circleRadius :: Double}

-- | Circle radius in local units.
circleRadius :: Lens' Circle Double
circleRadius = iso _circleRadius Circle

instance Renderable Circle where
  toSVG (Circle r) = mkCircle r

-- | Basic object mapping to \<rect\/\> in SVG.
data Rectangle = Rectangle { _rectWidth :: Double, _rectHeight :: Double }

-- | Rectangle width in local units.
rectWidth :: Lens' Rectangle Double
rectWidth = lens _rectWidth $ \obj val -> obj{_rectWidth=val}

-- | Rectangle height in local units.
rectHeight :: Lens' Rectangle Double
rectHeight = lens _rectHeight $ \obj val -> obj{_rectHeight=val}

instance Renderable Rectangle where
  toSVG (Rectangle w h) = mkRect w h

-- | Object representing an interpolation between SVG nodes.
data Morph = Morph { _morphDelta :: Double, _morphSrc :: SVG, _morphDst :: SVG }

-- | Control variable for the interpolation. A value of 0 gives the
--   source SVG and 1 gives the target svg.
morphDelta :: Lens' Morph Double
morphDelta = lens _morphDelta $ \obj val -> obj{_morphDelta = val}

-- | Source shape.
morphSrc :: Lens' Morph SVG
morphSrc = lens _morphSrc $ \obj val -> obj{_morphSrc = val}

-- | Target shape.
morphDst :: Lens' Morph SVG
morphDst = lens _morphDst $ \obj val -> obj{_morphDst = val}

instance Renderable Morph where
  toSVG (Morph t src dst) = morph linear src dst t

-- | Cameras can take control of objects and manipulate them
--   with convenient pan and zoom operations.
data Camera = Camera
instance Renderable Camera where
  toSVG Camera = None

-- | Connect an object to a camera such that
--   camera settings (position, zoom, and rotation) is
--   applied to the object.
--
--   Example
--
-- @
-- do cam \<- 'newObject' 'Camera'
--    circ \<- 'newObject' $ 'Circle' 2
--    'oModifyS' circ $
--      'oContext' .= 'withFillOpacity' 1 . 'withFillColor' "blue"
--    'oShow' circ
--    'cameraAttach' cam circ
--    'cameraZoom' cam 1 2
--    'cameraZoom' cam 1 1
-- @
--
--   <<docs/gifs/doc_cameraAttach.gif>>
cameraAttach :: Object s Camera -> Object s a -> Scene s ()
cameraAttach cam obj =
  spriteModify (objectSprite obj) $ do
    camData <- unVar (objectData cam)
    return $ \(svg,zindex) ->
      let (x,y) = camData^.oTranslate
          ctx =
            translate (-x) (-y) .
            uncurry translate (camData^.oScaleOrigin) .
            scale (camData^.oScale) .
            uncurry translate (camData^.oScaleOrigin & both %~ negate)
      in (ctx svg, zindex)

-- |
--
--   Example
--
-- @
-- do cam \<- 'newObject' 'Camera'
--    circ \<- 'newObject' $ 'Circle' 2; 'oShow' circ
--    'oModify' circ $ 'oTranslate' .~ (-3,0)
--    box \<- 'newObject' $ 'Rectangle' 4 4; 'oShow' box
--    'oModify' box $ 'oTranslate' .~ (3,0)
--    'cameraAttach' cam circ
--    'cameraAttach' cam box
--    'cameraFocus' cam (-3,0)
--    'cameraZoom' cam 2 2      -- Zoom in
--    'cameraZoom' cam 2 1      -- Zoom out
--    'cameraFocus' cam (3,0)
--    'cameraZoom' cam 2 2      -- Zoom in
--    'cameraZoom' cam 2 1      -- Zoom out
-- @
--
--   <<docs/gifs/doc_cameraFocus.gif>>
cameraFocus :: Object s Camera -> (Double, Double) -> Scene s ()
cameraFocus cam (x,y) = do
  (ox, oy) <- oRead cam oScaleOrigin
  (tx, ty) <- oRead cam oTranslate
  s <- oRead cam oScale
  let newLocation = (x-((x-ox)*s+ox-tx), y-((y-oy)*s+oy-ty))
  oModifyS cam $ do
    oTranslate .= newLocation
    oScaleOrigin .= (x,y)

-- | Instantaneously set camera zoom level.
cameraSetZoom :: Object s Camera -> Double -> Scene s ()
cameraSetZoom cam s =
  oModifyS cam $
    oScale .= s

-- | Change camera zoom level over a set duration.
cameraZoom :: Object s Camera -> Duration -> Double -> Scene s ()
cameraZoom cam d s =
  oTweenS cam d $ \t ->
    oScale %= \v -> fromToS v s t

-- | Instantaneously set camera location.
cameraSetPan :: Object s Camera -> (Double, Double) -> Scene s ()
cameraSetPan cam location =
  oModifyS cam $ do
    oTranslate .= location

-- | Change camera location over a set duration.
cameraPan :: Object s Camera -> Duration -> (Double, Double) -> Scene s ()
cameraPan cam d (x,y) =
  oTweenS cam d $ \t -> do
    oTranslate._1 %= \v -> fromToS v x t
    oTranslate._2 %= \v -> fromToS v y t
