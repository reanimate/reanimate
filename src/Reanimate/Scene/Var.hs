{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Reanimate.Scene.Var where

import Control.Monad.ST (ST)
import qualified Data.Map as M
import Data.STRef
import Reanimate.Animation (Duration, Time)
import Reanimate.Scene.Core (Scene, liftST, queryNow, wait)

-- | Time dependent variable.
newtype Var s a = Var (STRef s (VarData a))

-- Note: We must ensure that upon transforming an VarData,
--       1. evarDefault old == evarDefault new
--       2. isNothing (evarLastTime old) || isJust (evarLastTime new) i.e. once evarLastValue has a Just value,
--          it shouldn't be Nothing again.
--       3. isNothing (evarLastTime var) => M.null (evarTimeline var)
data VarData a = VarData
  { evarDefault :: a,
    evarTimeline :: Timeline a,
    evarLastTime :: Maybe Time,
    evarLastValue :: a
  }

data Modifier a = StaticValue a | TweenValue Duration (a -> Time -> a)

type Timeline a = M.Map Time (Modifier a)

-- | Create a new variable with a default value.
--   Variables always have a defined value even if they are read at a timestamp that is
--   earlier than when the variable was created. For example:
--
-- @
-- do v \<- 'Reanimate.Scene.fork' ('wait' 10 \>\> 'newVar' 0) -- Create a variable at timestamp '10'.
--    'readVar' v                       -- Read the variable at timestamp '0'.
--                                    -- The value of the variable will be '0'.
-- @
newVar :: a -> Scene s (Var s a)
newVar def = Var <$> liftST (newSTRef $ VarData def M.empty Nothing def)

-- | Read the value of a variable at the current timestamp.
readVar :: Var s a -> Scene s a
readVar (Var ref) = readVarData <$> liftST (readSTRef ref) <*> queryNow

unpackVar :: Var s a -> ST s (Time -> a)
unpackVar (Var ref) = readVarData <$> readSTRef ref

-- | Write the value of a variable at the current timestamp.
--
--   Example:
--
-- @
-- do v \<- 'newVar' 0
--    'Reanimate.Scene.newSprite' $ 'Reanimate.Svg.Constructors.mkCircle' \<$\> 'Reanimate.Scene.unVar' v
--    'writeVar' v 1; 'wait' 1
--    'writeVar' v 2; 'wait' 1
--    'writeVar' v 3; 'wait' 1
-- @
--
--   <<docs/gifs/doc_writeVar.gif>>
writeVar :: Var s a -> a -> Scene s ()
writeVar (Var ref) val = do
  now <- queryNow
  liftST $ modifySTRef ref $ writeVarData now val

-- | Modify the value of a variable at the current timestamp and all future timestamps.
modifyVar :: Var s a -> (a -> a) -> Scene s ()
modifyVar (Var ref) fn = do
  now <- queryNow
  liftST $ modifySTRef ref $ modifyVarData now fn

-- | Modify a variable between @now@ and @now+duration@.
tweenVar :: Var s a -> Duration -> (a -> Time -> a) -> Scene s ()
tweenVar _ dur _ | dur < 0 = error "Reanimate.tweenVar: durations must be non-negative"
tweenVar (Var ref) dur fn = do
  now <- queryNow
  liftST $ modifySTRef ref $ tweenVarData now dur fn
  wait dur

readVarData :: VarData a -> Time -> a
readVarData (VarData def _ Nothing _) _ = def
readVarData (VarData def timeline (Just lastTime) lastValue) now
  | now < lastTime = lookupTimeline timeline def now
  | otherwise = lastValue

lookupTimeline :: Timeline a -> a -> Time -> a
lookupTimeline timeline def now = case M.lookupLE now timeline of
  Just (_, StaticValue sVal) -> sVal
  Just (t, TweenValue dur f)
    | t + dur > now -> f def now
  _ -> def

writeVarData :: Time -> a -> VarData a -> VarData a
writeVarData now x var =
  let before = keepBefore now var
      after = VarData (evarDefault var) M.empty (Just now) x
   in after `elseVar` before

modifyVarData :: Time -> (a -> a) -> VarData a -> VarData a
modifyVarData now fn var =
  let before = keepBefore now var
      after = keepFrom now var
      timeline = flip M.map (evarTimeline after) $ \case
        StaticValue s -> StaticValue $ fn s
        TweenValue dur f -> TweenValue dur $ \a t -> fn (f a t)
   in after {evarTimeline = timeline, evarLastValue = fn $ evarLastValue after} `elseVar` before

-- Note: The function passed here takes time on the scale 0 to 1
--       while the function in `TweenValue` takes time on an absolute scale.
tweenVarData :: Time -> Duration -> (a -> Time -> a) -> VarData a -> VarData a
tweenVarData st dur fn var@VarData {..} =
  let nd = st + dur
      before = keepBefore st var
      during = keepInRange (Just st) (Just nd) var
      tweenFn a t =
        let idx = (t - st) / dur
            idx' = if isNaN idx then 1 else idx
         in fn (readVarData (during {evarDefault = a}) t) idx'
      valueTweenEnd = tweenFn evarDefault nd -- we'll never use the def here, replace with error?
      after = VarData evarDefault (M.singleton st $ TweenValue dur tweenFn) (Just nd) valueTweenEnd
   in after `elseVar` before

-- Returns the union of two vars such that we use the second var if first var doesn't have a value.
-- Assumes both vars have same default value.
elseVar :: VarData a -> VarData a -> VarData a
elseVar var1 var2
  | Just t <- evarLastTime var1 =
    let afterTimeline = evarTimeline var1
        joinAt = maybe t fst $ M.lookupMin afterTimeline
        beforeTimeline = case keepBefore joinAt var2 of
          x
            | Just lastTime <- evarLastTime x, lastTime < joinAt -> M.insert lastTime (StaticValue $ evarLastValue x) $ evarTimeline x
            | otherwise -> evarTimeline x
     in var1 {evarTimeline = M.union afterTimeline beforeTimeline}
  | otherwise = var2

-- Restrict a var to a given time interval.
keepInRange :: Maybe Time -> Maybe Time -> VarData a -> VarData a
keepInRange st nd = maybe id keepFrom st . maybe id keepBefore nd

-- Restrict a var to start at given timestamp.
keepFrom :: Time -> VarData a -> VarData a
keepFrom st VarData {..} =
  let timeline' = M.dropWhileAntitone (< st) evarTimeline
      -- if there is no modifier in timeline starting at st,
      -- we must get the modifier that starts before and truncate it to start at st.
      timeline'' = case M.lookupLE st evarTimeline of
        Just (t, val@(StaticValue _))
          | t < st -> M.insert st val timeline'
        Just (t, TweenValue dur fn)
          | t < st, t + dur > st -> M.insert st (TweenValue (t + dur - st) fn) timeline'
        _ -> timeline'
   in VarData evarDefault timeline'' (max evarLastTime $ Just st) evarLastValue

-- Restrict a var to end(clamp) at given timestamp.
keepBefore :: Time -> VarData a -> VarData a
keepBefore nd var@VarData {..} =
  let timeline' = M.takeWhileAntitone (< nd) evarTimeline
      lastModifier = M.lookupMax timeline'
      timeline'' = case lastModifier of
        Just (t, TweenValue dur fn)
          | t + dur > nd -> M.insert t (TweenValue (nd - t) fn) timeline'
        _ -> timeline'
      lastTime = case lastModifier of
        Just (t, TweenValue dur _) -> Just $ min nd (t + dur)
        _ -> min nd <$> evarLastTime
   in VarData evarDefault timeline'' lastTime (maybe evarDefault (readVarData var) lastTime)
