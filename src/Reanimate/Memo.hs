{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

With animations defined as SVG images over time, it is unfortunately
quite easy to write inefficient code where expensive expressions are
re-evaluated for every frame even if nothing has changed. This get
around this issue, this module defines a global key-value table that
can store expensive expressions such that they are evaluated only once.

There is currently no way to clear values from the table and it is your
own responsibility to not consume all available memory.

-}
module Reanimate.Memo
  ( Key(..)
  , memo
  ) where

import           Data.Dynamic
import           Data.IORef
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Typeable
import           System.IO.Unsafe
import           System.Mem.StableName

data DynamicName = forall a. DynamicName !(StableName a) | forall a. (Eq a, Ord a, Typeable a) => DynamicKey a
instance Eq DynamicName where
  DynamicName a == DynamicName b = eqStableName a b
  DynamicKey a == DynamicKey b =
    case cast a of
      Nothing -> False
      Just a' -> a'==b
  _ == _ = False

instance Ord DynamicName where
  DynamicName a `compare` DynamicName b =
    hashStableName a `compare` hashStableName b
  DynamicName{} `compare` _ = LT
  DynamicKey a `compare` DynamicKey b =
    case cast a of
      Nothing -> typeOf a `compare` typeOf b
      Just a' -> a' `compare` b
  _ `compare` _ = GT

data CacheMap = CacheMap !(Map.Map DynamicName CacheMap) !(Map.Map DynamicName Dynamic)

emptyCacheMap :: CacheMap
emptyCacheMap = CacheMap Map.empty Map.empty

-- sizeCacheMap :: CacheMap -> Int
-- sizeCacheMap (CacheMap sub vals) =
--   sum (map sizeCacheMap (Map.elems sub)) + Map.size vals

cacheMapLookup :: [DynamicName] -> CacheMap -> Maybe Dynamic
cacheMapLookup [] _ = Nothing
cacheMapLookup [k] (CacheMap _ vals) = Map.lookup k vals
cacheMapLookup (k:ks) (CacheMap sub _) =
  cacheMapLookup ks =<< Map.lookup k sub

cacheMapInsert :: [DynamicName] -> Dynamic -> CacheMap -> CacheMap
cacheMapInsert [] _ m = m
cacheMapInsert [k] v (CacheMap sub vals) = CacheMap sub (Map.insert k v vals)
cacheMapInsert (k:ks) v (CacheMap sub vals) =
  CacheMap (Map.alter fn k sub) vals
  where
    fn = Just . cacheMapInsert ks v . fromMaybe emptyCacheMap

{-# NOINLINE cacheMap #-}
-- FIXME: There should be a way to clear the cache.
cacheMap :: IORef CacheMap
cacheMap = unsafePerformIO (newIORef emptyCacheMap)

-- | Keys can either by any boxed object with identity (to be compared with
--   StableNames) or a primitive type with an Eq instance.
data Key = forall a. Key !a | forall a. (Typeable a, Eq a, Ord a) => KeyPrim !a

fromKey :: Key -> IO DynamicName
fromKey (Key val)     = DynamicName <$> makeStableName val
fromKey (KeyPrim val) = pure (DynamicKey val)

-- | Cache expensive value in global store. You must guarantee that the
--   key uniquely determines the value.
memo :: Typeable a => [Key] -> a -> a
memo !k v = unsafePerformIO $ do
  keys <- mapM fromKey k
  atomicModifyIORef' cacheMap $ \m ->
    case fromDynamic =<< cacheMapLookup keys m of
      Just v' -> (m, v')
      Nothing -> (cacheMapInsert keys (toDyn v) m, v)
