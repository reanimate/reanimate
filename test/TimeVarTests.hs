{-# LANGUAGE RankNTypes #-}
module TimeVarTests
  ( timeVarTests
  ) where

import           Control.Exception
import           Control.Monad.ST.Unsafe
import           Data.IORef
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit

import           Reanimate
import           Reanimate.Scene

timeVarTests :: TestTree
timeVarTests =
  testGroup "Time variables"
  [ expectFail $ tc "Error check1" $ do
      v <- newEVar (0::Double)
      checkAt v
        [ (0,0), (0.25, 0.25), (0.5, 0.5), (1,0) ]
  , expectFail $ tc "Error check2" $ do
      io $ True @?= False
  , tc "Default value" $ do
      v <- newEVar True
      checkAt v
        [ (-1, True), (0, True), (1, True) ]
  , tc "Modified value" $ do
      v <- newEVar False
      writeEVar v True
      checkAt v
        [(-1, False), (0, True), (1, True)]
  , tc "Modified value" $ do
      v <- newEVar False
      writeEVar v True
      writeEVar v False
      checkAt v
        [(-1, False), (0, False), (1, False)]
  , tc "Overwrite future" $ do
      v <- newEVar "def"
      fork $ do
        wait 1
        writeEVar v "written in the future"
      writeEVar v "overwrite"
      checkAt v
        [ (-1, "def"), (0, "overwrite"), (1, "overwrite") ]

  , tc "Interrupt tween" $ do
      v <- newEVar 0
      fork $ tweenEVar v 1 $ \prev t -> fromToS prev 1 t
      checkAt v
        [ (0,0), (0.25, 0.25), (0.5, 0.5), (1,1) ]
      fork $ do
        wait 0.5
        writeEVar v 0.5
      checkAt v
        [ (0,0), (0.25, 0.25), (0.5, 0.5), (1,0.5) ]
  , tc "Double tween" $ do
      v <- newEVar 0
      fork $ tweenEVar v 1 $ \prev t -> fromToS prev 1 t
      fork $ tweenEVar v 1 $ \prev t -> fromToS prev 0 t
      checkAt v
        [ (0,0), (0.25, 0.25*0.75), (0.5, 0.5*0.5), (1,1*0) ]
  , expectFail $ tc "Negative tween duration" $ do
      v <- newEVar 0
      tweenEVar v (-1) $ \prev t -> fromToS prev 1 t
  , tc "Tween 1" $ do
      v <- newEVar 0
      wait 1
      tweenEVar v 1 $ \prev t -> fromToS prev 1 t
      checkAt v
        [ (0,0), (1,0), (1.5,0.5), (2,1), (3,1) ]
  , tc "Tween 2" $ do
      v <- newEVar (0::Int)
      wait 1
      tweenEVar v 1 $ \_prev _t -> 1
      checkAt v
        [ (0,0), (1,1), (1.5,1), (2,1), (3,1) ]
  , tc "Tween 3" $ do
      v <- newEVar (0::Int)
      wait 1
      tweenEVar v 0 $ \_prev _t -> 1
      checkAt v
        [ (0,0), (1,1), (2,1)]
  , tc "Tween 4" $ do
      v <- newEVar (0::Double)
      tweenEVar v 1 $ \prev t -> fromToS prev 1 t
      tweenEVar v 1 $ \prev t -> fromToS prev 0 t
      checkAt v
        [ (0,0), (0.5,0.5), (1,1), (1.5,0.5), (2,0)]
  , tc "Tween 5" $ do
      v <- newEVar (0::Double)
      tweenEVar v 1 $ \prev t -> fromToS prev 1 t
      tweenEVar v 1 $ \prev t -> fromToS prev 0 t
      wait (-1)
      writeEVar v 2
      checkAt v
        [ (0,0), (0.5,0.5), (1,2), (1.5,2), (2,2)]
  , tc "Tween zero duration" $ do
      v <- newEVar 0
      wait 1
      tweenEVar v 0 $ \_prev t -> t
      checkAt v
        [ (0,0), (1,1), (2,1)]
  , tc "Performance/writeEVar" $ do
      v <- newEVar (0::Int)
      ref <- io $ newIORef (0::Int)
      modifyEVar v $ \old -> unsafePerformIO (modifyIORef ref (+1) >> return 1) + old
      at0 <- readEVar v
      wait 1
      at1 <- readEVar v
      io $ at0+at1 @?= 2
      counter <- io $ readIORef ref
      io $ counter @?= 1
  , tc "Performance/tweenEVar" $ do
      v <- newEVar (0::Double)
      ref <- io $ newIORef (0::Int)
      let expensive prev t = unsafePerformIO $ do
            modifyIORef ref (+1)
            evaluate (fromToS prev 1 t)
      tweenEVar v 1 expensive
      -- Reading at 0.5 and 1 should call the expensive function.
      -- Reading at 2 and 3 should reuse the result from 1.
      -- The count should therefore be 2 and not 4.
      checkAt v
        [ (0.5,0.5), (1,1), (2,1), (3,1) ]
      counter <- io $ readIORef ref
      io $ counter @?= 2
  , tc "Performance/tweenEVar 2" $ do
      v <- newEVar (0::Double)
      fork $ wait 10 >> writeEVar v 1
      ref <- io $ newIORef (0::Int)
      let expensive prev t = unsafePerformIO $ do
            modifyIORef ref (+1)
            evaluate (fromToS prev 1 t)
      tweenEVar v 1 expensive
      wait 2
      writeEVar v 2
      -- Reading at 0.5 and 1 should call the expensive function.
      -- Reading at 2 should reuse the result from 1.
      -- The count should therefore be 2 and not 4.
      checkAt v
        [ (0.5,0.5), (1,1), (2,1), (3,2), (4,2) ]
      counter <- io $ readIORef ref
      io $ counter @?= 2
  , expectFail $ tc "Performance (old vars)" $ do
      v <- newVar (0::Int)
      ref <- io $ newIORef (0::Int)
      modifyVar v $ \old -> unsafePerformIO (modifyIORef ref (+1) >> return 1) + old
      at0 <- readVar v
      wait 1
      at1 <- readVar v
      io $ at0+at1 @?= 2
      counter <- io $ readIORef ref
      io $ counter @?= 1
  ]

checkAt :: (Eq a, Show a) => EVar s a -> [(Time, a)] -> Scene s ()
checkAt _ [] = pure ()
checkAt v ((d,expected):xs) = do
  fork $ do
    now <- queryNow
    wait (d-now)
    val <- readEVar v
    io $ val @?= expected
  checkAt v xs

tc :: TestName -> (forall s. Scene s ()) -> TestTree
tc name action = testCase name $
  (evalScene action @?= ())
  `catch` \(ErrorCall err) -> assertFailure err

io :: IO a -> Scene s a
io action = liftST (unsafeIOToST action)
