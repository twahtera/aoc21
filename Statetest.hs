{-# LANGUAGE RankNTypes #-}

module Statetest where
import Data.Map.Strict
import Control.Monad.ST
import Data.STRef

import Control.Monad.State

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) +  fib (n-1)

fibMem' :: Int -> State (Map Int Int) Int
fibMem' 0 = pure 0
fibMem' 1 = pure 1
fibMem' n = do
  m <- get

  case m !? n of
    Just v -> pure v
    Nothing -> do
      v1 <- fibMem' (n-1)
      v2 <- fibMem' (n-2)
      put $ insert n (v1+v2) m

      pure $ v1 + v2

fibMem :: Int -> Int
fibMem n = evalState (fibMem' n) empty

fibF :: Monad f =>  (Int -> f Int) -> Int -> f Int
fibF f 0 = pure 0
fibF f 1 = pure 1
--fibF f n = (+) <$> f (n-1) <*> f (n-2)
fibF f n = do
  f1 <- f (n-1)
  f2 <- f (n-2)
  pure $ f1 + f2

memo :: Ord a => (forall s. (a -> ST s b) -> a -> ST s b) -> a -> b
memo f n = runST $ do
  mRef <- newSTRef empty
  let go n' = do
        hm <- readSTRef mRef
        case hm !? n' of
          Just rv -> return rv
          Nothing -> do
            rv <- f go n'
            hm' <- readSTRef mRef
            writeSTRef mRef $  insert n' rv hm'
            return rv
  go n

memoFib :: Int -> Int
memoFib = memo fibF
