{-# LANGUAGE RankNTypes #-}

module Utils where

import qualified Data.Set as Set
import Data.Map.Strict
import Control.Monad.ST
import Data.STRef

import Control.Monad.State

a ∪ b = a `Set.union` b
a ∩ b = a `Set.intersection` b
a ∈ b = a `Set.member` b
a ⊂ b = a `Set.member` b

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
