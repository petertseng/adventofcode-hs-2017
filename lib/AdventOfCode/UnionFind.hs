{-# LANGUAGE FlexibleContexts #-}

module AdventOfCode.UnionFind (numGroupsAndFirst) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Traversable (for)

numGroupsAndFirst :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
numGroupsAndFirst bounds@(minId, maxId) pairs = runST $ do
  parent <- newListArray bounds [minId .. maxId] :: ST s (STUArray s Int Int)
  size   <- newArray bounds 1 :: ST s (STUArray s Int Int)

  let find x = do
        parX <- readArray parent x
        if x /= parX
          then do
            parParX <- find parX
            writeArray parent x parParX
            return parParX
          else return x

  let union x y = do
        parX <- find x
        parY <- find y
        when (parX /= parY) $ do
          sizeX <- readArray size parX
          sizeY <- readArray size parY
          case compare sizeX sizeY of
            LT -> do
              writeArray parent parX parY
              writeArray size parY (sizeX + sizeY)
            _ -> do
              writeArray parent parY parX
              writeArray size parX (sizeX + sizeY)
        return (parX /= parY)

  successfulUnions <- for pairs (uncurry union)
  let nGroups = (maxId - minId + 1) - length (filter id successfulUnions)

  parentOfMin <- find minId
  szMin <- readArray size parentOfMin

  return (nGroups, szMin)
