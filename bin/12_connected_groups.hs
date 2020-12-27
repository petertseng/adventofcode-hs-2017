{-# LANGUAGE FlexibleContexts #-}

import AdventOfCode (readInputFile)

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.MArray (newArray, newListArray, readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.Array.Unboxed ((!), UArray, elems)
import Data.Containers.ListUtils (nubInt)

-- Not really the most general implementation,
-- but in Advent of Code 2015-2020 we've only needed to build the list of pairs in advance,
-- never needing an online update.
-- So, we'll run with this for now.
-- I'm only returning the parents array because it's convenient.
groupPairs :: [(Int, Int)] -> UArray Int Int
groupPairs pairs = runSTUArray $ do
  let minId = minimum (map (uncurry min) pairs)
      maxId = maximum (map (uncurry max) pairs)
  parent <- newListArray (minId, maxId) [0 ..] :: ST s (STUArray s Int Int)
  rank   <- newArray (minId, maxId) 0 :: ST s (STUArray s Int Int)

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
          rankX <- readArray rank parX
          rankY <- readArray rank parY
          case compare rankX rankY of
            LT -> writeArray parent parX parY
            GT -> writeArray parent parY parX
            EQ -> do
              writeArray parent parY parX
              writeArray rank parX (rankX + 1)

  mapM_ (uncurry union) pairs
  -- well this is a little weird that we need to do this now,
  -- but if we don't, then elements of the returned array won't be their representative.
  mapM_ find [minId .. maxId]

  return parent

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

parse :: String -> [(Int, Int)]
parse s = [(read l, read r) | r <- rs]
  where l:"<->":rs = words (filter (/= ',') s)

main :: IO ()
main = do
  s <- readInputFile
  let pairs = concatMap parse (lines s)
      parent = groupPairs pairs
  print (count (== parent ! 0) (elems parent))
  print (length (nubInt (elems parent)))
