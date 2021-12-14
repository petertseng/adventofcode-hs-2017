{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- Experimenting with conditional compilation...
-- makes the file a little cluttered,
-- but no worse than just commenting all the other code I guess.

import AdventOfCode (readInputFile)

-- MArray version decent enough (0.4 seconds)
-- Map version pretty slow (8 seconds)
#define usemarray

#ifdef usemarray
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
#else
import Data.List (unfoldr)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
#endif

#ifdef usemarray
steps :: (Int -> Int) -> [Int] -> Int
steps f instl = runST $ do
  let len = length instl
  inst <- newListArray (0, len - 1) instl :: ST s (STUArray s Int Int)

  let run i pc = if 0 > pc || pc >= len then return i else do
        val <- readArray inst pc
        writeArray inst pc (f val)
        run (i + 1) (pc + val)

  run 0 0
#else
step :: (Int -> Int) -> (IntMap Int, Int) -> Maybe ((), (IntMap Int, Int))
step f (inst, pc) = fmap j (IntMap.lookup pc inst)
  where j i = ((), (IntMap.insert pc (f i) inst, pc + i))
#endif

main :: IO ()
main = do
  s <- readInputFile
#ifdef usemarray
  let inst = map read (lines s)
  print (steps succ inst)
  print (steps (\x -> if x >= 3 then x - 1 else x + 1) inst)
#else
  let inst = IntMap.fromList (zip [0..] (map read (lines s)))
      steps f = length (unfoldr (step f) (inst, 0))
  print (steps succ)
  print (steps (\x -> if x >= 3 then x - 1 else x + 1))
#endif
