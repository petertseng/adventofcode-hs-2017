import AdventOfCode (readInputFile)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (maximumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)

repeatTime :: Ord a => (a -> a) -> a -> (Int, Int)
repeatTime f = step 0 Map.empty
  where step t seen s = case Map.insertLookupWithKey (\_ a _ -> a) s t seen of
          (Nothing, seen') -> step (t + 1) seen' (f s)
          (Just t', _) -> (t, t - t')

mancala :: IntMap Int -> IntMap Int
mancala m = IntMap.mapWithKey redist m
  where (i, val) = maximumBy (comparing (\(a, b) -> (b, -a))) (IntMap.assocs m)
        redist k v = (if k == i then 0 else v) + val `quot` sz + (if (k - (i + 1)) `mod` sz < val `rem` sz then 1 else 0)
        sz = IntMap.size m

main :: IO ()
main = do
  s <- readInputFile
  let banks = IntMap.fromAscList (zip [0..] (map read (words s)))
      (t, len) = repeatTime mancala banks
  print t
  print len
