import AdventOfCode (readInputFile)

import Data.List (sort)
import qualified Data.Set as Set

uniqueBy :: Ord b => (a -> b) -> [a] -> Bool
uniqueBy f xs = Set.size (Set.fromList (map f xs)) == length xs

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let passphrases = map words (lines s)
  print (count (uniqueBy id) passphrases)
  print (count (uniqueBy sort) passphrases)
