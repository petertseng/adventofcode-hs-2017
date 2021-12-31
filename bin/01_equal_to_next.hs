import AdventOfCode (readInputFile)

import Data.Char (digitToInt)
import Data.List (dropWhileEnd)

rot :: [a] -> [a]
rot [] = []
rot (x:xs) = xs ++ [x]

main :: IO ()
main = do
  s <- readInputFile
  let ds = map digitToInt (dropWhileEnd (== '\n') s)
  print (sum (map fst (filter (uncurry (==)) (zip ds (rot ds)))))
  print (2 * sum (map fst (filter (uncurry (==)) (zip ds (drop (length ds `quot` 2) ds)))))
