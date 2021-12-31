import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.List (dropWhileEnd)

dist :: (Int, Int) -> Int
-- actually still works on my input if I comment out this first case...
dist (y, x) | signum y /= signum x = abs y + abs x
dist (y, x) = max (abs y) (abs x)

dir :: String -> (Int, Int)
dir "ne" = (-1, 0)
dir "sw" = (1, 0)
dir "nw" = (0, -1)
dir "se" = (0, 1)
dir "n" = (-1, -1)
dir "s" = (1, 1)
dir s = error ("bad dir " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let dirs = map dir (splitOn ',' (dropWhileEnd (== '\n') s))
      poses = scanl (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) dirs
      dists = map dist poses
  print (last dists)
  print (maximum dists)
