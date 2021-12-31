import AdventOfCode.KnotHash (hash)
import AdventOfCode.UnionFind (numGroupsAndFirst)

import Data.Bits (popCount, testBit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

conns :: Map (Int, Int) Int -> ((Int, Int), Int) -> [(Int, Int)]
conns ids ((y, x), i) = [(i, j) | (dy, dx) <- [(1, 0), (0, 1)], j <- maybeToList (Map.lookup (y + dy, x + dx) ids)]

gridBits :: [[Int]] -> [(Int, Int)]
gridBits = concatMap rowBits . zip [0..]

rowBits :: (Int, [Int]) -> [(Int, Int)]
rowBits (y, xs) = concatMap bits (zip [0..] xs)
  where bits (i, x) = [(y, i * 8 + j) | j <- [0 .. 7], x `testBit` (7 - j)]

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  let row n = hash (input ++ ('-' : show n))
      rows = map row [0 .. 127 :: Int]
      numBits = sum (map (sum . map popCount) rows)
  print numBits

  let bits = zip (gridBits rows) [0..]
      ids = Map.fromList bits
      pairs = concatMap (conns ids) bits
  print (fst (numGroupsAndFirst (0, numBits - 1) pairs))
