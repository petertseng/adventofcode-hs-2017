import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

--import Data.Array.Unboxed ((!), (//), UArray, accumArray, bounds)
import Data.List (delete, partition)
import Data.Maybe (mapMaybe)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- IntMap is faster than array for this because array updates copy the entire array.

-- Each element is (length, strength)
--subchains :: UArray Int Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
subchains :: IntMap Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
subchains _ _ _ _ [] = []
subchains loop left len weight dominoes = concatMap chainsWith eligibles
  where
    chainsWith (d@(a, b), l') = (len', weight') : subchains loop' l' len' weight' (delete d dominoes)
      where
        --nloops = if l' <= snd (bounds loop) then loop ! l' else 0
        nloops = IntMap.findWithDefault 0 l' loop
        weight' = weight + a + b + nloops * 2 * l'
        len' = len + 1 + nloops
        --loop' = if nloops == 0 then loop else loop // [(l', 0)]
        loop' = if nloops == 0 then loop else IntMap.insert l' 0 loop
    eligibles = mapMaybe eligible dominoes
    eligible d@(a, b)
      | a == left = Just (d, b)
      | b == left = Just (d, a)
      | otherwise = Nothing

parse :: String -> (Int, Int)
parse s = let (l, r) = splitOnOne '/' s in (read l, read r)

main :: IO ()
main = do
  s <- readInputFile
  let dominoes = map parse (lines s)
      (loops, nonloops) = partition (uncurry (==)) dominoes
      --maxLoop = maximum (map (uncurry max) loops)
      --loopCount = accumArray (+) 0 (0, maxLoop) [(n, 1) | (n, _) <- loops]
      loopCount = IntMap.fromListWith (+) [(n, 1) | (n, _) <- loops]
      chains = subchains loopCount 0 0 0 nonloops
      maxStr = maximum . map snd
  print (maxStr chains)

  let maxLen = maximum (map fst chains)
      maxChains = filter ((== maxLen) . fst) chains
  print (maxStr maxChains)
