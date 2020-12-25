import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.Char (isDigit)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Program = (String, Int, [String])

bottoms :: [Program] -> [String]
bottoms programs = Set.toList ((Set.difference `on` Set.fromList) parents (concat children))
  where (parents, _, children) = unzip3 programs

weightChange :: [Program] -> String -> Int
weightChange programs = weightChange' (error "balanced")
  where waaa = weightAtAndAbove programs
        weightChange' shouldBe cand = let children = waaa Map.! cand in
          maybe (shouldBe - sum (map snd children)) (uncurry weightChange') (unbal children)

-- Unbalanced child and the weight it should be
unbal :: [(String, Int)] -> Maybe (Int, String)
-- Exactly three elements and all equal: nothing is unbalanced
unbal [(_, w1), (_, w2), (_, w3)] | w1 == w2 && w1 == w3 = Nothing
unbal (x1@(n1, w1) : x2@(n2, w2) : (n3, w3) : xs)
  -- Three equal elements plus more: discard one and keep looking
  | w1 == w2 && w1 == w3 = unbal (x1:x2:xs)
  -- Three elements, not all equal: the odd one out is it.
  | w1 == w2 = Just (w1, n3)
  | w2 == w3 = Just (w2, n1)
  | w3 == w1 = Just (w3, n2)
unbal xs = error ("don't know which is unbalanced among " ++ show xs)

weightAtAndAbove :: [Program] -> Map String [(String, Int)]
weightAtAndAbove programs = waaaRecMap
  where weights = Map.fromList [(program, weight) | (program, weight, _) <- programs]
        waaaRecMap = Map.fromList [(parent, map selfAndAbove children) | (parent, _, children) <- programs]
        selfAndAbove c = (c, weights Map.! c + sum (map snd (waaaRecMap Map.! c)))

one :: [a] -> a
one []    = error "none for one"
one [x]   = x
one (_:_) = error "too many for one"

parse :: String -> Program
parse s = (name, weight, children)
  where (l, r) = splitOnOne '>' s
        name = takeWhile (/= ' ') l
        weight = read (filter isDigit (words l !! 1))
        children = map (dropWhile (== ' ')) (splitOn ',' r)

main :: IO ()
main = do
  s <- readInputFile
  let programs = map parse (lines s)
      bottom = one (bottoms programs)
  putStrLn bottom
  print (weightChange programs bottom)
