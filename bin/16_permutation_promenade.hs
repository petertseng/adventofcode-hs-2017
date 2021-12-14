import AdventOfCode (readInputFileAndFlags)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (dropWhileEnd, foldl')
import Data.Map (Map)
import qualified Data.Map as Map

type Perm = (IntMap Int, Map Char Char)

apply :: Perm -> IntMap Char -> IntMap Char
apply (pos, char) = IntMap.mapKeys (pos IntMap.!) . IntMap.map (char Map.!)

-- Note difference from day 06:
-- add a -> b for translating into a key
-- (not strictly necessary since IntMap Char are Ord, but it's fine)
-- This one returns period start (t') instead of end (t)
repeatTime :: Ord b => (a -> a) -> (a -> b) -> a -> (Int, Int)
repeatTime f h = step 0 Map.empty
  where step t seen s = case Map.insertLookupWithKey (\_ a _ -> a) (h s) t seen of
          (Nothing, seen') -> step (t + 1) seen' (f s)
          (Just t', _) -> (t', t - t')

idPerm :: Int -> Perm
idPerm sz = (pos, char)
  where pos = IntMap.fromAscList [(i, i) | i <- take sz [0..]]
        char = Map.fromAscList [(c, c) | c <- take sz ['a'..]]

perm :: Int -> Perm -> String -> Perm
perm sz (pos, char) s = case s of
  's':xs -> (rotate sz (read xs) pos, char)
  'x':xs -> let (a, b) = splitOnOne '/' xs in (exchange (read a) (read b) pos, char)
  'p':a:'/':[b] -> (pos, partner a b char)
  _ -> error ("bad perm " ++ s)

rotate :: Int -> Int -> IntMap Int -> IntMap Int
rotate sz n = IntMap.map (\x -> (x + n) `rem` sz)

exchange :: Int -> Int -> IntMap Int -> IntMap Int
exchange a b = IntMap.map (swap a b)

partner :: Char -> Char -> Map Char Char -> Map Char Char
partner a b = Map.map (swap a b)

swap :: Eq a => a -> a -> a -> a
swap a b x | x == a = b | x == b = a | otherwise = x

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let nprogs = maybe 16 read (lookup 'n' flags)
      perms = foldl' (perm nprogs) (idPerm nprogs) (splitOn ',' (dropWhileEnd (== '\n') s))
      pos = IntMap.fromAscList (take nprogs (zip [0..] ['a'..]))
      orders = iterate (apply perms) pos
  putStrLn (IntMap.elems (orders !! 1))

  let (start, period) = repeatTime (apply perms) IntMap.elems pos
  putStrLn (IntMap.elems (orders !! (start + ((1000 * 1000 * 1000 - start) `rem` period))))
