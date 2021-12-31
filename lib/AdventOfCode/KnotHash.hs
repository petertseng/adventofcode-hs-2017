module AdventOfCode.KnotHash (hash, hashSeq, twist) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence ((><), Seq)
import qualified Data.Sequence as Seq

-- Didn't know whether Seq Int or [Int] would be faster.
-- Turns out it looks like [Int] is a little bit faster.
-- (The typeclass didn't seem to introduce any extra penalty to either)

class Knottable t where
  rotate :: Int -> t -> t
  reverseFirst :: Int -> t -> t

instance Knottable [a] where
  rotate n xs = drop n' xs ++ take n' xs
    where n' = n `mod` 256
  reverseFirst n xs = reverse (take n xs) ++ drop n xs

instance Knottable (Seq a) where
  rotate n xs = Seq.drop n' xs >< Seq.take n' xs
    where n' = n `mod` 256
  reverseFirst n xs = Seq.reverse (Seq.take n xs) >< Seq.drop n xs

suffix :: [Int]
suffix = [17, 31, 73, 47, 23]

hash :: String -> [Int]
hash s = dense sparse
  where sparse = twist (r64 s)

hashSeq :: String -> [Int]
hashSeq s = dense sparse
  where sparse = twistSeq (r64 s)

dense :: [Int] -> [Int]
dense = map (foldl' xor 0) . every 16

every :: Int -> [Int] -> [[Int]]
every _ [] = []
every n xs = take n xs : every n (drop n xs)

r64 :: String -> [Int]
r64 s = concat (replicate 64 (map ord s ++ suffix))

twist :: [Int] -> [Int]
twist = twistPoly [0 .. 255]

twistSeq :: [Int] -> [Int]
twistSeq = toList . twistPoly (Seq.fromList [0 .. 255])

twistPoly :: (Knottable a) => a -> [Int] -> a
twistPoly xs lengths = rotate deferRot s
  where (s, _, _, deferRot) = foldl' twistPoly' (xs, 0, 0, 0) lengths

twistPoly' :: Knottable a => (a, Int, Int, Int) -> Int -> (a, Int, Int, Int)
twistPoly' (s, pos, skip, deferRot) len = (srev, pos + len + skip, skip + 1, -pos)
  where srot = rotate (pos + deferRot) s
        srev = reverseFirst len srot
