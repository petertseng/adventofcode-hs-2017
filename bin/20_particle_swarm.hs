import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Data.Function (on)
import Data.List (groupBy, minimumBy, sortOn)
import Data.Ord (comparing)

type V3 = (Int, Int, Int)
type Particle = (V3, V3, V3)

posAt :: Int -> Particle -> V3
posAt t ((px, py, pz), (vx, vy, vz), (ax, ay, az)) = (pos px vx ax, pos py vy ay, pos pz vz az)
  where pos p v a = p + v * t + a * t * (t + 1) `quot` 2

distAt :: Int -> Particle -> Int
distAt t = (\(x, y, z) -> abs x + abs y + abs z) . posAt t

step :: ([Particle], Int) -> ([Particle], Int)
step (particles, t) = (collideBy (posAt t) particles, t + 1)

-- too bad, this is almost nubOrdOn,
-- except nubOrdOn would keep one element of each colliding group
collideBy :: Ord b => (a -> b) -> [a] -> [a]
collideBy f = concat . filter len1 . groupBy ((==) `on` f) . sortOn f

len1 :: [a] -> Bool
len1 [] = False
len1 [_] = True
len1 (_:_) = False

particle :: String -> Particle
particle s = (bracketed p, bracketed v, bracketed a)
  where (p, va) = splitOnOne 'v' s
        (v, a) = splitOnOne 'a' va

bracketed :: String -> V3
bracketed s = (read (dropWhile (== ' ') a), read b, read c)
  where (l, _) = splitOnOne '>' s
        (_, between) = splitOnOne '<' l
        (a, bc) = splitOnOne ',' between
        (b, c) = splitOnOne ',' bc

main :: IO ()
main = do
  s <- readInputFile
  let particles = map particle (lines s)
      labeled = zip particles [0 :: Int ..]
  print (snd (minimumBy (comparing (distAt 10000 . fst)) labeled))
  -- TODO: Maybe dynamically check whether particles haven't collided for a while.
  print (length (fst (iterate step (particles, 0) !! 100)))
