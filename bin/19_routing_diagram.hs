import AdventOfCode (readInputFile)

import Data.Array.Unboxed ((!?), UArray, listArray)
import Data.Char (isLetter)
import Data.List (elemIndices, unfoldr)

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Eq, Ord, Show)
type Packet = (Pos, Dir)

move :: UArray Pos Char -> Packet -> Maybe (Char, Packet)
move g ((y, x), d@(Dir (dy, dx))) = c >>= (\c' -> if c' /= ' ' then Just (c', (p', d')) else Nothing)
  where p' = (y + dy, x + dx)
        c = g !? p'
        d' = if c == Just '+' then turn g p' d else d

turn :: UArray Pos Char -> Pos -> Dir -> Dir
turn g (y, x) (Dir (dy, dx)) = case filter ok candidates of
  [d] -> d
  [] -> error ("nowhere to turn @ " ++ show (y, x))
  cs -> error ("too many choices to turn @ " ++ show (y, x) ++ " has " ++ show cs)
  where candidates = [Dir (-dx, dy), Dir (dx, -dy)]
        ok (Dir (dy', dx')) = maybe False (/= ' ') (g !? (y + dy', x + dx'))

grid :: String -> UArray Pos Char
grid s = listArray ((0, 0), (h - 1, w - 1)) (concat ls)
  where ls = lines s
        h = length ls
        w = uniform length ls

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

main :: IO ()
main = do
  s <- readInputFile
  let g = grid s
      pos = case elemIndices '|' (head (lines s)) of
        [x] -> (0, x)
        xs -> error ("bad " ++ show xs)
      chars = unfoldr (move g) (pos, Dir (1, 0))
  putStrLn (filter isLetter chars)
  -- unfoldr doesn't include the initial point
  print (length chars + 1)
