import Data.Char (isDigit)
import Data.List (mapAccumL, find, unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import System.Environment (getArgs)

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int)

coords :: [Pos]
coords = unfoldr (Just . step) (0, 0, Dir (0, 1), (0, 0))

step :: (Int, Int, Dir, Pos) -> (Pos, (Int, Int, Dir, Pos))
step (orig, len, Dir (dy, dx), (y, x)) = (pos', (orig', len', dir', pos'))
  where pos' = (y + dy, x + dx)
        (orig', len', dir') = if len == 0
           then let o = orig + if dy == 0 then 0 else 1 in (o, o, Dir (dx, -dy))
           else (orig, len - 1, Dir (dy, dx))

surroundVals :: [Int]
surroundVals = snd (mapAccumL surround (Map.singleton (0, 0) 1) coords)

surround :: Map Pos Int -> Pos -> (Map Pos Int, Int)
surround m pos = (Map.insert pos newVal m, newVal)
  where newVal = sum (mapMaybe (`Map.lookup` m) (adj9 pos))

adj9 :: Pos -> [Pos]
adj9 (y, x) = [(y + dy, x + dx) | dy <- [-1, 0, 1], dx <- [-1, 0, 1]]

main :: IO ()
main = do
  args <- getArgs
  n <- case args of
    [] -> fmap read (readFile "/dev/stdin")
    a:_ | all isDigit a -> return (read a)
    a:_ -> fmap read (readFile a)
  let (y, x) = ((0, 0) : coords) !! (n - 1)
  print (abs y + abs x)
  print (fromJust (find (> n) surroundVals))
