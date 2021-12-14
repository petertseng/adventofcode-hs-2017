import Data.Char (isDigit)
import Data.List (find, foldl', unfoldr)
import Data.Maybe (fromJust)
import Data.Sequence ((><), (<|), Seq)
import qualified Data.Sequence as Seq
import System.Environment (getArgs)

-- seq: leftmost element is current.
rotateIns :: Int -> Seq Int -> Int -> Seq Int
rotateIns rot s n = (n <| r) >< l
  where n' = (rot + 1) `rem` Seq.length s
        (l, r) = Seq.splitAt n' s

--(num after 0, list length, position)
step :: Int -> (Int, Int, Int) -> (Int, Int, Int)
-- strict on after0, otherwise OOM
step rot (after0, len, pos) = after0 `seq` (if pos' == 0 then len else after0, len + 1, pos' + 1)
  where pos' = (pos + rot) `rem` len

main :: IO ()
main = do
  args <- getArgs
  n <- case args of
    [] -> fmap read (readFile "/dev/stdin")
    a:_ | all isDigit a -> return (read a)
    a:_ -> fmap read (readFile a)

  let s = foldl' (rotateIns n) (Seq.singleton 0) [1 .. 2017]
  print (Seq.index s 1)

  let after0 = unfoldr ((\t@(a, b, _) -> Just ((b, a), t)) . step n) (1, 2, 1)
  print (snd (fromJust (find ((>= 50000000) . fst) after0)))
