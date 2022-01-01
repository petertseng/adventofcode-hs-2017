import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Monad (foldM_)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (getElems, newArray, readArray, writeArray)
import Data.Array.Unboxed ((!), UArray, listArray)
import Data.Array.ST (STUArray)
import Data.Bits ((.&.), shiftR, testBit)
import Data.Char (ord)

ones :: UArray Int Int -> Char -> Int -> Int
ones transition state0 steps = runST $ do
  let pad = min steps 20000
  tape <- newArray (-pad, pad) False :: ST s (STUArray s Int Bool)

  foldM_ (\(state, pos) () -> do
      cur <- readArray tape pos
      let trans = transition ! (state * 2 + if cur then 1 else 0)
      writeArray tape pos (trans `testBit` 0)
      return (trans `shiftR` 2, pos - 1 + (trans .&. 2))
    ) (ord state0 - ord 'A', 0) (replicate steps ())

  es <- getElems tape
  return (count id es)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

encode :: (Bool, Int, Char) -> Int
-- write: false -> 0, true -> 1
-- dir: -1 -> 0, 1 -> 2
encode (write, d, next) = (ord next - ord 'A') * 4 + d + 1 + if write then 1 else 0

machine :: [String] -> Char -> [(Bool, Int, Char)]
machine s c = case map words s of
  [ ["In", "state", c':":"]
    , ["If", "the", "current", "value", "is", "0:"]
    , ["-", "Write", "the", "value", writez:"."]
    , ["-", "Move", "one", "slot", "to", "the", dirz]
    , ["-", "Continue", "with", "state", nextz:"."]
    , ["If", "the", "current", "value", "is", "1:"]
    , ["-", "Write", "the", "value", writeo:"."]
    , ["-", "Move", "one", "slot", "to", "the", diro]
    , ["-", "Continue", "with", "state", nexto:"."]
    ] | c == c' -> [(bool writez, dir dirz, nextz), (bool writeo, dir diro, nexto)]
  _ -> error ("bad machine " ++ show (map words s))

bool :: Char -> Bool
bool '0' = False
bool '1' = True
bool c = error (c : " not a bool")

dir :: String -> Int
dir "right." = 1
dir "left." = -1
dir s = error ("bad dir " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let (state, steps, states) = case splitOn "" (lines s) of
        [_] -> error "no machines"
        [] -> error "no blueprint"
        [a, b]:sts -> case (words a, words b) of
          (["Begin", "in", "state", st:"."], ["Perform", "a", "diagnostic", "checksum", "after", n, "steps."]) -> (st, read n, zipWith machine sts ['A'..])
          _ -> error "bad state/steps"
        _:_ -> error "bad state/steps"
      transition = listArray (0, 2 * length states - 1) (map encode (concat states))
  print (ones transition state steps)
