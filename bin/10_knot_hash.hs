import AdventOfCode (readInputFileAndFlags)
import AdventOfCode.KnotHash (hash, twist)
--import AdventOfCode.KnotHash (hashSeq, twist)
import AdventOfCode.Split (splitOn)

import Control.Monad (unless)
import Data.List (dropWhileEnd)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = do
  (s, f) <- readInputFileAndFlags
  unless (any ((== '2') . fst) f) $ do
    let lengths = mapMaybe readMaybe (splitOn ',' s)
    case twist lengths of
      a:b:_ -> print (a * b)
      xs -> error ("bad twist " ++ show xs)
  let hex = concatMap (printf "%02x")
  putStrLn (hex (hash (dropWhileEnd (== '\n') s)))
  --putStrLn (hex (hashSeq (dropWhileEnd (== '\n') s)))
