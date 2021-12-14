import AdventOfCode (readInputFile)

import Data.List (dropWhileEnd, find)
import Data.Maybe (fromJust)

type Scanner = (Int, Int)

severity :: Scanner -> Int
severity s@(a, b) | catch 0 s = a * b
severity _ = 0

catch :: Int -> Scanner -> Bool
catch t (a, b) = (a + t) `rem` ((b - 1) * 2) == 0

scanner :: String -> Scanner
scanner s = case words s of
  [a, b] -> (read (dropWhileEnd (== ':') a), read b)
  _ -> error ("bad scanner " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let scanners = map scanner (lines s)
  print (sum (map severity scanners))

  let noCatch t = not (any (catch t) scanners)
  print (fromJust (find noCatch [0..]))
