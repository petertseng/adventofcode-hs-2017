import AdventOfCode (readInputFile)
import AdventOfCode.UnionFind (numGroupsAndFirst)

parse :: String -> [(Int, Int)]
parse s = case words (filter (/= ',') s) of
  (l:"<->":rs) -> [(read l, read r) | r <- rs]
  _ -> error ("bad " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let pairs = concatMap parse (lines s)
      minId = minimum (map (uncurry min) pairs)
      maxId = maximum (map (uncurry max) pairs)
      (groups, sz0) = numGroupsAndFirst (minId, maxId) pairs
  print sz0
  print groups
