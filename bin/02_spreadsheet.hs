import AdventOfCode (readInputFile)

maxMinusMin :: [Int] -> Int
maxMinusMin xs = maximum xs - minimum xs

divide :: [Int] -> Int
divide xs = case filter ((== 0) . snd) [x `quotRem` y | x <- xs, y <- xs, x /= y] of
  [(v, _)] -> v
  [] -> error "no divisible pair"
  (_:_:_) -> error "too many divisible pair"

main :: IO ()
main = do
  s <- readInputFile
  let rows = map (map read . words) (lines s)
  print (sum (map maxMinusMin rows))
  print (sum (map divide rows))
