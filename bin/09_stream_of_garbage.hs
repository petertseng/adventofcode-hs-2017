import AdventOfCode (readInputFile)

score :: String -> Int
score = score' 0 False
  where
    score' d True  ('!':_:xs) = score' d True xs
    score' d True  ('>':xs)   = score' d False xs
    score' d True  (_:xs)     = score' d True xs
    score' d False ('<':xs)   = score' d True xs
    score' d False ('{':xs)   = score' (d + 1) False xs
    score' d False ('}':xs)   = d + score' (d - 1) False xs
    score' d False (',':xs)   = score' d False xs
    score' _ _     "\n"       = 0
    score' _ _     ""         = 0
    score' _ _     s          = error ("bad " ++ show s)

garbage :: String -> Int
garbage = garbage' False
  where
    garbage' True  ('!':_:xs) = garbage' True xs
    garbage' True  ('>':xs)   = garbage' False xs
    garbage' True  (_:xs)     = 1 + garbage' True xs
    garbage' False ('<':xs)   = garbage' True xs
    garbage' False (_:xs)     = garbage' False xs
    garbage' _     ""         = 0

main :: IO ()
main = do
  s <- readInputFile
  print (score s)
  print (garbage s)
