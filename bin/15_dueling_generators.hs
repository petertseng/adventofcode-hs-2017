{-# LANGUAGE NumericUnderscores #-}

import Data.Bits ((.&.))
import Data.Function (on)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

generator :: Int -> Int -> [Int]
generator factor seed = drop 1 (iterate (step factor) seed)

{- HLINT ignore step "Use underscore" -}
step :: Int -> Int -> Int
step factor prev = (prev * factor) `rem` 2147483647

{- HLINT ignore gena "Use underscore" -}
gena :: Int -> [Int]
gena = generator 16807

{- HLINT ignore genb "Use underscore" -}
genb :: Int -> [Int]
genb = generator 48271

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

findNumbers :: String -> (Int, Int)
findNumbers s = case mapMaybe readMaybe (words s) of
  [a, b] -> (a, b)
  _ -> error ("not two numbers in " ++ s)

main :: IO ()
main = do
  args <- getArgs
  (a0, b0) <- case args of
    a:b:_ -> return (read a, read b)
    f:_ -> fmap findNumbers (readFile f)
    _ -> fmap findNumbers (readFile "/dev/stdin")
  let eq16 = zipWith ((==) `on` (.&. 0xffff))
  print (count id (take 40_000_000 (eq16 (gena a0) (genb b0))))

  let mask m = filter ((== 0) . (.&. m))
  -- Weird lesson learned while doing this day.
  -- If I try to give gena a0 a name and reuse it later on,
  -- memory will actually be allocated for each element,
  -- and OOM will quite quickly happen.
  -- Try uncommenting the lines referring to streama.
  -- If the take 1 remains commented, it still works fine and doesn't allocate.
  -- But as soon as the take 1 is uncommented, not even the earlier use of streama completes!
  -- So allocation is happening because of the mere presence of a later reuse.
  --let streama = gena a0
  --print (count id (take 40_000_000 (eq16 streama (genb b0))))
  --print (take 1 (mask 0x3 streama))

  print (count id (take 5_000_000 (eq16 (mask 0x3 (gena a0)) (mask 0x7 (genb b0)))))
