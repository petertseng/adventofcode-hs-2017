import AdventOfCode (readInputFile)

import Data.Array ((!), Array, bounds, inRange, listArray)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map

data Value = Immediate Int | Register Char deriving (Eq, Show)
data Inst = Set Char Value | Sub Char Value | Mul Char Value | JumpNotZero Value Value deriving (Eq, Show)

run :: Array Int Inst -> Int -> (Int, Int)
run insts a = run' (Map.singleton 'a' a) 0 0
  where run' regs pc muls | not (bounds insts `inRange` pc) = (muls, Map.findWithDefault 0 'h' regs)
        run' regs 8 muls | insts ! 8 == Set 'f' (Immediate 1) =
          let b = Map.findWithDefault 0 'b' regs in
          run' (Map.insert 'f' (if prime b then 1 else 0) regs) 24 (muls + (b - 2) * (b - 2))
        run' regs pc muls = muls `seq` case insts ! pc of
          Set x y -> run' (Map.insert x (resolve regs y) regs) (pc + 1) muls
          Sub x y -> run' (Map.insertWith (+) x (-resolve regs y) regs) (pc + 1) muls
          Mul x y -> run' (Map.adjust (* resolve regs y) x regs) (pc + 1) (muls + 1)
          JumpNotZero x y -> run' regs pc' muls
            where pc' = pc + if resolve regs x == 0 then 1 else resolve regs y

prime :: Int -> Bool
prime n = not (any (\d -> n `rem` d == 0) [2 .. isqrt n])

isqrt :: Int -> Int
isqrt = dfloor . sqrt . fromIntegral
  where dfloor :: Double -> Int
        dfloor = floor

resolve :: Map Char Int -> Value -> Int
resolve _ (Immediate i) = i
resolve regs (Register r) = Map.findWithDefault 0 r regs

inst :: String -> Inst
inst s = case words s of
  ["set", [x], y] -> Set x (value y)
  ["sub", [x], y] -> Sub x (value y)
  ["mul", [x], y] -> Mul x (value y)
  ["jnz", x, y] -> JumpNotZero (value x) (value y)
  _ -> error ("bad inst " ++ s)

value :: String -> Value
value ('-':i) | all isDigit i = Immediate (-read i)
value i | all isDigit i = Immediate (read i)
value [c] = Register c
value s = error ("bad value " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let insts = map inst (lines s)
      arr = listArray (0, length insts - 1) insts
  print (fst (run arr 0))
  print (snd (run arr 1))
