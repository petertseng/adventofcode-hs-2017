import AdventOfCode (readInputFile)

import Data.Array ((!), Array, bounds, inRange, listArray)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data Value = Immediate Int | Register Char
data Command = Send Value
             | Set Char Value
             | Add Char Value
             | Mul Char Value
             | Mod Char Value
             | Receive Char
             | JumpGTZero Value Value

data ReceiveMode = LastSentIfNonzero | FromOtherComputer

-- We cannot directly do a knot-tying with [Int] -> [Int],
-- since then we don't have deadlock dectection.
-- Instead, run will return a list of outputs plus a status.
-- The waiting status will allow continuing if more inputs come in.
data Status = Term | Recovered | Waiting ([Int] -> ([Int], Status))

run :: ReceiveMode -> Array Int Command -> Int -> [Int] -> ([Int], Status)
run recv program compID = run' (Map.singleton 'p' compID) 1 []
  where
    run' :: Map Char Int -> Int -> [Int] -> [Int] -> ([Int], Status)
    run' _ pc tx _ | not (bounds program `inRange` pc) = (tx, Term)
    run' regs pc tx rx = case program ! pc of
      Send val    -> run' regs (pc + 1) (resolve regs val : tx) rx
      Set reg val -> run' (Map.insert reg (resolve regs val) regs) (pc + 1) tx rx
      Add reg val -> run' (binop (+) regs reg val) (pc + 1) tx rx
      Mul reg val -> run' (binop (*) regs reg val) (pc + 1) tx rx
      Mod reg val -> run' (binop mod regs reg val) (pc + 1) tx rx
      Receive reg -> case (recv, rx) of
        (LastSentIfNonzero, _)    -> if resolve regs (Register reg) /= 0 then (tx, Recovered) else run' regs (pc + 1) tx rx
        (FromOtherComputer, [])   -> (tx, Waiting (run' regs pc []))
        (FromOtherComputer, x:xs) -> run' (Map.insert reg x regs) (pc + 1) tx xs
      JumpGTZero test offset -> run' regs (pc + if resolve regs test > 0 then resolve regs offset else 1) tx rx

run2 :: Array Int Command -> Int
run2 program = run' 0 (run FromOtherComputer program 0 []) (run FromOtherComputer program 1 [])
  where
    run' :: Int -> ([Int], Status) -> ([Int], Status) -> Int
    -- both are waiting and both have no outputs: deadlocked
    run' n ([], Waiting _)  ([], Waiting _)  = n
    -- both are waiting and one or the other has outputs: continue with those outputs
    run' n (o1, Waiting k1) (o2, Waiting k2) = run' (n + length o2) (k1 (reverse o2)) (k2 (reverse o1))
    -- 1 is termed, count up its remaining outputs
    run' n (_, _)           (o2, Term)       = n + length o2
    -- 0 is termed (and 1 is not), send 0's remaining outputs to 1 if there are any
    run' n ([], Term)       (o2, Waiting _)  = n + length o2
    run' n (o1, Term)       (o2, Waiting k2) = run' (n + length o2) ([], Term) (k2 (reverse o1))
    run' _ _                (_, Recovered)   = error "wrong rcv mode"
    run' _ (_, Recovered)   _                = error "wrong rcv mode"

binop :: (Int -> Int -> Int) -> Map Char Int -> Char -> Value -> Map Char Int
binop op regs reg val = Map.alter (\x -> Just (fromMaybe 0 x `op` resolve regs val)) reg regs

resolve :: Map Char Int -> Value -> Int
resolve _    (Immediate i) = i
resolve regs (Register r)  = fromMaybe 0 (Map.lookup r regs)

parseValue :: String -> Value
parseValue [c] | 'a' <= c && c <= 'z' = Register c
parseValue s = maybe (error ("bad value " ++ s)) Immediate (readMaybe s)

parseCommand :: [String] -> Command
parseCommand ["snd", v]      = Send (parseValue v)
parseCommand ["set", [c], v] = Set c (parseValue v)
parseCommand ["add", [c], v] = Add c (parseValue v)
parseCommand ["mul", [c], v] = Mul c (parseValue v)
parseCommand ["mod", [c], v] = Mod c (parseValue v)
parseCommand ["rcv", [c]]    = Receive c
parseCommand ["jgz", v1, v2] = JumpGTZero (parseValue v1) (parseValue v2)
parseCommand s = error ("bad command: " ++ unwords s)

main :: IO ()
main = do
  s <- readInputFile
  let program = listArray (1, length (lines s)) (map (parseCommand . words) (lines s))
  case run LastSentIfNonzero program 0 [] of
    (_, Term)        -> putStrLn "finished"
    (x:_, Recovered) -> print x
    (_, Recovered)   -> putStrLn "no output"
    (_, Waiting _)   -> error "wrong rcv mode"

  print (run2 program)
