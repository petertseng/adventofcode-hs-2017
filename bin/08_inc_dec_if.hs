import AdventOfCode (readInputFile)

import Data.List (mapAccumL)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Instruction = (String, Int, String, Int -> Bool)

runInst :: Map String Int -> Instruction -> (Map String Int, Int)
runInst regs (dst, incdec, src, test) = if testResult then (regs', v) else (regs, 0)
  where testResult = test (Map.findWithDefault 0 src regs)
        (prev, regs') = Map.insertLookupWithKey (const (+)) dst incdec regs
        v = incdec + fromMaybe 0 prev

inst :: String -> Instruction
inst s = case words s of
  [dst, incdec, incdecn, "if", src, tst, tstn] -> (dst, incdecval incdec * read incdecn, src, tstf tst (read tstn))
    where incdecval "inc" = 1
          incdecval "dec" = -1
          incdecval _ = error ("bad incdec " ++ s)
          tstf "==" = (==)
          tstf "!=" = (/=)
          tstf ">" = (<)
          tstf ">=" = (<=)
          tstf "<" = (>)
          tstf "<=" = (>=)
          tstf _ = error ("bad tst " ++ s)
  _ -> error ("bad inst " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let insts = map inst (lines s)
      (regs, vals) = mapAccumL runInst Map.empty insts
  print (maximum (Map.elems regs))
  print (maximum vals)
