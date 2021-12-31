import AdventOfCode (readInputFile)

import Control.Monad (foldM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Foldable (for_)
import Data.STRef (newSTRef, modifySTRef', readSTRef)

{-
import Data.Bits (shiftL)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (iterate')

data Status = Clean | Weakened | Infected | Flagged deriving (Eq)
-}
type Dir = (Int, Int)

-- arrays were much faster than IntMap/IntSet for this,
-- by a factor of about 10x.

step1Arr :: Int -> Int -> [(Int, Int)] -> Int
step1Arr height width startsInfected = runST $ do
  let pad = 200
  infected <- newArray ((-pad, -pad), (height + pad, width + pad)) False :: ST s (STUArray s (Int, Int) Bool)
  infects <- newSTRef 0
  for_ startsInfected (\p -> writeArray infected p True)

  foldM_ (\(pos@(y, x), dir) () -> do
      curInf <- readArray infected pos
      writeArray infected pos (not curInf)
      unless curInf (modifySTRef' infects succ)
      let dir'@(dy, dx) = (if curInf then turnRight else turnLeft) dir
      return ((y + dy, x + dx), dir')
    ) ((height `div` 2, width `quot` 2), (-1, 0)) (replicate 10000 ())

  readSTRef infects

step2Arr :: Int -> Int -> [(Int, Int)] -> Int
step2Arr height width startsInfected = runST $ do
  let pad = 350
  stat <- newArray ((-pad, -pad), (height + pad, width + pad)) 0 :: ST s (STUArray s (Int, Int) Int)
  infects <- newSTRef 0
  for_ startsInfected (\p -> writeArray stat p 2)

  foldM_ (\(pos@(y, x), dir) () -> do
      curStat <- readArray stat pos
      writeArray stat pos (if curStat == 3 then 0 else curStat + 1)
      when (curStat == 1) (modifySTRef' infects succ)
      let dir'@(dy, dx) = turnFor curStat dir
      return ((y + dy, x + dx), dir')
    ) ((height `div` 2, width `quot` 2), (-1, 0)) (replicate 10000000 ())

  readSTRef infects

turnLeft :: Dir -> Dir
turnLeft (dy, dx) = (-dx, dy)

turnRight :: Dir -> Dir
turnRight (dy, dx) = (dx, -dy)

turnAround :: Dir -> Dir
turnAround (dy, dx) = (-dy, -dx)

turnFor :: Int -> Dir -> Dir
turnFor 0 = turnLeft
turnFor 1 = id
turnFor 2 = turnRight
turnFor 3 = turnAround
turnFor _ =  error "bad turn"

{-
bigWidth :: Int
bigWidth = 1 `shiftL` 30

origin :: Int
origin = (1 `shiftL` 29) * bigWidth + (1 `shiftL` 29)

step1 :: (Int, Dir, IntSet, Int) -> (Int, Dir, IntSet, Int)
step1 (pos, dir, inf, infects) = (pos + dy * bigWidth + dx, dir', toggle pos inf, infects + if curInf then 0 else 1)
  where dir'@(dy, dx) = (if curInf then turnRight else turnLeft) dir
        curInf = pos `IntSet.member` inf

toggle :: Int -> IntSet -> IntSet
toggle x xs = (if x `IntSet.member` xs then IntSet.delete else IntSet.insert) x xs

step2 :: (Int, Dir, IntMap Status, Int) -> (Int, Dir, IntMap Status, Int)
step2 (pos, dir, stat, infects) = infects `seq` (pos + dy * bigWidth + dx, dir', stat', infects + if curStat == Weakened then 1 else 0)
  where dir'@(dy, dx) = turnFor curStat dir
        curStat = IntMap.findWithDefault Clean pos stat
        stat' = IntMap.insert pos (nextStatus curStat) stat

turnFor :: Status -> Dir -> Dir
turnFor Clean = turnLeft
turnFor Weakened = id
turnFor Infected = turnRight
turnFor Flagged = turnAround

nextStatus :: Status -> Status
nextStatus Clean = Weakened
nextStatus Weakened = Infected
nextStatus Infected = Flagged
nextStatus Flagged = Clean
-}

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

enumGrid :: [[a]] -> [((Int, Int), a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

main :: IO ()
main = do
  s <- readInputFile
  let g = lines s
      height = length g
      width = uniform length g
      infecteds = filter ((== '#') . snd) (enumGrid g)
      poses = map fst infecteds
      --posesMod = map (\(y, x) -> origin + y * bigWidth + x) poses
      --start = origin + (height `div` 2) * bigWidth + (width `div` 2)
      --(_, _, _, ninf) = iterate step1 (start, (-1, 0), IntSet.fromAscList posesMod, 0) !! 10000
      ninf = step1Arr height width poses
  print ninf

  let ninf2 = step2Arr height width poses
  --let (_, _, _, ninf2) = iterate' step2 (start, (-1, 0), IntMap.fromAscList (map (, Infected) posesMod), 0) !! 10000000
  print ninf2
