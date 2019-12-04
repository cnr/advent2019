module Main (main) where

import Data.List (groupBy)

inputStart :: [Int]
inputStart = [3,7,2,0,3,7]

inputEnd :: [Int]
inputEnd = [9,0,5,1,5,7]

main :: IO ()
main = do
  print (solve part1)
  print (solve part2)

solve :: ([Int] -> Bool) -> Int
solve predicate = length [() | xs <- increasingSequences
                             , xs > inputStart
                             , xs < inputEnd
                             , predicate xs]

part1 :: [Int] -> Bool
part1 xs = or (zipWith (==) xs (tail xs))

part2 :: [Int] -> Bool
part2 = any ((== 2) . length) . groupBy (==)

increasingSequences :: [[Int]]
increasingSequences = go 6 1
  where
  -- `go n x` builds a list of `n` digits with increasing values. `x` is used
  -- as the minimum bound for the first digit `y`, `y` is used as the
  -- minimum bound for the next digit, ...
  go :: Int -- number of digits remaining
     -> Int -- current minimum bound. e.g., if we've built [3,4] so far, minimum bound is 4
     -> [[Int]]
  go 0 _ = [[]]
  go n x = [(y:ys) | y <- [x..9], ys <- go (n-1) y]
