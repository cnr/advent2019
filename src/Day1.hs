module Main (main) where

import Common (input)

main :: IO ()
main = do
  values <- map read . lines <$> input 1

  let part1 :: Int -> Int
      part1 = fuelRequired

      part2 :: Int -> Int
      part2 = fuelRecursive

      solve :: (Int -> Int) -> Int
      solve f = sum (map f values)

  print (solve part1)
  print (solve part2)

fuelRequired :: Int -> Int
fuelRequired = subtract 2 . (`div` 3)

-- fuel requires fuel, until it doesn't.
fuelRecursive :: Int -> Int
fuelRecursive = sum . takeWhile (> 0) . drop 1 . iterate fuelRequired
