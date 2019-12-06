module Main (main) where

import           Common (input)
import           Data.List ((\\))
import           Data.List.Split (splitOn)
import qualified Data.Map as M
import           Data.Tuple (swap)

main :: IO ()
main = do
  orbits <- M.fromList . map (swap . tuple2 . splitOn ")") . lines <$> input 6

  let part1 = sum [length (walk orbits k) | k <- M.keys orbits]
      part2 = length ((walk orbits "YOU" \\ walk orbits "SAN")
                   ++ (walk orbits "SAN" \\ walk orbits "YOU")) - 2

  print part1
  print part2

tuple2 :: [a] -> (a,a)
tuple2 [x,y] = (x,y)
tuple2 _ = error "expecting two elements"

walk :: M.Map String String -> String -> [String]
walk m = takeWhile (/= "COM") . iterate (m M.!)
