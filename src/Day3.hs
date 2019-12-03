module Main (main) where

import           Common (input)
import qualified Data.Map as M
import           Data.List.Split (splitOn)

main :: IO ()
main = do
  [one, two] <- map (splitOn ",") . lines <$> input 3
  let one'                   = buildSegments one
      two'                   = buildSegments two
      -- adding together step values for part 2
      intersecting           = M.delete (0,0) $ M.intersectionWith (+) one' two'
      -- part 1
      closest                = minimum . map (\(y,x) -> abs y + abs x) $ M.keys intersecting
      -- part 2
      firstIntersectionSteps = minimum $ M.elems intersecting

  print closest
  print firstIntersectionSteps

buildSegments :: [String] -> M.Map (Int,Int) Int
buildSegments =
  -- fromList is left-biased, so it'll only include the first time we visit each point
  M.fromList
  -- add step values for map insertion
  . (`zip` [0..])
  -- combine points visited into a single list
  . concat
  -- build list of lists of points visited
  . scanl (\locs segment -> parseSegment segment (last locs)) [(0,0)]

-- parse a segment and produce the list of points visited, in the order they're visited
parseSegment :: String -> (Int,Int) -> [(Int,Int)]
parseSegment [] _ = error "expecting non-empty segment string"
parseSegment (a:as) (x,y) =
  let len = read as
   in case a of
     'U' -> [(x,y') | y' <- [y+1..y+len]]
     'D' -> [(x,y') | y' <- [(y-1),(y-2)..y-len]]
     'L' -> [(x',y) | x' <- [(x-1),(x-2)..x-len]]
     'R' -> [(x',y) | x' <- [x+1..x+len]]
     c   -> error ("expecting one of UDLR. found: " <> show c)
