module Main (main) where

import Common (inputSingle)
import Data.List (permutations)
import IntCode (Program, parseProgram, runProgram)

part1 :: [Integer]
part1 = [0..4]

part2 :: [Integer]
part2 = [5..9]

main :: IO ()
main = do
  prog <- parseProgram <$> inputSingle 7

  let solve :: [Integer] -> Integer
      solve phases = maximum [ runPipe prog ps | ps <- permutations phases ]

  print (solve part1)
  print (solve part2)

runPipe :: Program -> [Integer] -> Integer
runPipe prog [p0,p1,p2,p3,p4] = last out4
  where
  out0 = runProgram prog (p0:0:out4)
  out1 = runProgram prog (p1:out0)
  out2 = runProgram prog (p2:out1)
  out3 = runProgram prog (p3:out2)
  out4 = runProgram prog (p4:out3)
runPipe _ _ = error "expecting 5 phases"
