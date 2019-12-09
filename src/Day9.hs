module Main (main) where

import Common (inputSingle)
import IntCode

main :: IO ()
main = do
  prog <- parseProgram <$> inputSingle 9
  print (runProgram prog [1])
  print (runProgram prog [2])
