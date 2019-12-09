module Main (main) where

import           Common (inputSingle)
import           IntCode (parseProgram, runProgram)

main :: IO ()
main = do
  prog <- parseProgram <$> inputSingle 5

  print (runProgram prog [1])
  print (runProgram prog [5])
