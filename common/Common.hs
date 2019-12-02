module Common
  ( input
  , inputSingle
  ) where

input :: Int -> IO String
input n = readFile ("inputs/" <> show n <> ".txt")

inputSingle :: Int -> IO String
inputSingle = fmap (head . lines) . input
