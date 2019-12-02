module Main (main) where

import           Prelude hiding (read)
import qualified Prelude as P

import           Common (inputSingle)
import           Control.Monad.ST (ST, runST)
import           Data.List.Split (splitOn)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Vector.Mutable (MVector, read, write)

part1Noun :: Int
part1Noun = 12

part1Verb :: Int
part1Verb = 2

part2Expected :: Int
part2Expected = 19690720

main :: IO ()
main = do
  nums <- map P.read . splitOn "," <$> inputSingle 2

  let vec :: Memory
      vec = V.fromList nums

  let part1 = runWithNounVerb part1Noun part1Verb vec

      part2 :: Int
      part2 = head [100 * noun + verb
                     | noun <- [0..99]
                     , verb <- [0..99]
                     , runWithNounVerb noun verb vec == part2Expected
                     ]
  print part1
  print part2

type Memory = Vector Int

runWithNounVerb :: Int -> Int -> Memory -> Int
runWithNounVerb noun verb vec = runST $ do
  thawed <- V.thaw vec
  write thawed 1 noun
  write thawed 2 verb
  exec thawed 0
  read thawed 0

exec :: forall s. MVector s Int -> Int -> ST s ()
exec vec pc = do
  insn <- read vec pc
  case insn of
    1 -> binop (+)
    2 -> binop (*)
    99 -> pure ()
    op -> error ("unexpected opcode: " <> show op)

  where

  binop :: (Int -> Int -> Int) -> ST s ()
  binop f = do
    x <- readPtr (pc+1)
    y <- readPtr (pc+2)
    writePtr (pc+3) (f x y)
    exec vec (pc+4)

  readPtr :: Int -> ST s Int
  readPtr ptr = read vec =<< read vec ptr

  writePtr :: Int -> Int -> ST s ()
  writePtr ptr a = do
    loc <- read vec ptr
    write vec loc a
