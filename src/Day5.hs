{-# language DataKinds #-}
{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}

module Main (main) where

import           Prelude hiding (read)
import qualified Prelude as P

import           Common (inputSingle)
import           Data.List (unfoldr)
import           Data.List.Split (splitOn)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Polysemy (Members, Sem, reinterpret, makeSem, run)
import           Polysemy.Input (Input(..), input)
import           Polysemy.Output (Output, output, runOutputList)
import           Polysemy.State (evalState, get, modify, put)

data Memory m a where
  ReadAddr  :: Int -> Memory m Int
  WriteAddr :: Int -> Int -> Memory m ()

makeSem ''Memory

main :: IO ()
main = do
  prog <- map P.read . splitOn "," <$> inputSingle 5

  print (runProgram prog [1])
  print (runProgram prog [5])

runProgram :: [Int] -> [Int] -> [Int]
runProgram prog inp = run
  . fmap fst
  . runOutputList @Int
  . runFiniteInput @Int inp
  $ runMemory (V.fromList prog) exec

exec :: forall r. Members '[Memory, Input Int, Output Int] r => Sem r ()
exec = go 0
  where
  go :: Int -> Sem r ()
  go pc = do
    insn <- readAddr pc
    let (rawModes,opcode) = insn `divMod` 100
        modes = unfoldr (\x -> let (d,m) = x `divMod` 10 in Just (m,d)) rawModes

        doInput :: Sem r ()
        doInput = do
          x <- raw 0
          i <- input
          writeAddr x i
          go (pc+2)

        doOutput :: Sem r ()
        doOutput = do
          output =<< arg 0
          go (pc+2)

        jumpWhen :: (Int -> Bool) -> Sem r ()
        jumpWhen p = do
          val  <- arg 0
          addr <- arg 1
          if p val
            then go addr
            else go (pc+3)

        binop :: (Int -> Int -> Int) -> Sem r ()
        binop f = do
          x   <- arg 0
          y   <- arg 1
          out <- raw 2
          writeAddr out (f x y)
          go (pc+4)

        cmp :: (Int -> Int -> Bool) -> Sem r ()
        cmp p = binop (\x y -> if p x y then 1 else 0)

        -- get the nth argument, dereferencing the pointer when necessary
        arg :: Int -> Sem r Int
        arg n =
          case modes !! n of
            0 -> readAddr =<< readAddr (pc+n+1)
            1 -> readAddr (pc+n+1)
            m -> error ("unexpected mode: " <> show m)

        -- get the nth argument
        raw :: Int -> Sem r Int
        raw n = readAddr (pc+n+1)

    case opcode of
      1  -> binop (+)
      2  -> binop (*)
      3  -> doInput
      4  -> doOutput
      5  -> jumpWhen (/= 0)
      6  -> jumpWhen (== 0)
      7  -> cmp (<)
      8  -> cmp (==)
      99 -> pure ()
      op -> error ("unexpected opcode: " <> show op)

runMemory :: Vector Int -> Sem (Memory ': r) a -> Sem r a
runMemory vec = evalState vec . reinterpret (\case
  ReadAddr n -> (V.! n) <$> get
  WriteAddr n a -> modify (V.// [(n,a)]))

runFiniteInput :: [i] -> Sem (Input i ': r) a -> Sem r a
runFiniteInput inp = evalState inp . reinterpret (\case
  Input -> get >>= \case
    (x:xs) -> x <$ put xs
    _      -> error "empty input")