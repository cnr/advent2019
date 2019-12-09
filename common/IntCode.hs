{-# language DataKinds #-}
{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}

module IntCode
  ( Program(..)
  , parseProgram
  , runProgram
  ) where

import           Prelude hiding (read)
import qualified Prelude as P

import           Data.List (unfoldr)
import           Data.List.Split (splitOn)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Polysemy (Members, Sem, reinterpret, reinterpret2, makeSem, run)
import           Polysemy.Input (Input(..), input)
import           Polysemy.Output (Output, output, runOutputList)
import           Polysemy.State (evalState, evalLazyState, get, modify, put)

data Memory m a where
  ReadAddr           :: Int -> Memory m Integer
  WriteAddr          :: Int -> Integer -> Memory m ()
  ReadRelativeBase   :: Memory m Int
  ModifyRelativeBase :: Int -> Memory m ()

makeSem ''Memory

newtype Program = Program { unProgram :: [Integer] }

parseProgram :: String -> Program
parseProgram = Program . (++ replicate 2048 0) . map P.read . splitOn ","

-- the interpreter order is important here, for reasons I can't entirely reason about.
-- changing the order of any of these breaks the laziness in day 7, even when using
-- `evalLazyState` for all of the interpreters.
runProgram :: Program -> [Integer] -> [Integer]
runProgram (Program prog) inp = run
  . fmap fst
  . runFiniteInput @Integer inp
  . runMemory (V.fromList prog)
  $ runOutputList @Integer exec

exec :: forall r. Members '[Memory, Input Integer, Output Integer] r => Sem r ()
exec = go 0
  where
  go :: Int -> Sem r ()
  go pc = do
    insn <- readAddr pc
    let (rawModes,opcode) = insn `divMod` 100
        modes = unfoldr (\x -> let (d,m) = x `divMod` 10 in Just (m,d)) rawModes

        doInput :: Sem r ()
        doInput = do
          i <- input
          writeArg 0 i
          go (pc+2)

        doOutput :: Sem r ()
        doOutput = do
          output =<< arg 0
          go (pc+2)

        jumpWhen :: (Integer -> Bool) -> Sem r ()
        jumpWhen p = do
          val  <- arg 0
          addr <- arg 1
          if p val
            then go (fromIntegral addr)
            else go (pc+3)

        binop :: (Integer -> Integer -> Integer) -> Sem r ()
        binop f = do
          x   <- arg 0
          y   <- arg 1
          writeArg 2 (f x y)
          go (pc+4)

        cmp :: (Integer -> Integer -> Bool) -> Sem r ()
        cmp p = binop (\x y -> if p x y then 1 else 0)

        doSetRel :: Sem r ()
        doSetRel = do
          offset <- arg 0
          modifyRelativeBase (fromIntegral offset)
          go (pc+2)

        -- get the nth argument, dereferencing the pointer when necessary
        arg :: Int -> Sem r Integer
        arg n =
          case modes !! n of
            0 -> readAddr . fromIntegral =<< readAddr (pc+n+1)
            1 -> readAddr (pc+n+1)
            2 -> do
              relBase <- readRelativeBase
              offset  <- raw n
              fromIntegral <$> readAddr (relBase + fromIntegral offset)
            m -> error ("unexpected arg mode: " <> show m)

        writeArg :: Int -> Integer -> Sem r ()
        writeArg n val =
          case modes !! fromIntegral n of
            0 -> do
              addr <- fromIntegral <$> raw n
              writeAddr addr val
            1 -> error "can't write to immediate value"
            2 -> do
              relBase <- readRelativeBase
              offset  <- raw n
              writeAddr (relBase + fromIntegral offset) val
            m -> error ("unexpected writeArg mode: " <> show m)

        -- get the nth argument
        raw :: Int -> Sem r Integer
        raw n = fromIntegral <$> readAddr (pc+n+1)

    case opcode of
      1  -> binop (+)
      2  -> binop (*)
      3  -> doInput
      4  -> doOutput
      5  -> jumpWhen (/= 0)
      6  -> jumpWhen (== 0)
      7  -> cmp (<)
      8  -> cmp (==)
      9  -> doSetRel
      99 -> pure ()
      op -> error ("unexpected opcode: " <> show op)

runMemory :: Vector Integer -> Sem (Memory ': r) a -> Sem r a
runMemory vec = evalState @Int 0 . evalState vec . reinterpret2 (\case
  ReadAddr n -> (V.! n) <$> get
  WriteAddr n a -> modify (V.// [(n,a)])
  ReadRelativeBase -> get @Int
  ModifyRelativeBase n -> modify @Int (+n))

-- We need lazy state for day 7: with lazy input, we can recursively pipe outputs
-- to inputs
runFiniteInput :: [i] -> Sem (Input i ': r) a -> Sem r a
runFiniteInput inp = evalLazyState inp . reinterpret (\case
  Input -> get >>= \case
    (x:xs) -> x <$ put xs
    _      -> error "empty input")
