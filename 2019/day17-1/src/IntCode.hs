-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module IntCode (parseAndExecute, parseAndExecuteIO, toAscii) where

import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Monad ( ap, liftM, (>=>) )
import Data.Vector ((//), (!))
import Data.List.Split (splitOn)
import Data.Function ((&))

type Vec = V.Vector Integer

parseInstruction :: Integer -> (Integer, Int, Int, Int)
parseInstruction i =
    ( i `rem` 100
    , fromIntegral  ((i `div` 100) `mod` 10)
    , fromIntegral  ((i `div` 1000) `mod` 10)
    , fromIntegral  ((i `div` 10000) `mod` 10)
    )

data Run :: * -> * where
  Done :: a -> Run a
  ReadInput :: (Integer -> Run a) -> Run a
  WriteOutput :: Integer -> Run a -> Run a

instance Show a => Show (Run a) where
  show (Done x) = "Done " ++ show x
  show (ReadInput _) = "ReadInput <k>"
  show (WriteOutput n _) = "WriteOutput " ++ show n ++ " <k>"

instance Functor Run where
  fmap = liftM

instance Applicative Run where
  pure = return
  (<*>) = ap

instance Monad Run where
  return = Done
  (Done x) >>= f = f x
  (ReadInput k) >>= f = ReadInput (k >=> f)
  (WriteOutput n k) >>= f = WriteOutput n (k >>= f)

writeOutput :: Integer -> Run ()
writeOutput i = WriteOutput i (Done ())

readInput :: Run Integer
readInput = ReadInput Done

type Machine = Run ()

run :: Vec -> Machine
run vec = run' 0 0 (vec <> V.replicate 10000 0)

run' :: Integer -> Integer -> Vec -> Machine
run' b i vec
  | op == 99 = return ()
  | op == 1 = run' b (i+4) (vec // [(fromIntegral imm3, v1 + v2)])
  | op == 2 = run' b (i+4) (vec // [(fromIntegral imm3, v1 * v2)])
  | op == 3 = do
     input <- readInput
     run' b (i+2) (vec // [(fromIntegral imm1, input)])
  | op == 4 = do
     writeOutput v1
     run' b (i+2) vec
  | op == 5 = run' b (if v1 /= 0 then v2 else i+3) vec
  | op == 6 = run' b (if v1 == 0 then v2 else i+3) vec
  | op == 7 = run' b (i+4) (vec // [(fromIntegral imm3, if v1 < v2 then 1 else 0)])
  | op == 8 = run' b (i+4) (vec // [(fromIntegral imm3, if v1 == v2 then 1 else 0)])
  | op == 9 = run' (b+v1) (i+2) vec
  | otherwise = error ("unknown op code " ++ show op)
 where
  (op, p1, p2, p3) = parseInstruction i0
  v1 | p1 == 1 = i1
     | p1 == 2 = r1 b
     | otherwise = r1 0
  v2 | p2 == 1 = i2
     | p2 == 2 = r2 b
     | otherwise = r2 0
  v3 | p3 == 1 = i3
     | p3 == 2 = r3 b
     | otherwise = r3 0
  i0 = vec ! fromIntegral i
  i1 = vec ! fromIntegral (i+1)
  i2 = vec ! fromIntegral (i+2)
  i3 = vec ! fromIntegral (i+3)
  r1 o = vec ! fromIntegral (i1+o)
  r2 o = vec ! fromIntegral (i2+o)
  r3 o = vec ! fromIntegral (i3+o)
  imm1 | p1 == 0 = i1
       | p1 == 2 = i1 + b
  imm2 | p2 == 0 = i2
       | p2 == 2 = i2 + b
  imm3 | p3 == 0 = i3
       | p3 == 2 = i3 + b

parse :: String -> Vec
parse s = s & splitOn "," & map read & V.fromList

evalList :: [Integer] -> Machine -> [Integer]
evalList is (Done x) = []
evalList [] (ReadInput _) = error "no input to read from"
evalList (i:is) (ReadInput k) = evalList is (k i)
evalList is (WriteOutput o k) = o : evalList is k

parseAndExecute :: String -> [Integer] -> [Integer]
parseAndExecute s input = parse s & run & evalList input

toAscii :: [Integer] -> String
toAscii = map (toEnum . fromIntegral)

evalIO :: Machine -> IO ()
evalIO (ReadInput k) = do
  c <- getChar
  evalIO (k (fromIntegral (fromEnum c)))
evalIO (WriteOutput n k) = do
  if n > 127
    then print n
    else putChar (toEnum (fromIntegral n))
  evalIO k
evalIO (Done _) = return ()

parseAndExecuteIO :: String -> IO ()
parseAndExecuteIO s = parse s & run & evalIO

