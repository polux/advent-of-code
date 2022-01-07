-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Intcode where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Function
import Data.List.Split (splitOn)
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import qualified Data.Map as M
import Debug.Trace (traceShow, traceShowId)

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

run :: Integer -> Integer -> Vec -> Machine
run b i vec
  | op == 99 = return ()
  | op == 1 = run b (i+4) (vec // [(fromIntegral imm3, v1 + v2)])
  | op == 2 = run b (i+4) (vec // [(fromIntegral imm3, v1 * v2)])
  | op == 3 = do
     input <- readInput
     run b (i+2) (vec // [(fromIntegral imm1, input)])
  | op == 4 = do
     writeOutput v1
     run b (i+2) vec
  | op == 5 = run b (if v1 /= 0 then v2 else i+3) vec
  | op == 6 = run b (if v1 == 0 then v2 else i+3) vec
  | op == 7 = run b (i+4) (vec // [(fromIntegral imm3, if v1 < v2 then 1 else 0)])
  | op == 8 = run b (i+4) (vec // [(fromIntegral imm3, if v1 == v2 then 1 else 0)])
  | op == 9 = run (b+v1) (i+2) vec
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

evalList :: [Integer] -> Run a -> [Integer]
evalList is (Done x) = []
evalList [] (ReadInput _) = error "no input to read from"
evalList (i:is) (ReadInput k) = evalList is (k i)
evalList is (WriteOutput o k) = o : evalList is k

execute :: [Integer] -> Vec -> [Integer]
execute input vec = evalList input $ run 0 0 (vec <> V.replicate 10000 0)

type Pos = (Int, Int)

move :: Pos -> Integer -> Pos
move (x,y) 1 = (x,y+1)
move (x,y) 2 = (x,y-1)
move (x,y) 3 = (x-1,y)
move (x,y) 4 = (x+1,y)
move _ n = error ("unknown direction " <> show n)

type Grid = M.Map Pos Integer

explore :: Pos -> Machine -> State Grid ()
explore pos (ReadInput k) =
  [1,2,3,4] `forM_` \direction -> do
    let newPos = move pos direction
    seen <- gets (M.member newPos)
    unless seen $ do
      let (WriteOutput status m) = k direction
      modify (M.insert newPos status)
      unless (status == 0) $
        explore newPos m
explore _ _ = error "unexpected machine state"

discoverGrid :: Vec -> Grid
discoverGrid vec = execState (explore (0,0) machine) (M.singleton (0,0) 1)
 where
  machine = run 0 0 (vec <> V.replicate 10000 0)

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

step :: Grid -> Grid
step grid = foldr insertOxygen grid delta
 where
  delta = concatMap neighbors oxygen
  oxygen = M.filter (==2) grid & M.toList & map fst
  insertOxygen pos = M.insertWith update pos 2
  update new 1 = new
  update _ old = old

findFixPoint :: Eq a => Int -> (a -> a) -> a -> Int
findFixPoint n f x =
  let x' = f x
  in if x == x' then n else findFixPoint (succ n) f x'

countMinutes :: Grid -> Int
countMinutes = findFixPoint 0 step

parse :: String -> Vec
parse s = s & splitOn "," & map read & V.fromList

parseAndExecute :: String -> [Integer] -> [Integer]
parseAndExecute s input = parse s & execute input