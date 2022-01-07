-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Intcode where

import Control.Monad.Reader
import Data.Function
import Data.List (sortOn, intercalate)
import Data.List.Split (splitOn)
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import qualified Data.Map as M
import Debug.Trace (traceShow, traceShowId)
import Data.BoolSimplifier

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


evalList :: [Integer] -> Machine -> [Integer]
evalList is (Done x) = []
evalList [] (ReadInput _) = error "no input to read from"
evalList (i:is) (ReadInput k) = evalList is (k i)
evalList is (WriteOutput o k) = o : evalList is k

type Pos = (Integer, Integer)
type Screen = M.Map Pos Integer

toScreen :: [Integer] -> Screen
toScreen (x:y:t:os) = M.insert (x,y) t (toScreen os)
toScreen _ = M.empty

parse :: String -> Vec
parse s = s & splitOn "," & map read & V.fromList

parseAndExecute :: String -> [Integer] -> [Integer]
parseAndExecute s input = parse s & run & evalList input

ascii :: Machine -> IO ()
ascii (ReadInput k) = do
  c <- getChar
  ascii (k (fromIntegral (fromEnum c)))
ascii (WriteOutput n k) = do
  if n > 127
    then print n
    else putChar (toEnum (fromIntegral n))
  ascii k
ascii (Done _) = return ()

-- simulation

situations :: [[Bool]]
situations = replicateM 9 [True, False]

canSurvive :: [Bool] -> Bool
canSurvive [] = True
canSurvive (False:_) = False
canSurvive (True:xs) = canSurvive xs || canSurvive (drop 3 xs)

mustJump :: [Bool] -> Bool
mustJump xs = not (canSurvive xs) && canSurvive (drop 3 xs)

canJump :: [Bool] -> Bool
canJump xs = canSurvive (drop 3 xs)

pretty :: [Bool] -> String
pretty = map (\b -> if b then '#' else '.')

disjunction :: [[Bool]] -> String
disjunction cs = cs & map conjunction & map (parens . intercalate "&") & intercalate "|"
 where
  conjunction as = zipWith render ['a'..'z'] as
  render letter True = [letter]
  render letter False = parens ('~':[letter])
  parens s = "(" ++ s ++ ")"

toQueryRep :: [[Bool]] -> QueryRep QOrTyp (Ion String)
toQueryRep cs = cs & map (zipWith toAtom "ABCDEFGHI") & map andqs & orqs
 where
   toAtom c True = qAtom [c]
   toAtom c False = qNot (qAtom [c])

mustJumpExpr =
  situations
   & filter mustJump -- keep only situations where we would survive without jumping
   & disjunction


{-

simplifying mustJumpExpr via espresso returns

Or(And(~a, d, h), And(~a, d, e, i), And(~a, d, e, f), And(~c, d, ~f, h, ~i), And(~c, d, ~e, ~f, h), And(~b, d, ~e, h), And(~b, d, ~f, h, ~i))

(~1, 4, 8)
(~1, 4, 5, 9)
(~1, 4, 5, 6)
= ~1 and (4 and (8 or (5 and (9 or 6))))

(~3, 4, ~6, 8, ~9)
(~3, 4, ~6, 8, ~5)
= ~3 and (4 and (~6 and (8 and (~9 or ~5))))

(~2, 4, 8, ~5)
(~2, 4, 8, ~6, ~9)
= ~2 and (4 and (8 and (~5 or (~6 and ~9))))

if ~1 then we need to jump no matter what, so let simplify the first block to ~1

~1
(~2, 4, 8, (~5 or (~6 and ~9)))
(~3, 4, ~6 , 8, (~9 or ~5))

rearange:

~1
(4, 8, ~2, (~5 or (~6 and ~9)))
(4, 8, ~3, ~6, (~9 or ~5))

factorise:

~1
(4, 8, ((~2, (~5 or (~6 and ~9))) or (~3, ~6, (~9 or ~5)))

This is too long, let's try to truncate to:

~1 or (4 and (8 and (~2 or ~3)))

which gives

NOT C J
NOT B T
OR T J
AND H J
AND D J
NOT A T
OR T J
RUN

running the machine on this program succeeds with 1141262756

-}