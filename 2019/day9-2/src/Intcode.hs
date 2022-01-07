-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Intcode where

import Data.Function
import Data.List
import Data.List.Split (splitOn)
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import Debug.Trace (traceShow)

type Vec = V.Vector Integer

parseInstruction :: Integer -> (Integer, Int, Int, Int)
parseInstruction i =
    ( i `rem` 100
    , fromIntegral  ((i `div` 100) `mod` 10)
    , fromIntegral  ((i `div` 1000) `mod` 10)
    , fromIntegral  ((i `div` 10000) `mod` 10)
    )

run :: [Integer] -> [Integer] -> Integer -> Integer -> Vec -> [Integer]
run input output b i vec
  | op == 99 = reverse output
  | op == 1 = run input output b (i+4) (vec // [(fromIntegral imm3, v1 + v2)])
  | op == 2 = run input output b (i+4) (vec // [(fromIntegral imm3, v1 * v2)])
  | op == 3 = run (tail input) output b (i+2) (vec // [(fromIntegral imm1, head input)])
  | op == 4 = run input (v1:output) b (i+2) vec
  | op == 5 = run input output b (if v1 /= 0 then v2 else i+3) vec
  | op == 6 = run input output b (if v1 == 0 then v2 else i+3) vec
  | op == 7 = run input output b (i+4) (vec // [(fromIntegral imm3, if v1 < v2 then 1 else 0)])
  | op == 8 = run input output b (i+4) (vec // [(fromIntegral imm3, if v1 == v2 then 1 else 0)])
  | op == 9 = run input output (b+v1) (i+2) vec
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

execute :: [Integer] -> Vec -> [Integer]
execute input vec = run input [] 0 0 (vec <> V.replicate 10000 0)

parse :: String -> Vec
parse s = s & splitOn "," & map read & V.fromList

parseAndExecute :: String -> [Integer] -> [Integer]
parseAndExecute s input = parse s & execute input