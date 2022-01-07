-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Intcode where

import Data.Function
import Data.List
import Data.List.Split (splitOn)
import Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import Debug.Trace (traceShow)

type Vec = V.Vector Int

parseInstruction :: Int -> (Int, Bool, Bool, Bool)
parseInstruction i =
    ( i `rem` 100
    , (i `div` 100) `mod` 10 == 1
    , (i `div` 1000) `mod` 10 == 1
    , (i `div` 10000) `mod` 10 == 1
    )

run :: [Int] -> [Int] -> Int -> Vec -> [Int]
run input output i vec
--  | traceShow (parseInstruction i0) False = undefined
  | op == 99 = reverse output
  | op == 1 = run input output (i+4) (vec // [(i3, v1 + v2)])
  | op == 2 = run input output (i+4) (vec // [(i3, v1 * v2)])
  | op == 3 = run (tail input) output (i+2) (vec // [(i1, head input)])
  | op == 4 = run input (v1:output) (i+2) vec
  | op == 5 = run input output (if v1 /= 0 then v2 else i+3) vec
  | op == 6 = run input output (if v1 == 0 then v2 else i+3) vec
  | op == 7 = run input output (i+4) (vec // [(i3, if v1 < v2 then 1 else 0)])
  | op == 8 = run input output (i+4) (vec // [(i3, if v1 == v2 then 1 else 0)])
  | otherwise = error ("unknown op code " ++ show op)
 where
  (op, p1, p2, p3) = parseInstruction i0
  v1 = if p1 then i1 else r1
  v2 = if p2 then i2 else r2
  v3 = if p3 then i3 else r3
  i0 = vec ! i
  i1 = vec ! (i+1)
  i2 = vec ! (i+2)
  i3 = vec ! (i+3)
  r1 = vec ! i1
  r2 = vec ! i2
  r3 = vec ! i3

execute :: [Int] -> Vec -> [Int]
execute input = run input [] 0

parseAndExecute :: String -> [Int] -> [Int]
parseAndExecute s input = s & splitOn "," & map read & V.fromList & execute input