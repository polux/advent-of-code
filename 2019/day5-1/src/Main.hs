-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Data.Function
import Data.List.Split (splitOn)
import Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import Debug.Trace (trace)

type Vec = V.Vector Int

run :: [Int] -> [Int] -> Int -> Vec -> [Int]
run input output i vec
  | op == 99 = reverse output
  | op == 1 = run input output (i+4) (vec // [(i3, v1 + v2)])
  | op == 2 = run input output (i+4) (vec // [(i3, v1 * v2)])
  | op == 3 = run (tail input) output (i+2) (vec // [(i1, head input)])
  | op == 4 = run input (v1:output) (i+2) vec
  | otherwise = error ("unknown op code " ++ show op)
 where
  op = i0 `rem` 100
  v1 = if (i0 `div` 100) `mod` 10 == 1 then i1 else p1
  v2 = if (i0 `div` 1000) `mod` 10 == 1 then i2 else p2
  v3 = if (i0 `div` 10000) `mod` 10 == 1 then i3 else p3
  i0 = vec ! i
  i1 = vec ! (i+1)
  i2 = vec ! (i+2)
  i3 = vec ! (i+3)
  p1 = vec ! i1
  p2 = vec ! i2
  p3 = vec ! i3

execute :: [Int] -> Vec -> [Int]
execute input = run input [] 0

main :: IO ()
main = do
  s <- getContents
  print (s & splitOn "," & map read & V.fromList & execute [1])