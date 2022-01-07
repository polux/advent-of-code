-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Data.Function
import Data.List.Split (splitOn)
import Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import Debug.Trace (trace)

type Vec = V.Vector Int

run :: Int -> Vec -> Int
run i vec
  | op == 99 = vec ! 0
  | op == 1 = run (i+4) (vec // [(out, v1 + v2)])
  | op == 2 = run (i+4) (vec // [(out, v1 * v2)])
  | otherwise = error ("unknown op code " ++ show op)
 where
  op = vec ! i
  v1 = vec ! (vec ! (i+1))
  v2 = vec ! (vec ! (i+2))
  out = vec ! (i+3)

execute :: Int -> Int -> Vec -> Int
execute noun verb vec = run 0 (vec // [(1, noun), (2, verb)])

solve :: Vec -> (Int, Int)
solve vec = head [(noun, verb) | noun <- [0..99], verb <- [0..99], execute noun verb vec == 19690720]

pack :: (Int, Int) -> Int
pack (noun, verb) = 100 * noun + verb

main :: IO ()
main = do
  s <- getContents
  print (s & splitOn "," & map read & V.fromList & solve & pack)