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
  | trace (show vec) False = undefined
  | op == 99 = vec ! 0
  | op == 1 = run (i+4) (vec // [(out, v1 + v2)])
  | op == 2 = run (i+4) (vec // [(out, v1 * v2)])
  | otherwise = error ("unknown op code " ++ show op)
 where
  op = vec ! i
  v1 = vec ! (vec ! (i+1))
  v2 = vec ! (vec ! (i+2))
  out = vec ! (i+3)

main :: IO ()
main = do
  s <- getContents
  print (s & splitOn "," & map read & V.fromList & (// [(1,12), (2,2)]) & run 0)