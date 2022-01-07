-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Data.Function
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Move = L Int | R Int | U Int | D Int
  deriving (Show)

readMove ('L':cs) = L (read cs)
readMove ('R':cs) = R (read cs)
readMove ('U':cs) = U (read cs)
readMove ('D':cs) = D (read cs)

readMoves = map readMove . splitOn ","

type Pos = (Int, Int)

move :: Pos -> Move -> (S.Set Pos, Pos)
move (i,j) (L n) = (S.fromList [(k, j) | k <- [i,i-1..i-n]], (i-n, j))
move (i,j) (R n) = (S.fromList [(k, j) | k <- [i..i+n]], (i+n, j))
move (i,j) (D n) = (S.fromList [(i, k) | k <- [j,j-1..j-n]], (i, j-n))
move (i,j) (U n) = (S.fromList [(i, k) | k <- [j..j+n]], (i, j+n))

path :: Pos -> [Move] -> S.Set Pos
path pos [] = S.empty
path pos (m:ms) =
  let (trace, newPos) = move pos m
  in trace `S.union` path newPos ms

distance (i,j) = abs i + abs j

main :: IO ()
main = do
  [l1, l2] <- lines <$> getContents
  let path1 = readMoves l1 & path (0,0)
  let path2 = readMoves l2 & path (0,0)
  (path1 `S.intersection` path2)
    & S.toList
    & map distance
    & sort
    & tail
    & head
    & print