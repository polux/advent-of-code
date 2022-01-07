-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE Strict #-}

module Main where

import Data.Monoid
import Data.Function
import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M

data Move = L Int | R Int | U Int | D Int
  deriving (Show)

readMove ('L':cs) = L (read cs)
readMove ('R':cs) = R (read cs)
readMove ('U':cs) = U (read cs)
readMove ('D':cs) = D (read cs)

readMoves = map readMove . splitOn ","

data Pos = Pos Int Int
  deriving (Eq, Ord, Show)

plus (Pos i1 j1) (Pos i2 j2) = Pos (i1+i2) (j1+j2)

data Trace = Trace { minLengths :: M.Map Pos Int, endPos :: Pos, lengthAtEndPos :: Int }
  deriving Show

instance Semigroup Trace where
  (Trace m1 e1 l1) <> (Trace m2 e2 l2) = Trace m e l
    where
      m = M.unionWith min m1 (m2 & M.mapKeysMonotonic (plus e1) & fmap (+l1))
      e = e1 `plus` e2
      l = l1 + l2

instance Monoid Trace where
  mempty = Trace M.empty (Pos 0 0) 0

moveToTrace :: Move -> Trace
moveToTrace (L n) = Trace (M.fromList (zip positions [0..])) (Pos (-n) 0) n
  where positions = [Pos i 0 | i <- [0,-1 .. -n]]
moveToTrace (R n) = Trace (M.fromList (zip positions [0..])) (Pos n 0) n
  where positions = [Pos i 0 | i <- [0 .. n]]
moveToTrace (D n) = Trace (M.fromList (zip positions [0..])) (Pos  0 (-n)) n
  where positions = [Pos 0 j | j <- [0,-1 .. -n]]
moveToTrace (U n) = Trace (M.fromList (zip positions [0..])) (Pos 0 n) n
  where positions = [Pos 0 j | j <- [0 .. n]]

trace :: [Move] -> Trace
trace = foldMap moveToTrace

main :: IO ()
main = do
  [l1, l2] <- lines <$> getContents
  let lengths1 = readMoves l1 & trace & minLengths
  let lengths2 = readMoves l2 & trace & minLengths
  M.intersectionWith (+) lengths1 lengths2
    & M.elems
    & sort
    & tail
    & head
    & print