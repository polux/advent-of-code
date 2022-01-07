-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE Strict #-}

module Main where

import Data.Function

low = 158126
high = 624574

digits :: Int -> [Int]
digits = map (read . (:[])) . show

increasing :: [Int] -> Bool
increasing digits = and (zipWith (<=) digits (tail digits))

hasAdjacentDuplicates :: [Int] -> Bool
hasAdjacentDuplicates digits = or (zipWith (==) digits (tail digits))

isCandidate :: [Int] -> Bool
isCandidate digits = increasing digits && hasAdjacentDuplicates digits

main :: IO ()
main =
  [low .. high]
    & map digits
    & filter isCandidate
    & length
    & print
