-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Data.List (sortBy)
import Data.List.Split (chunksOf, splitOn)
import Data.Ord (Down (..), comparing)

type Input = [[Int]]

type Output = Int

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map (map read) . splitOn [""] . lines

solve :: Input -> Output
solve = sum . take 3 . sortBy (comparing Down) . map sum
