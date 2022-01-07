-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import qualified Data.Set as S

main :: IO ()
main = do
  entries <- map read . lines <$> readFile "input"
  print (process entries)

process :: [Int] -> Int
process [] = error "empty list"
process xs = result $ head $ filter hasMatch xs
    where
        set = S.fromList xs
        hasMatch n = (2020-n) `S.member` set
        result n = n * (2020-n)