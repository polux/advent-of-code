-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  entries <- map read . lines <$> readFile "input"
  print (process entries)

preprocess :: [Int] -> M.Map Int (Int, Int)
preprocess xs = M.fromList $ map mkEntry $ [(x,y) | x <- xs, y <- xs]
  where
    mkEntry (x, y) = (x+y, (x, y))


process :: [Int] -> Int
process [] = error "empty list"
process xs = result $ head $ mapMaybe findMatch xs
    where
        sums = preprocess xs
        findMatch n = (n,) <$> M.lookup (2020-n) sums
        result (n, (x,y)) = x*y*n
