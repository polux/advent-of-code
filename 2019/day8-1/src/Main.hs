-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import qualified Data.Map as M
import Data.Char
import Data.Function
import Data.List.Split (splitOn, chunksOf)
import Data.Sort (sortOn)
import Debug.Trace (traceShowId)

width = 25
height = 6

count :: Int -> [Int] -> Int
count n ns = filter (== n) ns & length

main :: IO ()
main = do
  str <- getContents
  let chunk = str
        & filter isDigit
        & map digitToInt
        & chunksOf (width * height)
        & sortOn (count 0)
        & head
  print (count 1 chunk * count 2 chunk)