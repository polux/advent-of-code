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

overlay = zipWith combine
 where
  combine 2 n = n
  combine n _ = n

intToPixel 0 = '█'
intToPixel _ = ' '

main :: IO ()
main = do
  str <- getContents
  str
    & filter isDigit
    & map digitToInt
    & chunksOf (width * height)
    & foldr1 overlay
    & map intToPixel
    & chunksOf width
    & unlines
    & putStrLn
