-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Data.List.Split (splitOn)
main :: IO ()
main = do
  txt <- getContents
  print (process (parse txt))

process :: [(Int, Int, Char, String)] -> Int
process entries = length (filter valid entries)

valid :: (Int, Int, Char, String) -> Bool
valid (min, max, char, str) = charCount >= min && charCount <= max
 where
  charCount = length (filter (==char) str)

parse :: String -> [(Int, Int, Char, String)]
parse txt = map parseLine (lines txt)

parseLine :: String -> (Int, Int, Char, String)
parseLine l =
    let [prefix, char:":", str] = words l
    in let [minStr, maxStr] = splitOn "-" prefix
    in (read minStr, read maxStr, char, str)