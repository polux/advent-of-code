-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  txt <- getContents
  print (process (parse txt))

process :: [(Int, Int, Char, String)] -> Int
process entries = length (filter valid entries)

valid :: (Int, Int, Char, String) -> Bool
valid (min, max, char, str) =
  ( (if str !! (min -1) == char then 1 else 0)
      + (if str !! (max -1) == char then 1 else 0)
  )
    == 1

parse :: String -> [(Int, Int, Char, String)]
parse txt = map parseLine (lines txt)

parseLine :: String -> (Int, Int, Char, String)
parseLine l =
  let [[_, minStr, maxStr, char, str]] = (l =~ "([0-9]+)-([0-9]+) (.): (.*)")
   in (read minStr, read maxStr, head char, str)