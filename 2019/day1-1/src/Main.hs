-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where
import Data.Function

fuel mass = mass `div` 3 - 2
totalFuel masses = sum (map fuel masses)

main :: IO ()
main = do
  s <- getContents
  print (s & lines & map read & totalFuel)