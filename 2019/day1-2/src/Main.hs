-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where
import Data.Function

fuel mass = mass `div` 3 - 2
additionalFuel mass = sum $ takeWhile (>0) (iterate fuel mass)
totalFuel mass = additionalFuel (fuel mass)
totalFuelSum masses = sum (map totalFuel masses)

main :: IO ()
main = do
  s <- getContents
  print (s & lines & map read & totalFuelSum)