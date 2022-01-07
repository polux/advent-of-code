-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Intcode
import System.Environment

main :: IO ()
main = do
  s <- getContents
  print (parseAndExecute s [5])