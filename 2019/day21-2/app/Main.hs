-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Data.Function
import Intcode
import System.Environment
import qualified Data.Map as M

main :: IO ()
main = do
  prog <- readFile "input.txt"
  prog
    & parse
    & run
    & ascii