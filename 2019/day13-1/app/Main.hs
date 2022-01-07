-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Data.Function
import Intcode
import System.Environment
import qualified Data.Map as M

main :: IO ()
main = do
  prog <- parse <$> getContents
  prog
    & execute []
    & toScreen
    & M.filter (==2)
    & length
    & print