-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import qualified Data.Map as M
import Data.Function
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)

type Planet = String
type Orbits = M.Map Planet Planet

countTransitiveParents :: Orbits -> String -> Int
countTransitiveParents orbits planet =
  case M.lookup planet orbits of
    Nothing -> 1
    Just parent -> 1 + countTransitiveParents orbits parent

checksum :: Orbits -> Int
checksum orbits = M.elems orbits
  & map (countTransitiveParents orbits)
  & sum

parse :: String -> Orbits
parse str = str
  & lines
  & map (splitOn ")")
  & map toEntry
  & M.fromList
 where
  toEntry [v,k] = (k,v)

main :: IO ()
main = do
  str <- getContents
  print (str & parse & checksum)