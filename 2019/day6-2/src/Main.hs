-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)

type Planet = String
type Orbits = M.Map Planet Planet

ancestors :: Orbits -> Planet -> [Planet]
ancestors orbits planet =
  case M.lookup planet orbits of
    Nothing -> []
    Just parent -> planet : ancestors orbits parent

stripCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
stripCommonPrefix (x:xs) (y:ys) | x == y = stripCommonPrefix xs ys
stripCommonPrefix xs ys = (xs, ys)

minLength :: Orbits -> Planet -> Planet -> Int
minLength orbits p1 p2 = length tail1 + length tail2 - 2
 where
  (tail1, tail2) = stripCommonPrefix (reverse (ancestors orbits p1))
                                     (reverse (ancestors orbits p2))

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
  print (str & parse & minLength `flip` "YOU" `flip` "SAN")