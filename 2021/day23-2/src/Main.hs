-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Combinators (none)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, maybeToList)
import Data.MemoTrie
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..), _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

-- #region regex parsing
{-
import Text.Regex.Pcre2 (regex)

parseRegex :: String -> [Int]
parseRegex = map parseLine . T.lines . T.pack
 where
   toInt = read . T.unpack
   parseLine [regex|some example regex: (?<x>\d+)|] = toInt x
-}
-- #endregion

type Input = String

type Output = Input

main :: IO ()
main = print solution

solution = dijkstra neighbors (==winningGrid) startingGrid


{-
             1
   01234567890
  #############
0 #...........#
1 ###D#D#C#B###
2   #B#A#A#C#
    #########

-}

data Letter = A | B | C | D
  deriving (Eq, Show, Ord)
type Grid = Map (V2 Int) Letter

room :: Letter -> Int
room A = 2
room B = 4
room C = 6
room D = 8

startingGrid :: Grid
startingGrid = M.fromList (zip [V2 x y | x <- [2, 4, 6, 8], y <- [1, 2, 3, 4]] [D,D,D,B, D,C,B,A, C,B,A,A, B,A,C,C])

winningGrid :: Grid
winningGrid = M.fromList [(V2 (room c) y, c) | c <- [A, B, C, D], y <- [1, 2, 3, 4]]

availableRoomSpot :: Grid -> Letter -> Maybe (V2 Int)
availableRoomSpot grid c =
  if all (==c) (catMaybes [grid M.!? V2 (room c) y | y <- [1..4]])
    then maximumMay [pos | y <- [1..4], let pos = V2 (room c) y, pos `M.notMember` grid]
    else Nothing

isRoomDone :: Grid -> Letter -> Bool
isRoomDone grid c = all (==Just c) [grid M.!? V2 (room c) y | y <- [1..4]]

range :: Int -> Int -> [Int]
range x1 x2
  | x1 <= x2 = [x1 .. x2]
  | otherwise = [x1, x1 -1 .. x2]

path :: V2 Int -> V2 Int -> [V2 Int]
path (V2 x1 0) (V2 x2 y2) = [V2 x 0 | x <- tail (range x1 x2)] ++ [V2 x2 y | y <- [1 .. y2]]
path (V2 x1 y1) (V2 x2 0) = [V2 x1 y | y <- tail (range y1 1)] ++ [V2 x 0 | x <- range x1 x2]
path _ _ = error "invalid path"

isPathFree :: Grid -> V2 Int -> V2 Int -> Bool
isPathFree grid source dest = none (`M.member` grid) (path source dest)

hallwaySpots :: [V2 Int]
hallwaySpots = [V2 x 0 | x <- [0,1,3,5,7,9,10]]

distance p1 p2 = sum (fmap abs (p2 - p1))

cost :: Letter -> Int
cost A = 1
cost B = 10
cost C = 100
cost D = 1000

neighbors :: Grid -> [(Grid, Int)]
neighbors grid = concatMap neighborsFor (M.toList grid)
 where
  neighborsFor (pos, c) =
    [ (grid & M.delete pos & M.insert dest c, cost c * distance pos dest)
    | dest <- candidateDestinations pos c
    , isPathFree grid pos dest
    ]
  candidateDestinations _ c | isRoomDone grid c = [] -- optim
  candidateDestinations (V2 x 0) c = maybeToList (availableRoomSpot grid c)
  candidateDestinations pos _ = hallwaySpots

display :: Grid -> String
display grid = unlines [
  "#############",
  "#" ++ concat [at (V2 x 0) | x <- [0..10]] ++ "#",
  "###" ++ at (V2 2 1) ++ "#" ++ at (V2 4 1) ++ "#" ++ at (V2 6 1) ++ "#" ++ at (V2 8 1) ++ "###",
  "  #" ++ at (V2 2 2) ++ "#" ++ at (V2 4 2) ++ "#" ++ at (V2 6 2) ++ "#" ++ at (V2 8 2) ++ "#",
  "  #" ++ at (V2 2 3) ++ "#" ++ at (V2 4 3) ++ "#" ++ at (V2 6 3) ++ "#" ++ at (V2 8 3) ++ "#",
  "  #" ++ at (V2 2 4) ++ "#" ++ at (V2 4 4) ++ "#" ++ at (V2 6 4) ++ "#" ++ at (V2 8 4) ++ "#",
  "  #########"]
 where
   at pos = maybe "." show (grid M.!? pos)