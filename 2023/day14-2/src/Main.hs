-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (elemIndex, sortOn, transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
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

type Input = [String]

type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = lines

-- load :: String -> Int
tiltRow = go 0 0
 where
  go i n [] = [[i - n + 1 .. i]]
  go i n ('#' : cs) = [i - n + 1 .. i] : go (i + 1) 0 cs
  go i n ('.' : cs) = go (i + 1) n cs
  go i n ('O' : cs) = go (i + 1) (n + 1) cs

rebuild w = map (rebuildRow w)
 where
  rebuildRow w = go 1
   where
    go i _ | i > w = []
    go i (n : ns)
      | i == n = 'O' : go (i + 1) ns
      | otherwise = '.' : go (i + 1) (n : ns)
    go i [] = '.' : go (i + 1) []

merge :: [String] -> [String] -> [String]
merge = zipWith mergeRow
 where
  mergeRow = zipWith mergeCell
  mergeCell 'O' 'O' = 'O'
  mergeCell 'O' '.' = 'O'
  mergeCell '.' 'O' = '.'
  mergeCell '.' '#' = '#'
  mergeCell '.' '.' = '.'
  mergeCell c1 c2 = error $ "merge error " ++ [c1, c2]

tilt grid =
  grid
    & map tiltRow
    & map concat
    & rebuild (length grid)
    & flip merge grid

oneQuarterCycle grid =
  grid
    & transpose
    & map reverse
    & tilt

oneCycle grid = grid & oneQuarterCycle & oneQuarterCycle & oneQuarterCycle & oneQuarterCycle

northLoad grid =
  grid
    & transpose
    & map reverse
    & map rowLoad
    & sum
 where
  rowLoad row = sum $ zipWith cellLoad [1 ..] row
  cellLoad i 'O' = i
  cellLoad _ _ = 0

-- solve :: Input -> Output
solve input =
  let (i, c) = findRecurringElement grids
   in northLoad (grids !! (i + (1_000_000_000 - i) `mod` (c - i)))
 where
  grids = iterate oneCycle input
