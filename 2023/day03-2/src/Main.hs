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
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
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

type Input = [[Char]] -- A.Array (V2 Int) Char

type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = lines

type Value = Int
type Span = ((V2 Int, Int), Value)

-- solve :: Input -> Output
solve input =
  M.fromListWith (++) [(gear, [v]) | span@(_, v) <- spans, gear <- adjacentGears span]
    & M.elems
    & concatMap
      ( \case
          [n1, n2] -> [n1 * n2]
          _ -> []
      )
    & sum
 where
  spans = concat $ zipWith rowSpans [0 ..] input
  rowSpans j = go 0
   where
    go _ [] = []
    go i (c : cs)
      | isDigit c = go2 i (succ i) (read [c]) cs
      | otherwise = go (succ i) cs
    go2 o i acc [] = [((V2 o j, i - o), acc)]
    go2 o i acc (c : cs)
      | isDigit c = go2 o (succ i) (acc * 10 + read [c]) cs
      | otherwise = ((V2 o j, i - o), acc) : go (succ i) cs
  arr = arrayFromList2D input
  (V2 w h) = arraySize arr
  withinBounds (V2 i j) = i >= 0 && i < w && j >= 0 && j < h
  neighbors ((orig, len), _) = filter withinBounds $ (orig + V2 (-1) 0) : (orig + V2 len 0) : [orig + V2 i j | i <- [-1 .. len], j <- [-1, 1]]
  isSymbol pos = let v = arr A.! pos in v /= '.' && not (isDigit v)
  adjacentGears span = [pos | pos <- neighbors span, arr A.! pos == '*']