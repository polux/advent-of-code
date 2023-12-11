-- Copyright 2022 Google LLC.
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
{-# LANGUAGE MultiWayIf #-}

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
import Data.Ratio (Ratio)

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

type Input = UA.Array (V2 Int) Char

type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . lines

-- solve :: Input -> Output
solve input = length [i | i <- UA.indices input, i `S.notMember` loopSet && inside i]
 where
  loop = start : go start (inferredNeighbor start)
  loopSet = S.fromList loop

  hray v@(V2 _ j) = takeWhile (/= v) (iterate (+ V2 1 0) (V2 0 j))

  inside i = odd (length (filter counts (hray i)))
    where
      counts v = v `S.member` loopSet && valueAt v `elem` "|F7"

  go prev i | valueAt i == 'S' = [i]
            | otherwise = i : go i (neighbor prev i)
  start = head [i | (i, 'S') <- UA.assocs input]
  valueAt i = input UA.! i
  valueAt' i =
    case valueAt i of
      'S' -> inferredType i
      v -> v
  up i = i - V2 0 1
  down i = i + V2 0 1
  right i = i + V2 1 0
  left i = i - V2 1 0
  inferredType i =
    let downOk = valueAt (down i) `elem` "|JL"
        leftOk = valueAt (left i) `elem` "-FL"
        upOk = valueAt (up i) `elem` "|F7"
        rightOk = valueAt (right i) `elem` "-7J"
    in if | downOk && rightOk -> 'F'
          | leftOk && downOk -> '7'
          | upOk && leftOk -> 'J'
          | rightOk && upOk -> 'L'
          | otherwise -> error "cannot infer neighbor"
  inferredNeighbor i =
    case inferredType i of
      'F' -> right i
      '7' -> down i
      'J' -> left i
      'L' -> up i
      _ -> error "inferredNeighbor"
  neighbor prev i =
    case valueAt i of
      '|' | prev == up i -> down i
      '|' | prev == down i -> up i
      'F' | prev == right i -> down i
      'F' | prev == down i -> right i
      '-' | prev == right i -> left i
      '-' | prev == left i -> right i
      'J' | prev == left i -> up i
      'J' | prev == up i -> left i
      'L' | prev == up i -> right i
      'L' | prev == right i -> up i
      '7' | prev == down i -> left i
      '7' | prev == left i -> down i
      p -> error ("unkonwn pipe " ++ [p])
