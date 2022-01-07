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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
import Data.List (elemIndex, sort, sortOn)
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

type Input = [([String], [String])]

type Output = Int

type Mapping = Map Char (Set Char)

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine str =
    let [prefix, suffix] = splitOn "|" str
     in (words prefix, words suffix)

digits :: [(Set Char, Int)]
digits =
  [ (S.fromList "abcefg", 0)
  , (S.fromList "cf", 1)
  , (S.fromList "acdeg", 2)
  , (S.fromList "acdfg", 3)
  , (S.fromList "bcdf", 4)
  , (S.fromList "abdfg", 5)
  , (S.fromList "abdefg", 6)
  , (S.fromList "acf", 7)
  , (S.fromList "abcdefg", 8)
  , (S.fromList "abcdfg", 9)
  ]

candidates :: [Char] -> [Mapping]
candidates cs = [mkMapping segs | (segs, _) <- digits, length segs == length cs]
 where
  mkMapping segs = M.fromList [(seg, S.fromList cs) | seg <- S.toList segs]

solve :: Input -> Output
solve = sum . map solveLine
 where
  solveLine (patterns, output) =
    let mapping = findMapping patterns
        newDigits = M.fromList [(S.map (mapping M.!) key, val) | (key, val) <- digits]
     in output & map (S.fromList) & map (newDigits M.!) & foldl (\n d -> n * 10 + d) 0

findMapping patterns =
  mapM candidates patterns
    & map (simplify . M.unionsWith S.intersection)
    & filter (all ((== 1) . length) . M.elems)
    & filter ((== 7) . length . S.unions . M.elems)
    & head
    & fmap (head . S.toList)

simplify :: Mapping -> Mapping
simplify m = converge step m
 where
  step :: Mapping -> Mapping
  step m =
    let singletons = M.elems m & filter ((== 1) . length) & S.unions
     in M.map (\s -> if length s == 1 then s else s S.\\ singletons) m