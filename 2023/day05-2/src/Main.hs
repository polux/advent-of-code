-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Control.Arrow (Arrow (first, second, (&&&)), (***), (>>>))
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
import Data.List (elemIndex, foldl', nub, sort, sortOn)
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
import Text.Regex.PCRE (Extract (empty), (=~))
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

data RangeMapping = RangeMapping {sourceRangeStart :: Int, destRangeStart :: Int, rangeLength :: Int}
  deriving (Show)
data Input = Input {seeds :: [(Int,Int)], rangeMaps :: [[RangeMapping]]}
  deriving (Show)

type Output = Int

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse (splitOn "\n\n" -> (header : body)) = Input (parseSeeds header) (map parseRangeMap body)
 where
  parseSeeds (splitOn ":" -> [_, s]) = pairUp (map read (words s))
  pairUp (x : y : xs) = (x, y) : pairUp xs
  pairUp _ = []
  parseRangeMap (lines -> (_ : body)) = map parseRangeMapping body
  parseRangeMappingHeader (words -> [splitOn "-to-" -> [source, dest], _]) = (source, dest)
  parseRangeMapping (map read . words -> [dest, src, len]) = RangeMapping src dest len

-- (x, f(x)) sorted by x
type Mapping = Vector (Int, Int)

apply :: Mapping -> Int -> Int
apply dm n =
  let i = bisect (\i -> fst (dm V.! i) <= n) 0 (length dm)
      (lb, flb) = dm V.! i
   in n + flb - lb

toMapping :: [RangeMapping] -> Mapping
toMapping mps = V.fromList $ go 0 (sortOn sourceRangeStart mps)
 where
  go n [] = [(n, n)]
  go n mps@(RangeMapping src _ _ : _)
    | n < src = (n, n) : go2 mps
    | otherwise = go2 mps
  go2 [] = []
  go2 (RangeMapping src dest len : mps) =
    (src, dest) : go (src + len) mps

toInverseMapping :: [RangeMapping] -> Mapping
toInverseMapping = toMapping . map inverseRangeMapping
 where
  inverseRangeMapping (RangeMapping start dest len) = RangeMapping dest start len

type InvertibleMapping = (Mapping, Mapping)

toInvertibleMapping :: [RangeMapping] -> InvertibleMapping
toInvertibleMapping = toMapping &&& toInverseMapping

compose :: InvertibleMapping -> InvertibleMapping -> InvertibleMapping
compose (dm1, rdm1) (dm2, rdm2) = (composeDms dm1 dm2 rdm1, composeDms rdm2 rdm1 dm2)
 where
  range = map fst . V.toList
  composeDms dm1 dm2 rdm1 =
    V.fromList
      $ [ (n, apply dm2 (apply dm1 n))
        | n <- nub $ sort (range dm1 ++ map (apply rdm1) (range dm2))
        ]

emptyInvertibleMapping :: InvertibleMapping
emptyInvertibleMapping = (V.fromList [(0, 0)], V.fromList [(0, 0)])

seedsToInvertibleMapping :: [(Int, Int)] -> InvertibleMapping
seedsToInvertibleMapping seeds = toInvertibleMapping [RangeMapping n n len | (n, len) <- seeds]

contains :: (Int, Int) -> Int -> Bool
contains (a, b) c = c >= a && c <= b

-- solve :: Input -> Output
solve Input{seeds, rangeMaps} =
  mapping
    & V.toList
    & filter (\(n, _) -> any (`contains` n) intervals)
    & map snd
    & minimum
 where
  imappings = map toInvertibleMapping  rangeMaps
  (mapping, _) = foldr compose emptyInvertibleMapping (seedsToInvertibleMapping seeds : imappings)
  intervals = [(n, n + len - 1) | (n, len) <- seeds]
