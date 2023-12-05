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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Data.List (foldl')

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
data Input = Input {seeds :: [Int], rangeMaps :: [[RangeMapping]]}
  deriving (Show)

type Output = Int

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse (splitOn "\n\n" -> (header : body)) = Input (parseSeeds header) (map parseRangeMap body)
 where
  parseSeeds (splitOn ":" -> [_, s]) = map read (words s)
  parseRangeMap (lines -> (_ : body)) = map parseRangeMapping body
  parseRangeMapping (map read . words -> [dest, src, len]) = RangeMapping src dest len

-- (range start, delta) sorted by range start
type DeltaMap = Vector (Int, Int)

toDeltaMap :: [RangeMapping] -> DeltaMap
toDeltaMap mps = V.fromList $ go 0 (sortOn sourceRangeStart mps)
  where
    go n [] = [(n, 0)]
    go n mps@(RangeMapping src _ _ : _)
      | n < src = (n, 0) : go2 mps
      | otherwise = go2 mps
    go2 [] = []
    go2 (RangeMapping src dest len : mps) =
      (src, dest-src) : go (src+len) mps


lookUpDelta :: DeltaMap -> Int -> Int
lookUpDelta dm n =
  let i = bisect (\i -> fst (dm V.! i) <= n) 0 (length dm)
  in snd (dm V.! i)

solve :: Input -> Output
solve Input{seeds,rangeMaps} = minimum (map resolve seeds)
  where
    deltaMaps = map toDeltaMap rangeMaps
    resolve seed = foldl' (\i dm -> i + lookUpDelta dm i) seed deltaMaps
