{-# LANGUAGE BangPatterns #-}
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

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, guard, unless, when)
import qualified Data.Array as A
import qualified Data.Array.ST as ST
import Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, foldl', maximumBy, permutations, sort, sortBy, sortOn)
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

import Data.Bits
import Data.Ord (Down (Down), comparing)
import Text.Regex.Pcre2 (regex)

parse :: String -> Input
parse = M.fromList . map parseLine . T.lines . T.pack
 where
  toInt = read . T.unpack
  parseLine [regex|Valve (?<v>.*) has flow rate=(?<x>\d+); tunnels? leads? to valves? (?<vs>.*)|] = (T.unpack v, (toInt x, splitOn ", " (T.unpack vs)))

type FlowRate = Int
type Input = Map String (FlowRate, [String])
type Output = Int

main :: IO ()
main =
  --readFile "example" >>= print . solve . parse
  readFile "input" >>= print . solve . parse

type Vertex = Int
type Graph = Map Vertex (FlowRate, [Vertex])

toGraph :: Input -> Graph
toGraph input = M.fromList [(number v, (c, map number vs)) | (v, (c, vs)) <- M.toList input]
 where
  numbers = M.fromList (zip (M.keys input) [0 ..])
  number v = numbers M.! v

type BitSet = Int
type Time = Int

--solve :: Input -> Output
solve input = maximum [p1+p2 | (v1,p1) <- paths 26, (v2,p2) <- paths 26, v1 .&. v2 == 0]
 where
  graph = toGraph input
  vertices = M.keys graph
  nonNullVertices = vertices & filter ((> 0) . flowRate)
  flowRate v = fst (graph M.! v)
  neighbors v = snd (graph M.! v)

  paths :: Time -> [(BitSet, Int)]
  paths time = maximums (go (0 :: BitSet) time 0)
   where
    maximums ps = M.toList (M.fromListWith max ps)
    go valves time prev =
        (valves, 0) :
        [ (valves', flowRate v * time' + tailRate)
        | v <- nonNullVertices
        , not (testBit valves v)
        , let time' = time - distance prev v - 1
        , time' >= 0
        , (valves', tailRate) <- go (setBit valves v) time' v
        ]

  distance :: Vertex -> Vertex -> Int
  distance i j = floyd ! V2 i j

  floyd :: UA.UArray (V2 Int) Int
  floyd = snd (iterate step (0, mkArray initValue) !! maximum vertices)
   where
    n = length graph - 1
    bounds = (V2 0 0, V2 n n)
    mkArray f = UA.array bounds [(V2 i j, f i j) | i <- [0 .. n], j <- [0 .. n]]

    initArray = mkArray initValue
    initValue i j
      | i == j = 0
      | j `elem` neighbors i = 1
      | otherwise = 1_000

    step (k, a) = (k + 1, mkArray (update k a))
    update k a i j = min (a ! V2 i j) (a ! V2 i k + a ! V2 k j)
