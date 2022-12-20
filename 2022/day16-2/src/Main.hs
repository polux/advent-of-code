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
import Data.List (elemIndex, minimumBy, permutations, sort, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, isJust)
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
import Linear (V2 (..), vector, _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

import Data.Bits
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import GHC.Exts (Down (..))
import Text.Regex.Pcre2 (regex)

parse :: String -> Input
parse = M.fromList . map parseLine . T.lines . T.pack
 where
  toInt = read . T.unpack
  parseLine [regex|Valve (?<v>.*) has flow rate=(?<x>\d+); tunnels? leads? to valves? (?<vs>.*)|] = (T.unpack v, (toInt x, splitOn ", " (T.unpack vs)))

type Input = Map String (Int, [String])

-- type Output = Int

main :: IO ()
main = do
  readFile "example" >>= print . solve . parse
  readFile "input" >>= print . solve . parse

-- readFile "example" >>= print . convert . parse

type Graph = Map Int (Int, [Int])

convert :: Input -> Graph
convert input = M.fromList [(number v, (c, map number vs)) | (v, (c, vs)) <- M.toList input]
 where
  numbers = M.fromList (zip (M.keys input) [0 ..])
  number v = numbers M.! v

type BitSet = Int
type Vertex = Int
type Time = Int
type Duration = Int

data State = S {v1 :: Vertex, v2 :: Vertex, valves :: BitSet, time :: Time, production :: Int}
  deriving (Eq, Ord, Generic, Show)

data Config = C { v1 :: Vertex, v2 :: Vertex, time :: Time}
  deriving (Eq, Ord, Generic, Show)

mkS :: Vertex -> Vertex -> BitSet -> Time -> Int -> State
mkS v1 v2 = S (min v1 v2) (max v1 v2)

-- solve :: Input -> Output
solve input = traceShow (upperBound initState) $ go mempty 0 (Seq.singleton initState)
 where
  initState = mkS 0 0 0 25 0
  graph = convert input
  vertices = M.keys graph
  yield v = fst (graph M.! v)
  vertexNeighbors v = snd (graph M.! v)

  go :: S.Set State -> Int -> Seq State -> Int
  --go _ m q | traceShow (m, Seq.length q) False = undefined
  go seen m Empty = m
  go seen m (state :<| states) =
    let m' = max m (production state)
     in go
          (S.insert state seen)
          m'
          ((neighbors state & filter (`S.notMember` seen) & filter (valid m') & Seq.fromList) <> states)

  valid m s = time s > 0 && (upperBound s >= m)

  neighbors :: State -> [State]
  neighbors (S v e valves t c) =
    [mkS v e (valves `setBit` v `setBit` e) (t - 1) (c + t * yield v + t * yield e) | not (testBit valves v), not (testBit valves e), e /= v]
      ++ [mkS v' e (setBit valves e) (t - 1) (c + t * yield e) | not (testBit valves e), v' <- vertexNeighbors v]
      ++ [mkS v e' (setBit valves v) (t - 1) (c + t * yield v) | not (testBit valves v), e' <- vertexNeighbors e]
      ++ [mkS v' e' valves (t - 1) c | v' <- vertexNeighbors v, e' <- vertexNeighbors e]

  closed :: BitSet -> [Vertex]
  closed set = [v | v <- vertices, not (testBit set v)]


  upperBound :: State -> Int
  upperBound (S _ _ valves t p) = p + sum (zipWith (*) (double [t, t - 2 .. 0]) (sortDesc products))
   where
    double [] = []
    double (x : xs) = x : x : double xs
    sortDesc = sortOn Down
    products = map yield (closed valves)

  distance :: Vertex -> Vertex -> Int
  distance i j = mFW i j (maximum vertices)

  mFW :: Vertex -> Vertex -> Int -> Int
  mFW = memo3 floydWarshall

  floydWarshall :: Vertex -> Vertex -> Int -> Int
  floydWarshall i j 0
    | i == j = 0
    | j `elem` vertexNeighbors i = 1
    | otherwise = 1_000_000_000_000
  floydWarshall i j k = min (mFW i j (k - 1)) (mFW i k (k - 1) + mFW k j (k - 1))