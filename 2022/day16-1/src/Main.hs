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
import Data.List (elemIndex, permutations, sortOn, sort, sortBy)
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
import Text.Regex.Pcre2 (regex)
import Data.Ord (comparing, Down (Down))

parse :: String -> Input
parse = M.fromList . map parseLine . T.lines . T.pack
 where
  toInt = read . T.unpack
  parseLine [regex|Valve (?<v>.*) has flow rate=(?<x>\d+); tunnels? leads? to valves? (?<vs>.*)|] = (T.unpack v, (toInt x, splitOn ", " (T.unpack vs)))

type Input = Map String (Int, [String])

--type Output = Int

main :: IO ()
main =
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

--solve :: Input -> Output
solve input = bisect (real 0 0 29) 0 (upperBound 0 30)
 where
  graph = convert input
  vertices = M.keys graph
  yield v = fst (graph M.! v)
  neighbors v = snd (graph M.! v)

  memo4 :: (HasTrie r, HasTrie s, HasTrie t, HasTrie u) => (r -> s -> t -> u -> a) -> r -> s -> t -> u -> a
  memo4 = mup memo3

  mreal = memo4 real

  real :: Vertex -> BitSet -> Time -> Int -> Bool
  real v open 0 c = c <= 0
  real v open t c | c > upperBound open t = False
  real v open t c =
    (not (testBit open v) && mreal v (setBit open v) (t-1) (c-t*yield v))
    || or [mreal v' open (t-1) c | v' <- neighbors v]

  closed :: BitSet -> [Vertex]
  closed set = [v | v <- vertices, not (testBit set v)]

  upperBound :: BitSet -> Time -> Int
  upperBound open t = sum (zipWith (*) [t,t-2..0] (sortBy (comparing Down) (map yield (closed open))))

  distance :: Vertex -> Vertex -> Int
  distance i j = mFloyd i j (maximum vertices)

  mFloyd = memo3 floydWarshall

  floydWarshall :: Vertex -> Vertex -> Int -> Int
  floydWarshall i j 0 | i == j = 0
                      | j `elem` neighbors i = 1
                      | otherwise = 1_000_000_000_000
  floydWarshall i j k = min (mFloyd i j (k-1)) (mFloyd i k (k-1) + mFloyd k j (k-1))
