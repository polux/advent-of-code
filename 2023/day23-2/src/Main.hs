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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Combinators (over)
import Control.Lens.Extras (is)
import Control.Monad (forM_, guard, unless, when)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.Bits (Bits (shiftL), (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, scanl', sortOn)
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

type Input = UA.Array (V2 Int) Char

type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . lines

-- solve :: Input -> Output
solve input = longestPath 0 src
 where
  at pos = input UA.! pos
  (V2 w h) = arraySize input
  start = head [pos | i <- [0 .. w - 1], let pos = V2 i 0, at pos == '.']
  end = head [pos | i <- [0 .. w - 1], let pos = V2 i (h - 1), at pos == '.']

  neighbors pos =
    [ n
    | n <- neighbors2D (V2 w h) pos
    , at n /= '#'
    ]

  intersections =
    [start, end]
      ++ [ pos
         | pos <- UA.indices input
         , at pos /= '#'
         , length (neighbors pos) > 2
         ]

  reachableIntersections pos = go (S.singleton pos) [(n, 1) | n <- neighbors pos] []
   where
    go _ [] acc = acc
    go seen ((p, l) : ps) acc
      | p `S.member` targets = go (S.insert p seen) ps ((p, l) : acc)
      | otherwise = go (S.insert p seen) ([(n, l + 1) | n <- neighbors p, n `S.notMember` seen] ++ ps) acc

    targets = S.fromList intersections

  rename pos = names M.! pos
   where
    names = M.fromList (zip intersections powersOfTwo)
    powersOfTwo = iterate (`shiftL` 1) (1 :: Int)

  graph =
    M.fromList
      [ (rename p, over (each . _1) rename (reachableIntersections p))
      | p <- intersections
      ]

  src = rename start
  tgt = rename end

  longestPath :: Int -> Int -> Int
  longestPath !seen v
    | v == tgt = 0
    | otherwise = maximumDef minBound [l + longestPath (seen .|. v) n | (n, l) <- graph M.! v, seen .&. n == 0]