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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (Lens', at, each, folded, isn't, ix, maximumOf, minimumOf, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.Graph.Inductive (neighbors)
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, groupBy, sort, sortOn)
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
import Linear (V2 (..), V3 (..), _x, _y, _z)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import Data.Foldable (Foldable(toList))

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

type Input = [V3 Int]

-- type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine (splitOn "," -> [read -> x, read -> y, read -> z]) = V3 x y z
  parseLine _ = error ""

neighbors3d :: V3 Int -> [V3 Int]
neighbors3d v =
  [ v + d
  | d <-
      [ V3 1 0 0
      , V3 (-1) 0 0
      , V3 0 1 0
      , V3 0 (-1) 0
      , V3 0 0 1
      , V3 0 0 (-1)
      ]
  ]

surface :: Set (V3 Int) -> Int
surface vs = sum (map numUnglued (S.toList vs))
 where
  numUnglued v = length (filter (`S.notMember` vs) (neighbors3d v))

solve :: [V3 Int] -> Int
solve input = surface reachable - boxSurface
 where
  inputSet = S.fromList input
  reachable = reachableSet neighbors cMin
  cMin@(V3 minX minY minZ) = V3 (minD _x) (minD _y) (minD _z) + V3 (-1) (-1) (-1)
  cMax@(V3 maxX maxY maxZ) = V3 (maxD _x) (maxD _y) (maxD _z) + V3 1 1 1
  neighbors v = [v' | v' <- neighbors3d v, insideBox v' && v' `S.notMember` inputSet]
  insideBox v = and (zipWith (>=) (toList v) (toList cMin)) && and (zipWith (<=) (toList v) (toList cMax))
  boxSurface =
    let V3 dx dy dz = (cMax - cMin + V3 1 1 1)
     in dx * dy * 2 + dx * dz * 2 + dy * dz * 2
  minD l = fromJust (minimumOf (folded . l) input)
  maxD l = fromJust (maximumOf (folded . l) input)
