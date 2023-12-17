-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
-- #endregion

module Main where

-- #region imports

import Control.Arrow ((>>>),(***), Arrow (first, second))
import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe hiding (at)
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_, unless, when, guard)

import qualified Data.OrdPSQ as PQueue

type Input = UA.Array (V2 Int) Int

type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . map (map (read . (:[]))) . lines

type Node = (V2 Int, V2 Int, Int)

--solve :: Input -> Output
solve input = fst <$> mydijkstra neighbors edgeCost isTarget sources
 where
  sources :: [Node]
  sources = [(V2 0 0, V2 1 0, 0), (V2 0 0, V2 0 1, 0)]
  (V2 w h) = arraySize input
  isTarget (cell,_,_) = cell == V2 (w-1) (h-1)

  inside (V2 x y) = x >= 0 && x < w && y >= 0 && y < h

  neighbors (pos, dir, steps) = do
    delta <- [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
    guard (delta /= -dir)
    let newPos = pos + delta
    guard (inside newPos)
    let newSteps = if delta == dir then steps+1 else 1
    guard (newSteps < 4)
    return (newPos, delta, newSteps)

  edgeCost _ (cell, _, _) = input UA.! cell

mydijkstra neighbors edgeCost isTarget sources = go mempty (PQueue.fromList [(v,0,[v]) | v <- sources])
 where
  go done queue =
    case PQueue.minView queue of
      Nothing -> Nothing
      Just (u, distU, pathToU, queue') ->
        if isTarget u
          then Just (distU, reverse pathToU)
          else
            go
              (S.insert u done)
              (foldr (step done u pathToU distU) queue' (neighbors u & filter (`S.notMember` done)))
  step done u pathToU distU v queue =
    let alt = distU + edgeCost u v
        distV = maybe maxBound fst (PQueue.lookup v queue)
     in if alt < distV
          then PQueue.insert v alt (v : pathToU) queue
          else queue
