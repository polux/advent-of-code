-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
import Data.Array (inRange)
import qualified Data.Array as A
import Data.Array.Base (bounds)
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (foldl', minimumBy)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.MemoTrie
import Data.Ord (comparing)
import qualified Data.OrdPSQ as PQueue
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
import Util hiding (dijkstra)

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

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . lines

pattern E = V2 1 0
pattern W = V2 (-1) 0
pattern N = V2 0 (-1)
pattern S = V2 0 1

allDirs = [E, W, N, S]

solve input =
  let (dist, prev) =
        dijkstra @(V2 Int, V2 Int) @Int
          neighbors
          (\(_, d1) (_, d2) -> if d1 == d2 then 1 else 1001)
          (start, V2 1 0)
      minCost = minimum [cost | dir <- allDirs, Just cost <- [dist M.!? (end, dir)]]
      ends = [(end, dir) | dir <- allDirs, Just cost <- [dist M.!? (end, dir)], cost == minCost]
   in length (S.unions [ancestors prev e | e <- ends])
 where
  at p = input UA.! p
  start = head [p | (p, c) <- UA.assocs input, c == 'S']
  end = head [p | (p, c) <- UA.assocs input, c == 'E']
  ancestors prev (pos, dir) = S.insert pos (S.unions [ancestors prev pdir | pdir <- fromMaybe [] (prev M.!? (pos, dir))])
  neighbors :: (V2 Int, V2 Int) -> [(V2 Int, V2 Int)]
  neighbors (p, dir) =
    [ (p', dir')
    | dir' <- [dir, turnRight2D dir, turnLeft2D dir]
    , let p' = p + dir'
    , inRange (bounds input) p'
    , at p' /= '#'
    ]

dijkstra ::
  (Ord a, Num c, Ord c, Bounded c) =>
  (a -> [a]) ->
  (a -> a -> c) ->
  a ->
  (Map a c, Map a [a])
dijkstra neighbors edgeCost source = go mempty (M.singleton source 0) mempty (PQueue.singleton source 0 ())
 where
  distance dist k = fromMaybe maxBound (dist M.!? k)

  go done dist prev queue =
    case PQueue.minView queue of
      Nothing -> (dist, prev)
      Just (u, distU, (), queue') ->
        let (dist', prev', queue'') = foldl' (step u) (dist, prev, queue') [n | n <- neighbors u, n `S.notMember` done]
         in go (S.insert u done) dist' prev' queue''

  step u (dist, prev, queue) n =
    let alt = distance dist u + edgeCost u n
     in if
          | alt < distance dist n -> (M.insert n alt dist, M.insert n [u] prev, PQueue.insert n alt () queue)
          | alt == distance dist n -> (dist, M.insertWith (++) n [u] prev, queue)
          | otherwise -> (dist, prev, queue)