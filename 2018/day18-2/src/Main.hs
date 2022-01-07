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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
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
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . lines

step :: Input -> Input
step arr = UA.listArray (UA.bounds arr) (map evolve (UA.assocs arr))
  where
    evolve (cell, chr) =
      let adj = neighbors arr cell
          count c = length (filter (== c) adj)
       in case chr of
            '.' -> if count '|' >= 3 then '|' else '.'
            '|' -> if count '#' >= 3 then '#' else '|'
            '#' -> if count '#' >= 1 && count '|' >= 1 then '#' else '.'
            c -> error ("unknown " <> [c])

neighbors :: Input -> V2 Int -> [Char]
neighbors arr (V2 x y) =
  [ arr UA.! V2 i j
    | i <- [x -1 .. x + 1],
      j <- [y -1 .. y + 1],
      (i, j) /= (x, y),
      i >= 0,
      i < w,
      j >= 0,
      j < h
  ]
  where
    (V2 w h) = arraySize arr

findCycle :: Ord a => [a] -> (Int, Int)
findCycle = go 0 mempty
  where
    go i seen (x : xs)
      | Just j <- M.lookup x seen = (j, i)
      | otherwise = go (i + 1) (M.insert x i seen) xs

solve input =
  let (start, end) = findCycle states
      index = start + ((1000000000 - end) `mod` (end - start))
      finalState = states !! index
      counts = finalState & UA.elems & groupBy id & M.map length
   in (counts M.! '|') * (counts M.! '#')
  where
    states = iterate step input
