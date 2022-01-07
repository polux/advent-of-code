-- Copyright 2021 Google LLC.
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
import Linear (V2 (..), _x, _y, V3, V3)
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
import Data.List (sortOn, elemIndex, maximumBy, (\\))
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)

-- #endregion

import Text.Regex.Pcre2 (regex)
import Linear.V3 (V3(V3))
import Data.Ord (comparing)

type Input = [(V3 Int, Int)]


main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . T.lines . T.pack
 where
   toInt = read . T.unpack
   parseLine [regex|pos=<(?<x>.+),(?<y>.+),(?<z>.+)>, r=(?<r>.+)|] = (V3 (toInt x) (toInt y) (toInt z), toInt r)

distance :: V3 Int -> V3 Int -> Int
distance p1 p2 =
  let (V3 dx dy dz) = p2 - p1
  in abs dx + abs dy + abs dz

solve :: Input -> Int
solve input = input & filter (inRange . fst) & length
 where
   strongest@(pos, r) = maximumBy (comparing snd) input
   inRange otherPos = distance pos otherPos <= r
