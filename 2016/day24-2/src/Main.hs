-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TupleSections #-}
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?))
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (partition, minimumBy, permutations, sortOn, elemIndex)
import Data.Char (isDigit)
import Data.Ord (comparing)
import Data.Foldable (Foldable(toList))

-- #endregion

type Input = UA.Array (Int, Int) Char


main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . lines

solve input = minimum (map pathCost candidatePaths)
--solve input = minimumBy (comparing pathCost) candidatePaths
  where
    (width, height) = arraySize input
    shortestPath = memo2 bfs
    locations = filter (isDigit . snd) (UA.assocs input)
    ([locationOfZero], otherLocations) = partition ((=='0') . snd) locations
    locationOfZeroCoord = fst locationOfZero
    otherLocationsCoords = map fst otherLocations
    candidatePaths = map ((++[locationOfZeroCoord]) . (locationOfZeroCoord :)) (permutations otherLocationsCoords)
    pathCost path = sum $ zipWith shortestPath path (tail path)
    neighbors (x, y) = filter valid [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
     where
       valid (i, j) = i >= 0 && i <= width && j >= 0 && j <= height && input UA.! (i,j) /= '#'
    bfs source target = go (S.singleton source) (Seq.singleton (source, 0))
     where
       go _ Empty = error "not found"
       --go seen cells | traceShow (seen, cells) False = undefined
       go seen ((cell, dist) :<| cells)
         | cell == target = dist
         | otherwise =
            let ns = S.fromList (neighbors cell)
            in go
             (S.union ns seen)
             (cells <> (ns & (`S.difference` seen) & toList & map (,dist+1) & Seq.fromList))
