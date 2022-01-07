-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

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
import Data.List (permutations, sortOn, elemIndex)
import Data.Foldable (Foldable(toList))

-- #endregion

type Input = [(String, String, Int)]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = map parseLine (lines str)
 where
   parseLine line =
     let [[_, city1, city2, distStr]] = line =~ "(\\S+) to (\\S+) = (\\d+)"
     in (city1, city2, read distStr)

type BiMap = Map (String, String) Int

buildBiMap :: Input -> BiMap
buildBiMap input = M.unions (map mkEntry input)
 where
   mkEntry (city1, city2, dist) = M.fromList [((city1, city2), dist), ((city2, city1), dist)]

cities :: Input -> Set String
cities input = S.fromList (concatMap citiesOfEntry input)
 where
   citiesOfEntry (city1, city2, _) = [city1, city2]

score :: BiMap -> [String] -> Int
score distances (c1:c2:cs) = distances M.! (c1, c2) + score distances (c2:cs)
score _ [_] = 0
score _ [] = error "empty path"

solve :: Input -> Output
solve input = minimum (map (score distances) candidates)
  where
    candidates = permutations (toList $ cities input)
    distances = buildBiMap input
