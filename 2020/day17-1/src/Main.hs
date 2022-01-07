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
import Linear (V2 (..), V3 (..))
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

-- #endregion

type Input = Set Cell

type Output = Int
type Cell = V3 Int

mkCell :: Int -> Int -> Cell
mkCell i j = V3 i j 0

neighbors :: Cell -> Set Cell
neighbors (V3 i j k) = S.fromList $
  [V3 i' j' k' | i' <- [i-1..i+1], j' <- [j-1..j+1], k' <- [k-1..k+1], (i',j', k') /= (i,j,k)]

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = S.unions $ zipWith parseLine [0..] (lines str)
 where
   parseLine j line = S.unions $ zipWith (parseCell j) [0..] line
   parseCell j i '#' = S.singleton (mkCell i j)
   parseCell j i '.' = mempty

step :: Input -> Input
step grid = S.filter activeNext candidates
  where
    candidates = grid <> S.unions (S.map neighbors grid)
    active cell = cell `S.member` grid
    activeNext cell =
      let numActiveNeighbors = length (S.filter active (neighbors cell))
      in if active cell
        then numActiveNeighbors == 2 || numActiveNeighbors == 3
        else numActiveNeighbors == 3

solve :: Input -> Output
solve input = length (iterate step input !! 6)
