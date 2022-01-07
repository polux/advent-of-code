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
import Data.List (transpose, sortOn, elemIndex)
import Data.List.Split
import Control.Arrow ((>>>))

-- #endregion

type Input = [[Int]]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
   parseLine str =
      splitOn ": " str
      & (!! 1)
      & splitOn ", "
      & map (words >>> (!! 1) >>> read)

sumsTo :: Int -> Int -> [[Int]]
sumsTo 1 goal = return [goal]
sumsTo numSums goal = do
  n <- [0..goal]
  ns <- sumsTo (numSums-1) (goal-n)
  return (n:ns)

solve :: Input -> Output
solve input = maximum (map score candidates)
 where
   candidates = sumsTo (length input) 100
   factors = init (transpose input) -- ignore calories
   score candidate = product (map (properties candidate) factors)
   properties candidate row = max 0 (sum (zipWith (*) candidate row))
