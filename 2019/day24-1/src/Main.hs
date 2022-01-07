-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeApplications #-}
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
import Control.Monad (forM_)

-- #endregion

type Input = Set (V2 Int)

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = str
  & lines
  & arrayFromList2D @_ @UA.Array @_ @(V2 Int)
  & UA.assocs
  & filter ((=='#').snd)
  & map fst
  & S.fromList

size = V2 5 5

neighbors :: V2 Int -> [V2 Int]
neighbors = neighbors2D size

numNeighborsOn :: Input -> V2 Int -> Int
numNeighborsOn on v = neighbors v & filter (`S.member` on) & length

candidates :: Input -> Set (V2 Int)
candidates on = on `S.union` (on & S.toList & concatMap neighbors & S.fromList)

step :: Input -> Input
step on = candidates on & S.filter (shouldLive on)

shouldLive :: Input -> V2 Int -> Bool
shouldLive on v = if v `S.member` on then numNeighbors == 1 else numNeighbors `elem` [1,2]
 where
   numNeighbors = numNeighborsOn on v

findCycle :: [Input] -> Input
findCycle = go mempty
  where
    go _ [] = error "not found"
    go seen (x : xs)
      | x `S.member` seen = x
      | otherwise = go (S.insert x seen) xs

bioPoints (V2 x y) = 2^(x+y*5)

solve input = findCycle (iterate step input) & S.map bioPoints & sum
