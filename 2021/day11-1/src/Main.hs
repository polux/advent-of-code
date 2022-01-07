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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, intercalate, sortOn)
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

type Input = Map (V2 Int) Int

--type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse =
  M.fromList
    . UA.assocs
    . arrayFromList2D @_ @UA.Array @_ @_
    . map (map (read . singleton))
    . lines

neighbors :: V2 Int -> [V2 Int]
neighbors p =
  [p + V2 x y | x <- [-1 .. 1], y <- [-1 .. 1]]
  & filter (\q -> q /= p && q ^. _x >= 0 && q ^. _x < 10 && q ^. _y >= 0 && q ^. _y < 10)

step' :: Input -> (Int, Input)
step' input = first length (flash mempty init)
  where
    init = M.map bump input
    bump n = (n+1) `mod` 10
    bumpSaturate 0 = 0
    bumpSaturate n = bump n
    flash flashed grid =
      let zeros = M.filterWithKey (\k n -> k `S.notMember` flashed && n == 0) grid & M.keys
      in if null zeros
        then (flashed, grid)
        else flash (flashed <> S.fromList zeros) (foldr bumpNeighbors grid zeros)
    bumpNeighbors p grid = foldr (M.adjust bumpSaturate) grid (neighbors p)

step :: (Int, Input) -> (Int, Input)
step (count, input) = let (d, g) = step' input in (count+d, g)

display :: Input -> String
display m = unlines [concat [show (m M.! V2 x y) | x <- [0..9]] | y <- [0..9]]

--solve :: Input -> Output
solve input = fst $ iterate step (0, input) !! 100