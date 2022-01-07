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

-- #region imports

import Algorithm.Search (aStar)
import Control.Lens (at, each, folded, isn't, ix, traversed, view, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (elemIndex, sortOn, (\\))
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

(depth, targetX, targetY) = (11817, 9, 751)
--(depth, targetX, targetY) = (510, 10, 10)

type Input = String

type Output = Input

main :: IO ()
main = print solve

geoIndex :: Int -> Int -> Int
geoIndex = memo2 geoIndex'

geoIndex' :: Int -> Int -> Int
geoIndex' 0 0 = 0
geoIndex' x y | x == targetX && y == targetY = 0
geoIndex' x 0 = x * 16807
geoIndex' 0 y = y * 48271
geoIndex' x y = erosionLevel (x -1) y * erosionLevel x (y -1)

erosionLevel :: Int -> Int -> Int
erosionLevel x y = (geoIndex x y + depth) `mod` 20183

riskLevel x y = erosionLevel x y `mod` 3

data Tool = Torch | Gear | Neither
  deriving (Show, Eq, Ord)

type State = (V2 Int, Tool)

cost :: State -> State -> Int
cost (_, t1) (_, t2) = if t1 == t2 then 1 else 7

tools (V2 x y) =
  case riskLevel x y of
    0 -> [Gear, Torch]
    1 -> [Gear, Neither]
    2 -> [Torch, Neither]
    _ -> error "unknown risk level"


okTool :: Int -> Tool -> Bool
okTool 0 Gear = True
okTool 0 Torch = True
okTool 1 Gear = True
okTool 1 Neither = True
okTool 2 Torch = True
okTool 2 Neither = True
okTool _ _ = False

otherTool :: Int -> Tool -> Tool
otherTool 0 Gear = Torch
otherTool 0 Torch = Gear
otherTool 1 Gear = Neither
otherTool 1 Neither = Gear
otherTool 2 Torch = Neither
otherTool 2 Neither = Torch
otherTool _ _ = error "wat"

neighbors :: State -> [State]
neighbors (V2 x y, tool) =
  (V2 x y, otherTool (riskLevel x y) tool) :
    [ (pos, tool)
      | pos@(V2 i j) <- [V2 (x - 1) y, V2 (x + 1) y, V2 x (y - 1), V2 x (y + 1)],
        i >= 0,
        j >= 0,
        okTool (riskLevel i j) tool
    ]

estimate :: State -> Int
estimate (p, _) = sum (fmap abs (V2 targetX targetY - p))

isTarget :: State -> Bool
isTarget (pos, Torch) = pos == V2 targetX targetY
isTarget _ = False


solve = fst <$> aStar neighbors cost estimate isTarget (V2 0 0, Torch)
