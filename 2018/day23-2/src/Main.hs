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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (Lens', at, each, folded, isn't, ix, traversed, view, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
-- #endregion

import Data.Generics.Product (typed)
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, maximumBy, scanl', sortOn, (\\), minimumBy)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import Data.MemoTrie
import Data.Ord (comparing)
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
import Linear (V2 (..), V3 (..), (^*), _x, _y, _z)
import Linear.Quaternion (axisAngle)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Text.Regex.Pcre2 (regex)
import Util
import Vis
import Control.Arrow ((***),(&&&))

type Input = [(V3 Int, Int)]

main = do
  balls <- parse <$> getContents -- readFile "input.txt"
  let v1 = bestInitialVoxel (scaleDown 10_000_000 balls)
  print v1
  let v2 = bestVoxel (voxelToNextBox v1) (scaleDown 1_000_000 balls)
  print v2
  let v3 = bestVoxel (voxelToNextBox v2) (scaleDown 100_000 balls)
  print v3
  let v4 = bestVoxel (voxelToNextBox v3) (scaleDown 10_000 balls)
  print v4
  let v5 = bestVoxel (voxelToNextBox v4) (scaleDown 1_000 balls)
  print v5
  let v6 = bestVoxel (voxelToNextBox v5) (scaleDown 100 balls)
  print v6
  let v7 = bestVoxel (voxelToNextBox v6) (scaleDown 10 balls)
  print v7
  let v8 = bestVoxel (voxelToNextBox v7) balls
  print v8

parse :: String -> Input
parse = map parseLine . T.lines . T.pack
  where
    toInt = read . T.unpack
    parseLine [regex|pos=<(?<x>.+),(?<y>.+),(?<z>.+)>, r=(?<r>.+)|] = (V3 (toInt x) (toInt y) (toInt z), toInt r)

distance :: V3 Int -> V3 Int -> Int
distance p1 p2 =
  let (V3 dx dy dz) = p2 - p1
   in abs dx + abs dy + abs dz

ball :: (V3 Int, Int) -> Map (V3 Int) Int
ball (pos, r) = M.fromList $ map (,1) $ filter ((<= r) . distance pos) cube
  where
    cube = [pos + V3 x y z | x <- [- r .. r], y <- [- r .. r], z <- [- r .. r]]

solve :: Input -> Int
solve input = map (distance (V3 0 0 0)) candidates & minimum
  where
    counts = M.unionsWith (+) (map ball input)
    maxCount = maximum counts
    candidates = M.toList counts & filter ((== maxCount) . snd) & map fst

type Ball = (V3 Int, Int)

scaleDown :: Int -> [Ball] -> [Ball]
scaleDown n = map scaleBall
  where
    scaleBall (pos, r) = (fmap (`div` n) pos, r `div` n)

type Box = (V3 Int, V3 Int)

bestInitialVoxel :: [Ball] -> V3 Int
bestInitialVoxel balls = minimumBy (comparing (distance (V3 0 0 0))) candidates
  where
    counts = M.unionsWith (+) (map ball balls)
    maxCount = maximum counts
    candidates = M.toList counts & filter ((== maxCount) . snd) & map fst

voxelToNextBox :: V3 Int -> Box
voxelToNextBox pos = ((pos ^* 10) + V3 (-25) (-25) (-25), (pos ^* 10) + V3 25 25 25)

bestVoxel :: Box -> [Ball] -> V3 Int
bestVoxel (V3 x1 y1 z1, V3 x2 y2 z2) balls =
  maximumBy (comparing (numBalls &&& distance (V3 0 0 0))) [V3 x y z | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2.. max y1 y2], z <- [min z1 z2 .. max z1 z2]]
  where
    numBalls p = filter (p `inside`) balls & length
    inside p1 (p2, r) = distance p1 p2 <= r
