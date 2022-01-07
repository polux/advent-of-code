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
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
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
import Safe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Input = UA.Array (V2 Int) Char

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = arrayFromList2D (lines str)

linesOfSight :: V2 Int -> [[V2 Int]]
linesOfSight coord = [tail $ iterate (+ offset) coord | offset <- offsets]
  where
    offsets = [V2 i j | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]

clamp :: Int -> Int -> [V2 Int] -> [V2 Int]
clamp width height = takeWhile inside
  where
    inside (V2 i j) = i >= 0 && i < width && j >= 0 && j < height

findFirstVisible :: Input -> [V2 Int] -> Char
findFirstVisible arr coords =
  coords
    & map (arr UA.!)
    & filter (/= '.')
    & headDef '.'

numOccupiedNeighbors :: Input -> V2 Int -> Int
numOccupiedNeighbors arr coord =
  linesOfSight coord
    & map (clamp width height)
    & map (findFirstVisible arr)
    & filter (== '#')
    & length
  where
    (V2 width height) = arraySize arr

update :: Input -> Input
update arr = mapArrayWithIndex updateCell arr
  where
    updateCell coord 'L' | numOccupiedNeighbors arr coord == 0 = '#'
    updateCell coord '#' | numOccupiedNeighbors arr coord >= 5 = 'L'
    updateCell _ e = e

converge :: (Eq t) => (t -> t) -> t -> t
converge f x =
  let x' = f x
   in if x == x' then x else converge f x'

solve :: Input -> Output
solve input = length (filter (== '#') (UA.elems (converge update input)))

display :: Input -> String
display arr = unlines [displayRow j | j <- [0 .. height -1]]
  where
    displayRow j = [arr UA.! V2 i j | i <- [0 .. width -1]]
    (V2 width height) = arraySize arr