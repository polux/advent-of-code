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

import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?))
import Control.Lens.Extras (is)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Generics.Labels ()
import Data.Int (Int64)
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
import Data.Word (Word8)
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util (pTraceShow, pTraceShowId, arrayFromList1D, arrayFromList2D, arraySize)
import Linear (V2(..))

-- #endregion

type Input = UA.Array (V2 Int) Char

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = arrayFromList2D (lines str)

slopes :: [(Int, Int)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

solve :: Input -> Output
solve input = product [numTreeStops x y | (x, y) <- slopes]
  where
    width, height :: Int
    (V2 width height) = arraySize input
    coordinates :: Int -> Int -> [(Int, Int)]
    coordinates dx dy = zip [0, dx ..] (takeWhile (<height) [0, dy ..])
    visitedCells :: Int -> Int -> [Char]
    visitedCells dx dy =
      [ input UA.! V2 (x `mod` width) (y `mod` height)
        | (x, y) <- coordinates dx dy
      ]
    numTreeStops :: Int -> Int -> Int
    numTreeStops x y = length (filter (== '#') (visitedCells x y))