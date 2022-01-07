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

import Control.DeepSeq
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Complex (Complex (..), cis, imagPart, realPart)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, scanl', sortOn, transpose, foldl', tails)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, mapMaybe)
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
import GHC.IO (unsafePerformIO)
import Linear (V2 (..), angle, (*^), _x, _y)
import QuickSpec
import Safe hiding (at)
import Test.QuickCheck (Arbitrary (..), NonNegative (NonNegative), chooseInt, vectorOf, (==>), quickCheckResult)
import Test.QuickCheck.Modifiers (Positive (Positive, getPositive))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Input = UV.Vector Int

parse :: String -> Input
parse = UV.fromList . map (read . (: [])) . head . lines

input :: Input
input = parse (unsafePerformIO (readFile "/home/polux/projects/aoc/2019/day16-2/input.txt"))

vecSize :: Int
vecSize = UV.length input * 10_000

offset :: Int
offset = UV.take 7 input & UV.toList & concatMap show & read

inputFromOffset :: [Int]
inputFromOffset = [input UV.! (i `mod` UV.length input) | i <- [offset..vecSize-1]]

multFromOffset :: [Int] -> [Int]
multFromOffset vec = scanr (+) 0 vec & map (`mod` 10)

result :: [Int]
result = take 8 (iterate multFromOffset inputFromOffset !! 100)

main :: IO ()
main = print result