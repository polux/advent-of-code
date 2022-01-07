-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

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

import Control.Lens (Ixed(ix), each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Safe
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (foldl', transpose, sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)

-- #endregion

import Text.Regex.Pcre2 (regex)


data Instr = Rect Int Int | RotateColumn Int Int | RotateRow Int Int
  deriving (Show)
type Input = [Instr]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . T.lines . T.pack
 where
   toInt = read . T.unpack
   parseLine [regex|rect (?<w>\d+)x(?<h>\d+)|] = Rect (toInt w) (toInt h)
   parseLine [regex|rotate column x=(?<x>\d+) by (?<n>\d+)|] = RotateColumn (toInt x) (toInt n)
   parseLine [regex|rotate row y=(?<y>\d+) by (?<n>\d+)|] = RotateRow (toInt y) (toInt n)

rotate :: Int -> [a] -> [a]
rotate n xs = drop m xs ++ take m xs
  where
    len = length xs
    m = len - (n `mod` len)

rect :: Int -> Int -> [String] -> [String]
rect w h lines = map replacePrefix (take h lines) ++ drop h lines
 where
   prefix = replicate w '#'
   replacePrefix cs = prefix ++ drop w cs

exec :: [String] -> Instr -> [String]
exec grid (Rect w h) = rect w h grid
exec grid (RotateRow j n) = grid & ix j %~ rotate n
exec grid (RotateColumn i n) = grid & transpose & ix i %~ rotate n & transpose

initialGrid = replicate 6 (replicate 50 '.')

solve :: Input -> Output
solve input =
  input
  & foldl' exec initialGrid
  & concat
  & filter (=='#')
  & length
