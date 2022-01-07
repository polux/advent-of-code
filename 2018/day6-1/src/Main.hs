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
-- #region imports

-- #region imports
import Control.Lens (at, each, folded, isn't, ix, maximumOf, traversed, view, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, group, sort, sortOn, (\\))
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

type P = V2 Int

type Input = [P]

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
  where
    parseLine str =
      let [x, y] = map read (splitOn "," str)
       in V2 x y

distance :: P -> P -> Int
distance p1 p2 = sum (fmap abs (p2 - p1))

closest :: P -> [P] -> Maybe P
closest p ps =
  case sortOn snd tagged of
    ((p1, d1) : (_, d2) : _) | d1 == d2 -> Nothing
    ((p, _) : _) -> Just p
    _ -> error "no closest point"
  where
    tagged = [(q, distance p q) | q <- ps]

fill :: V2 Int -> [P] -> Map P (Maybe P)
fill (V2 w h) ps = M.fromList [(p, closest p ps) | i <- [0 .. w], j <- [0 .. h], let p = V2 i j]

bounds :: [P] -> P
bounds ps = V2 (maximum (map (view _x) ps)) (maximum (map (view _y) ps))

borderPoints :: V2 Int -> [P]
borderPoints (V2 w h) = [V2 i j | i <- [0 .. w], j <- [0, h]] ++ [V2 i j | i <- [0, w], j <- [0 .. h]]

solve input =
  closestMap
    & M.elems
    & catMaybes
    & filter (`S.notMember` infiniteCellGenerators)
    & groupBy id
    & M.map length
    & maximum
  where
    bbox = bounds input
    closestMap = fill bbox input
    infiniteCellGenerators = borderPoints bbox & mapMaybe (closestMap M.!) & S.fromList