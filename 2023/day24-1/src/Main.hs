-- Copyright 2022 Google LLC.
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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use underscore" #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.List (elemIndex, sortOn, tails)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.MemoTrie
import Data.Ratio
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
import Linear (V2 (..), det22, (*^), _x, _xy, _y)
import Linear.V2 (crossZ)
import Linear.V3
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

type Input = [(V3 Int, V3 Int)]

type Output = Int

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine (splitOn "@" -> [parseV3 -> v1, parseV3 -> v2]) = (v1, v2)
  parseV3 (map read . splitOn "," -> [x, y, z]) = V3 x y z


{-

p1 + t1 * v1 = p2 + t2 * v2
t1 * v1 - t2 * v2 = p2 - p1

[v1 -v2] . [t1 t2]T = p2 - p1

-}

intersection :: (V2 Int, V2 Int) -> (V2 Int, V2 Int) -> Maybe (V2 Double)
intersection (p1, v1) (p2, v2)
  | crossZ v1 v2 == 0 = Nothing
  | otherwise =
      let t1 = fromIntegral (det22 (V2 (p2 - p1) (-v2))) / fromIntegral (det22 (V2 v1 (-v2)))
          t2 = fromIntegral (det22 (V2 v1 (p2 - p1))) / fromIntegral (det22 (V2 v1 (-v2)))
       in if t1 >= 0 && t2 >= 0 then Just (fmap fromIntegral p1 + t1 *^ fmap fromIntegral v1) else Nothing

minVal = 200000000000000
maxVal = 400000000000000
inside (V2 x y) =
  and
    [ x >= fromIntegral minVal
    , x <= fromIntegral maxVal
    , y >= fromIntegral minVal
    , y <= fromIntegral maxVal
    ]

pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

-- solve :: Input -> Output
solve input = input & pairs & mapMaybe intersect2d & filter inside & length
 where
  intersect2d ((p1, v1), (p2, v2)) = intersection (p1 ^. _xy, v1 ^. _xy) (p2 ^. _xy, v2 ^. _xy)
