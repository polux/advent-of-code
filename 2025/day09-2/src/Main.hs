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
{-# OPTIONS_GHC -Wno-x-partial #-}

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
import Data.List (elemIndex, maximumBy, sort, sortOn, tails)
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
import Graphics.Gloss
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

type Input = [V2 Int]

type Output = Input

main :: IO ()
main = do
  --readFile "input" >>= print . solve . parse

 str <- readFile "input"
 display FullScreen black (render (parse str))

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine (splitOn "," -> [read -> x, read -> y]) = V2 x y

render input =
  scale
    0.01
    0.01
    ( color white (polygon coords)
        <> color red (mconcat (map (\(x, y) -> translate x y (circleSolid 100)) coords))
    )
 where
  coords :: [Point]
  coords = map (\(V2 x y) -> (fromIntegral x, fromIntegral y)) input

data HSeg = HSeg {y :: Int, x1 :: Int, x2 :: Int} deriving (Show)
data VSeg = VSeg {x :: Int, y1 :: Int, y2 :: Int} deriving (Show)

intersectsBottomIncluded :: HSeg -> VSeg -> Bool
intersectsBottomIncluded (HSeg y x1 x2) (VSeg x y1 y2) =
  y > min y1 y2 && y <= max y1 y2 && x > min x1 x2 && x < max x1 x2

intersectsTopIncluded :: HSeg -> VSeg -> Bool
intersectsTopIncluded (HSeg y x1 x2) (VSeg x y1 y2) =
  y >= min y1 y2 && y < max y1 y2 && x > min x1 x2 && x < max x1 x2

intersectsLeftIncluded :: VSeg -> HSeg -> Bool
intersectsLeftIncluded (VSeg x y1 y2) (HSeg y x1 x2) =
  y > min y1 y2 && y < max y1 y2 && x > min x1 x2 && x <= max x1 x2

intersectsRightIncluded :: VSeg -> HSeg -> Bool
intersectsRightIncluded (VSeg x y1 y2) (HSeg y x1 x2) =
  y > min y1 y2 && y < max y1 y2 && x >= min x1 x2 && x < max x1 x2

toSegs :: Input -> ([HSeg], [VSeg])
toSegs input = go [] [] (input ++ [head input])
 where
  go hs vs [_] = (hs, vs)
  go hs vs (V2 x1 y1 : V2 x2 y2 : ps)
    | y1 == y2 = go (HSeg y1 x1 x2 : hs) vs (V2 x2 y2 : ps)
    | x1 == x2 = go hs (VSeg x1 y1 y2 : vs) (V2 x2 y2 : ps)
    | otherwise = error "unexpected"

-- solve :: Input -> Output
solve input = maximum [area (p1, p2) | p1 : ps <- tails input, p2 <- ps, ok p1 p2]
 where
  (hsegs, vsegs) = toSegs input
  area (V2 x1 y1, V2 x2 y2) = (1 + abs (x1 - x2)) * (1 + abs (y1 - y2))
  ok (V2 x1 y1) (V2 x2 y2) =
    let minx = min x1 x2
        maxx = max x1 x2
        miny = min y1 y2
        maxy = max y1 y2
     in not $ or
          [ any (intersectsBottomIncluded (HSeg maxy minx maxx)) vsegs
          , any (intersectsTopIncluded (HSeg miny minx maxx)) vsegs
          , any (intersectsLeftIncluded (VSeg maxx miny maxy)) hsegs
          , any (intersectsRightIncluded (VSeg minx miny maxy)) hsegs
          ]
