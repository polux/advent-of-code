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

{-# HLINT ignore "Avoid reverse" #-}
-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Combinators (view)
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
import Data.List (elemIndex, foldl', sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
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

type Brick = (V3 Int, V3 Int)
type Input = [Brick]

type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine (splitOn "~" -> [parseV3 -> v1, parseV3 -> v2]) = (v1, v2)
  parseV3 (splitOn "," -> [x, y, z]) = fmap read (V3 x y z)

type Name = String
type Res = (Map (V2 Int) (Maybe Name, Int), Map Name (Set Name))

names = [c : s | s <- "" : names, c <- ['a' .. 'z']]

-- solve :: Input -> Output
solve input = length [n | (n, _) <- named, all sitsOnMoreThanOneBrick (fromMaybe [] (n `M.lookup` supports))]
 where
  sitsOn = foldl' drop (M.empty, M.empty) named & snd
  supports = M.fromListWith (<>) [(below, [atop]) | (atop, belows) <- M.toList sitsOn, below <- S.toList belows]
  sitsOnMoreThanOneBrick n = length (sitsOn M.! n) > 1

  sorted = sortOn (view (_1 . _z)) input
  named = zip names sorted
  shadow (V3 xmin ymin _, V3 xmax ymax _) = [V2 x y | x <- [xmin .. xmax], y <- [ymin .. ymax]]
  height (V3 _ _ zmin, V3 _ _ zmax) = zmax - zmin + 1
  drop :: Res -> (Name, Brick) -> Res
  drop (zmap, sitsOn) (name, brick) =
    let hits = shadow brick & map (\pos -> fromMaybe (Nothing, 0) (pos `M.lookup` zmap)) & sortOn snd & reverse
        toph = snd (head hits)
        touched = hits & takeWhile ((== toph) . snd) & mapMaybe fst & S.fromList
        overlay = M.fromList [(pos, (Just name, toph + height brick)) | pos <- shadow brick]
     in (M.union overlay zmap, M.insert name touched sitsOn)
