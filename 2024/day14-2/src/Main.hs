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
import Data.List (elemIndex, sortOn)
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
import Data.List (tails)

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

type Input = [(V2 Int, V2 Int)]

main :: IO ()
main = readFile "input" >>= putStrLn . unlines . map renderFrame . filter candidate2 . zip [0..] . frames . parse
--main = readFile "input" >>= print . findRecurringElement . frames . parse

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine (ints -> [x, y, vx, vy]) = (V2 x y, V2 vx vy)
  parseLine _ = error "parse"

renderFrame (i,rs) = show i ++ ":\n" ++ render rs

render rs = unlines [[if V2 x y `S.member` ps then '#' else '.' | x <- [0..w-1]] | y <- [0..h-1]]
  where
    ps = S.fromList (map fst rs)

w = 101
h = 103

candidate2 (i,rs) = traceShow i $ traceShowId (maximum (map score pairs)) > 10
  where
    psSet = S.fromList ps
    ps = map fst rs
    pairs = [(p1,p2) | (p1:p1s) <- tails ps, p2 <- p1s, p1 /= p2]
    score (p1,p2) = iterate (+ (p2-p1)) p1 & takeWhile (`S.member` psSet) & length

candidate (i,rs) = traceShow i $ traceShowId (length (reachableSet (\p -> neighbors2D (V2 w h) p & filter (`S.notMember` ps)) firstFree)) < 3*((w*h)`div`4)
  where
    firstFree = head [p | x <- [0..w-1],  y <- [0..h-1], let p = V2 x y, p `S.notMember` ps]
    ps = S.fromList (map fst rs)

frames input = input & iterate (map move)
 where
  move (p, v) = let (V2 x y) = p + v in (V2 (x `mod` w) (y `mod` h), v)
