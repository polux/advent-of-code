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

-- #region imports
import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4, view)
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
import Safe hiding (at)
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortOn, elemIndex, sort)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)

-- #endregion

import Text.Regex.Pcre2 (regex)

parse :: String -> Input
parse = map parseLine . T.lines . T.pack
 where
   parseInt (T.unpack->s) =
     case s of
       ('-':cs) -> - (read cs)
       _ -> read s
   parseLine [regex|position=<(?<x>.+), (?<y>.+)> velocity=<(?<dx>.+), (?<dy>.+)>|] = (V2 (parseInt x) (parseInt y), V2 (parseInt dx) (parseInt dy))

type Input = [(V2 Int, V2 Int)]

type Output = Input

main :: IO ()
main = getContents >>= putStrLn . solve . parse

step :: Input -> Input
step = map stepPoint
 where
   stepPoint (p, v) = (p + v, v)

isCandidate :: [V2 Int] -> Bool
isCandidate points =
  points
    & groupBy (view _x)
    & M.elems
    & any ((>5) . maximum . countConsecutive . sort . map (view _y))

countConsecutive :: [Int] -> [Int]
countConsecutive xs = go 1 xs
 where
   go n (x:y:ys) | y == x+1 = go (n+1) (y:ys)
   go n (x:xs) = n : go 1 xs
   go n [] = []

display :: [V2 Int] -> String
display points = unlines [[if V2 i j `S.member` set then '#' else ' ' | i <- [0..w]] | j <- [0..h]]
  where
    set = S.fromList points
    w = maximum (map (view _x) points)
    h = maximum (map (view _y) points)

solve :: Input -> String
solve input = filter isCandidate movie & head & display
  where
    movie = iterate step input & map (map fst)
