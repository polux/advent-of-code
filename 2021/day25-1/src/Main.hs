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
{-# LANGUAGE TypeApplications #-}
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

data Dir = E | S
  deriving (Eq, Ord, Show)
type Input = (Map (V2 Int) Dir, V2 Int)

--type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse str =
  ( array
      & UA.assocs
      & M.fromList
      & M.mapMaybe parseCell
  , arraySize array
  )
 where
  array = lines str & arrayFromList2D @_ @UA.Array
  parseCell '>' = Just E
  parseCell 'v' = Just S
  parseCell _ = Nothing

--solve :: Input -> Output
solve (grid, V2 w h) = head [i | (i, s1, s2) <- zip3 [1..] steps (tail steps), s1 == s2]
 where
   steps = iterate step grid
   step g = moveSouthHerd (moveEastHerd g)
   moveEastHerd g = foldr (moveEast g) g (M.keys (M.filter (==E) g))
   moveEast g pos
     | east pos `M.notMember` g = M.delete pos . M.insert (east pos) E
     | otherwise = id
   moveSouthHerd g = foldr (moveSouth g) g (M.keys (M.filter (==S) g))
   moveSouth g pos
     | south pos `M.notMember` g = M.delete pos . M.insert (south pos) S
     | otherwise = id
   east (V2 x y) = V2 ((x+1) `mod` w) y
   south (V2 x y) = V2 x ((y+1) `mod` h)

display (V2 w h) g = unlines [[toChar (g M.!? V2 x y) | x <- [0..w-1]] | y <- [0..h-1]]
 where
   toChar (Just E) = '>'
   toChar (Just S) = 'v'
   toChar Nothing = '.'