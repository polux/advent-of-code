-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second, (&&&)), (***), (>>>))
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
import Data.List (elemIndex, group, sort, sortOn)
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
import Data.List (sortBy)
import Data.Ord (comparing)

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

type Input = [(Hand, Int)]

type Output = Input

order = M.fromList (zip (reverse "AKQJT98765432") [0 ..])

newtype Card = Card Char
  deriving stock (Eq)
  deriving newtype (Show)

instance Ord Card where
  compare (Card c1) (Card c2) = (order M.! c1) `compare` (order M.! c2)

type Hand = [Card]

data HandType = High | One | Two | Three | Full | Four | Five
  deriving (Eq, Show, Ord)

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine (words -> [hand, bid]) = (map Card hand, read bid)
  parseLine _ = error "parse error"

categorize :: Hand -> HandType
categorize hand =
  case sortOn length (group (sort hand)) of
    [[_, _, _, _, _]] -> Five
    [[_], [_, _, _, _]] -> Four
    [[_, _], [_, _, _]] -> Full
    [[_], [_], [_, _, _]] -> Three
    [[_], [_, _], [_, _]] -> Two
    [[_], [_], [_], [_, _]] -> One
    [[_], [_], [_], [_], [_]] -> High
    _ -> error "unknown hand type"

--solve :: Input -> Output
solve input =
  input
    & sortBy (comparing ((categorize &&& id) . fst))
    & map snd
    & zipWith (*) [1..]
    & sum

