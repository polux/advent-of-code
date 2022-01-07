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
-- #endregion

module Main where

-- #region imports

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)

-- #endregion

type Input = [(Int, Int)] -- period, initial value

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
   parseLine str =
     let [[_, period, initialValue]] = str =~ "Disc #. has (\\d+) positions; at time=0, it is at position (\\d+)"
     in (read period, read initialValue)

valid input n = all (==0) (zipWith valueAt [n+1,n+2..] input)
 where
   valueAt t (period, initialValue) = (initialValue + t) `mod` period

solve :: Input -> Output
solve input = sieve (toChineseRemainderProblem (input <> [(11,0)]))

toChineseRemainderProblem :: Input -> [(Int, Int)]
toChineseRemainderProblem = zipWith toEquation [1..]
 where
   toEquation discNum (period, initialValue) = ((period - initialValue - discNum) `mod` period, period)

sieve :: [(Int, Int)] -> Int
sieve [] = error "empty list"
sieve ((a1, n1):eqs) = go eqs a1 n1
 where
   go ((a, n):eqs) x ns = go eqs ([x,x+ns..] & filter ((==a).(`mod` n)) & head) (n*ns)
   go [] x ns = x

{-
Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.

I'm looking for the first t such that

t = 0 modulo 5
t = 1 modulo 2


Disc #1 has 13 positions; at time=0, it is at position 11.
Disc #2 has 5 positions; at time=0, it is at position 0.
Disc #3 has 17 positions; at time=0, it is at position 11.
Disc #4 has 3 positions; at time=0, it is at position 0.
Disc #5 has 7 positions; at time=0, it is at position 2.
Disc #6 has 19 positions; at time=0, it is at position 17.

I'm looking for thr first t such that

#1 t = 1 modulo 13     1 = 13-11-1
#2 t = 3 modulo 5      3 = 5-0-2
#3 t = 3 modulo 17     3 = 17-11-3
#4 t = 2 modulo 3      2 = (3-0-4) % 3
#5 t = 0 modulo 7      0 = 7-2-5

generally:

  t = ((period - initialValue - disc number) % period) modulo period

-}