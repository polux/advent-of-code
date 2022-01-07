-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?))
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
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
import Data.List.Split (splitOn)

-- #endregion


type Interval = (Int, Int)
type Input = [Interval]


main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
   parseLine str =
     let [l,r] = splitOn "-" str
     in (read l, read r)

-- >>> insert (1,2) [(3,5), (7,9)]
-- [(1,2),(3,5),(7,9)]
-- >>> insert (1,4) [(3,5), (7,9)]
-- [(1,5),(7,9)]
-- >>> insert (4,5) [(3,5), (7,9)]
-- [(3,5),(7,9)]
-- >>> insert (4,6) [(3,5), (7,9)]
-- [(3,6),(7,9)]
-- >>> insert (4,8) [(3,5), (7,9)]
-- [(3,9)]
-- >>> insert (4,10) [(3,5), (7,9)]
-- [(3,10)]
-- >>> insert (6,8) [(3,5), (7,9)]
-- [(3,5),(6,9)]
insert :: Interval -> [Interval] -> [Interval]
insert iv [] = [iv]
insert (l1, r1) (iv@(l2, r2):ivs)
  | l1 > r2 = iv : insert (l1, r1) ivs
  | r1 < l2 = (l1, r1) : iv : ivs
  | otherwise = insertRight (min l1 l2) r1 (iv:ivs)
  where
    insertRight l1 r1 (iv@(l2, r2):ivs)
      | r1 < l2 = (l1,r1) : iv : ivs
      | r1 <= r2 = (l1, r2) : ivs
      | otherwise = insertRight l1 r1 ivs
    insertRight l1 r1 [] = [(l1, r1)]

-- >>> count (-1) [(0,12), (13,4294967295)]
-- 0
-- >>> count (-1) [(0,12), (14,4294967295)]
-- 1
-- >>> count (-1) [(0,12), (15,4294967295)]
-- 2
-- >>> count (-1) [(0,12), (15,4294967294)]
-- 3
count :: Int -> [Interval] -> Int
count n [] = 4294967296-n-1
count n ((l,r):iv) = (l-n-1) + count r iv

solve input =
  foldr insert [] input
  & count (-1)
