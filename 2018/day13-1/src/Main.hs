-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE PatternSynonyms #-}
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

import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.Maybe (fromJust, catMaybes, mapMaybe)
import Data.List (sortOn, elemIndex, transpose, group, sort, tails, inits)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)

-- #endregion

type Input = UA.Array (V2 Int) Char


main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = arrayFromList2D (lines str)

data Cart = Cart { pos :: V2 Int, dir :: V2 Int, numIntersectionsSeen :: Int }
  deriving (Show)

initialCarts :: Input -> [Cart]
initialCarts = mapMaybe toCart . UA.assocs
 where
   toCart (pos, '>') = Just $ Cart pos (V2 1 0) 0
   toCart (pos, '<') = Just $ Cart pos (V2 (-1) 0) 0
   toCart (pos, '^') = Just $ Cart pos (V2 0 (-1)) 0
   toCart (pos, 'v') = Just $ Cart pos (V2 0 1) 0
   toCart _ = Nothing

turnLeft :: V2 Int -> V2 Int
turnLeft (V2 dx dy) = V2 dy (-dx)

turnRight :: V2 Int -> V2 Int
turnRight (V2 dx dy) = V2 (-dy) dx

pattern U = V2 0 (-1)
pattern D = V2 0 1
pattern R = V2 1 0
pattern L = V2 (-1) 0

step :: Input -> Cart -> Cart
step input (Cart pos dir num)
  | cell `elem` "|-<>^v" = Cart (pos+dir) dir num
  | cell == '/' =
      case dir of
        U -> go R
        L -> go D
        R -> go U
        D -> go L
  | cell == '\\' =
      case dir of
        U -> go L
        L -> go U
        R -> go D
        D -> go R
  | cell == '+' = Cart (pos+dirAtIntersection) dirAtIntersection (num+1)
  | otherwise = error "oops"
  where
    cell = input UA.! pos
    go newDir = Cart (pos+newDir) newDir num
    dirAtIntersection =
      dir & case num `mod` 3 of
        0 -> turnLeft
        1 -> id
        2 -> turnRight

tick :: Input -> [Cart] -> [[Cart]]
tick input carts = zipWith (<>) (inits (map (step input) sortedCarts)) (tails sortedCarts)
  where
    sortedCarts = sortOn pos carts

ticks :: Input -> [Cart] -> [[Cart]]
ticks input carts = res <> ticks input (last res)
 where
   res = tick input carts

solve input =
  initialCarts input
  & ticks input
  & concatMap (filter ((>1) . length) . group . sort . map pos)
  & head
  & head