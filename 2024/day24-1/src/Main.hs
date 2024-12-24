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

{-# HLINT ignore "Move reverse out" #-}
-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, isPrefixOf, sortOn)
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

data Expr = Lit Bool | Or String String | And String String | Xor String String
  deriving (Eq, Ord, Show)
type Input = Map String Expr

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse (splitOn "\n\n" -> [pre, post]) = M.union (parsePre pre) (parsePost post)
 where
  parsePre (lines -> seeds) = M.unions (map parseSeed seeds)
  parseSeed (splitOn ": " -> [gate, num]) = M.singleton gate (Lit (num == "1"))
  parsePost (lines -> rules) = M.unions (map parseRule rules)
  parseRule (words -> [g1, op, g2, _, g3]) = M.singleton g3 (parseOp op g1 g2)
  parseOp "OR" = Or
  parseOp "XOR" = Xor
  parseOp "AND" = And

solve input =
  input
    & M.keys
    & reverse
    & filter ("z" `isPrefixOf`)
    & map eval
    & toNum 0
 where
  meval = memo eval

  eval x = evalExpr (input M.! x)

  evalExpr (Lit b) = b
  evalExpr (And x1 x2) = meval x1 && meval x2
  evalExpr (Or x1 x2) = meval x1 || meval x2
  evalExpr (Xor x1 x2) = meval x1 `xor` meval x2

  toNum acc [] = acc
  toNum acc (b : bs) = toNum (acc * 2 + if b then 1 else 0) bs
