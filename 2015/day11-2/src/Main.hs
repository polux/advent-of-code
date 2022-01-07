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
import Data.Char (chr, ord)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, group, sortOn)
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Safe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Input = String

type Output = Input

main :: IO ()
main = putStrLn (solve "vzbxkghb")

incr :: String -> String
incr str = reverse (go (reverse str))
  where
    go ('z' : cs) = 'a' : go cs
    go (c : cs) = chr (ord c + 1) : cs
    go [] = ['a']

valid :: String -> Bool
valid str = hasIncreasingSequence (map ord str) && noForbiddenLetter str && twoDifferentPairs str
  where
    hasIncreasingSequence (x : y : z : xs)
      | z == y + 1 && y == x + 1 = True
      | otherwise = hasIncreasingSequence (y : z : xs)
    hasIncreasingSequence _ = False
    noForbiddenLetter str = not (any (`elem` "iol") str)
    twoDifferentPairs str = length (filter ((> 1) . length) (group str)) >= 2

solve :: Input -> Output
solve input =
  input
    & iterate incr
    & filter valid
    & tail
    & head
