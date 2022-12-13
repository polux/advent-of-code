-- Copyright 2022 Google LLC.
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
{-# LANGUAGE InstanceSigs #-}
-- #endregion

module Main where

-- #region imports

import Control.Arrow ((>>>),(***), Arrow (first, second))
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
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortOn, elemIndex, sort)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_, unless, when)
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

data Tree = Leaf Int | Node [Tree]
  deriving (Show, Eq)

type Input = [Tree]

type Output = Int

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = runParserOrDie (inputParser <* do { P.space ; P.eof })
 where
   inputParser = P.many (treeParser <* P.space)
   treeParser = nodeParser P.<|> numParser
   nodeParser = do
     P.char '['
     children <- treeParser `P.sepBy` P.char ','
     P.char ']'
     return (Node children)
   numParser = Leaf <$> L.decimal

instance Ord Tree where
  compare :: Tree -> Tree -> Ordering
  compare (Node ts) (Node us) = compare ts us
  compare (Leaf n) (Leaf m) = compare n m
  compare (Leaf n) (Node ts) = compare (Node [Leaf n]) (Node ts)
  compare (Node ts) (Leaf m) = compare (Node ts) (Node [Leaf m])

div1 = Node [Node [Leaf 2]]
div2 = Node [Node [Leaf 6]]

solve :: Input -> Output
solve input = (l1+1) * (l1+1+l2+1)
  where
    sorted = sort input
    (length->l1, suffix) = span (<div1) sorted
    l2 = length (takeWhile (<div2) suffix)