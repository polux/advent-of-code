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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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
import Data.List (sortOn, elemIndex, foldl1')
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

data Tree = Pair Tree Tree | Number Int
  deriving (Show)

type Input = [Tree]

--type Output = Tree

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map parseTree . lines

parseTree :: String -> Tree
parseTree = runParserOrDie (treeParser <* P.eof)
 where
   treeParser = pairParser P.<|> numParser
   pairParser = do
     P.char '['
     left <- treeParser
     P.char ','
     right <- treeParser
     P.char ']'
     return (Pair left right)
   numParser = Number <$> L.decimal

data ExplodeResult = Exploded Tree Int Int | DidSetRight Tree Int | DidSetLeft Tree Int | Done Tree | NothingHappened
  deriving (Show)


pretty (Pair a b) = "[" ++ pretty a ++ "," ++ pretty b ++ "]"
pretty (Number i) = show i

depth (Pair a b) = 1 + max (depth a) (depth b)
depth (Number i) = 0

explode = toMaybe . go 0
 where
   toMaybe Exploded {} = error "unexpected"
   toMaybe (DidSetRight t _) = Just t
   toMaybe (DidSetLeft t _) = Just t
   toMaybe (Done t) = Just t
   toMaybe NothingHappened = Nothing

   addRightMost i (Pair a b) = Pair a (addRightMost i b)
   addRightMost i (Number j) = Number (i+j)

   addLeftMost i (Pair a b) = Pair (addLeftMost i a) b
   addLeftMost i (Number j) = Number (i+j)

   go _ (Number i) = NothingHappened
   go 4 (Pair (Number a) (Number b)) = Exploded (Number 0) a b
   go d (Pair a b) =
     case go (d+1) a of
       Done a' -> Done (Pair a' b)
       Exploded a' i j -> DidSetRight (Pair a' (addLeftMost j b)) i
       DidSetRight a' i -> DidSetRight (Pair a' b) i
       DidSetLeft a' j -> Done (Pair a' (addLeftMost j b))
       NothingHappened -> case go (d+1) b of
         Done b' -> Done (Pair a b')
         Exploded b' i j -> DidSetLeft (Pair (addRightMost i a) b') j
         DidSetRight b' i -> Done (Pair (addRightMost i a) b')
         DidSetLeft b' i -> DidSetLeft (Pair a b') i
         NothingHappened -> NothingHappened

split (Pair a b) =
  case split a of
    Just a' -> Just (Pair a' b)
    Nothing -> Pair a <$> split b
split (Number i)
  | i >= 10 = let (q,r) = divMod i 2 in Just (Pair (Number q) (Number (q+r)))
  | otherwise = Nothing

normalize tree =
  case explode tree of
    Just tree' -> normalize tree'
    Nothing -> maybe tree normalize (split tree)

mag (Pair a b) = 3 * mag a + 2 * mag b
mag (Number i) = i

fishSum n m = normalize (Pair n m)
score (n,m) = mag (fishSum n m)
swap (x,y) = (y,x)

allPairs (x:xs) = [(x,y) | y <- xs] ++ allPairs xs
allPairs _ = []

solve input = maximum (map score (allPairs input ++ map swap (allPairs input)))
