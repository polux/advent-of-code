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
import Data.Maybe (fromJust, catMaybes, isNothing)
import Data.List (sortOn, elemIndex, isInfixOf, (\\))
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)
import Control.Monad.Writer.Strict (Writer, execWriter, MonadWriter (tell))

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

type Input = Map String (Int, [String])

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = M.fromList . map parseLine . lines
 where
   parseLine str
     | "->" `isInfixOf` str  =
       let [prefix, suffix] = splitOn " -> " str
           (parent, num) = parseParent prefix
           children = splitOn ", " suffix
       in (parent, (num, children))
     | otherwise = let (parent, num) = parseParent str in (parent, (num, []))
   parseParent str =
     let [name, parens] = words str
     in (name, read (tail (init parens)))

weightedSiblings :: String -> Input -> [[(String, Int)]]
weightedSiblings root input = execWriter (go root)
  where
    go :: String -> Writer [[(String, Int)]] Int
    go node = do
      let (weight, children) = input M.! node
      childrenWeights <- mapM go children
      tell [zip children childrenWeights]
      return (weight + sum childrenWeights)

solve input = weightedSiblings root input & filter (notAllSame . map snd) & head
  where
    nodes = M.keys input
    withParents = input & M.elems & concatMap snd
    root = head (nodes \\ withParents)
    notAllSame xs = length (S.fromList xs) > 1

{-
output: [("vrgxe",2166),("shnqfh",2159),("auzded",2159),("hkhsc",2159),("jwddn",2159),("mcxki",2159),("lhwyt",2159)]

vrgxe weights 7 too many
weight of vrgxe is 1226 in input.txt
so its weight should be 1219

-}