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
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Applicative (Alternative (empty, (<|>)))
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
import Data.Functor (($>), (<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, foldl', sortOn)
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
import Linear.V3 (V3 (V3))
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

data Action = On | Off
  deriving (Show)
data Instruction = Instr
  { action :: Action
  , minx :: Int
  , maxx :: Int
  , miny :: Int
  , maxy :: Int
  , minz :: Int
  , maxz :: Int
  }
  deriving (Show)

type Input = [Instruction]

type Output = Int

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map (runParserOrDie instructionParser) . lines
 where
  signedDecimalParser = L.signed P.space L.decimal
  rangeParser = do
    min <- signedDecimalParser
    P.string ".."
    max <- signedDecimalParser
    return (min, max)
  instructionParser = do
    action <- P.string "on" $> On <|> P.string "off" $> Off
    P.string " x="
    (minx, maxx) <- rangeParser
    P.string ",y="
    (miny, maxy) <- rangeParser
    P.string ",z="
    (minz, maxz) <- rangeParser
    return Instr{..}

solve :: Input -> Output
solve input = foldl' apply mempty input & length
 where
  op On = S.union
  op Off = S.difference
  apply set Instr{..} =
    op
      action
      set
      ( S.fromList
          [ V3 x y z
          | x <- [max minx (-50) .. min maxx 50]
          , y <- [max miny (-50) .. min maxy 50]
          , z <- [max minz (-50) .. min maxz 50]
          ]
      )
