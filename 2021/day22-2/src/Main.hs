-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (Lens', at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import GHC.RTS.Flags (ProfFlags (modSelector))
import Linear (V2 (..), _x, _y)
import Linear.V3 (V3 (V3))
import Safe hiding (at)
import Test.QuickCheck (Positive (Positive))
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
type Input = [Instruction]
type Output = Int

data Action = On | Off
  deriving (Show)

data Range = Range {low :: Int, high :: Int}
  deriving (Show, Eq, Generic)

data Box = Box
  { x :: Range
  , y :: Range
  , z :: Range
  }
  deriving (Show, Eq, Generic)

data Instruction = Instr
  { action :: Action
  , box :: Box
  }
  deriving (Show)

data Dimension = X | Y | Z
  deriving (Show, Eq)

-- in KDSplit d i t1 t2, t1 is <= i and t2 > i
data KDTree = KDLeaf Bool | KDSplit Dimension Int KDTree KDTree
  deriving (Show, Eq)

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map (runParserOrDie instructionParser) . lines
 where
  signedDecimalParser = L.signed P.space L.decimal
  rangeParser = do
    _ <- P.oneOf "xyz"
    _ <- P.char '='
    min <- signedDecimalParser
    P.string ".."
    max <- signedDecimalParser
    return (Range min max)
  instructionParser = do
    action <- P.string "on" $> On <|> P.string "off" $> Off
    P.space
    [x,y,z] <- rangeParser `P.sepBy` P.char ','
    return Instr{action, box = Box{..}}

solve :: Input -> Output
solve input = foldl' apply (KDLeaf False) input & kdVolume
 where
  apply tree (Instr On b) = kdInsert tree b
  apply tree (Instr Off b) = kdRemove tree b

maxBox :: Box
maxBox = Box maxRange maxRange maxRange
 where
  maxRange = Range minBound maxBound

rangeAt :: Dimension -> Lens' Box Range
rangeAt X = #x
rangeAt Y = #y
rangeAt Z = #z

lowAt :: Dimension -> Lens' Box Int
lowAt dim = rangeAt dim . #low

highAt :: Dimension -> Lens' Box Int
highAt dim = rangeAt dim . #high

kdModify :: Bool -> KDTree -> Box -> KDTree
kdModify mode tree box = go maxBox tree
 where
  go bbox (KDLeaf b)
    | b == mode = KDLeaf mode
    | otherwise = carve bbox
  go bbox (KDSplit dim i l r)
    | i < box ^. lowAt dim = KDSplit dim i l (go (bbox & lowAt dim .~ i + 1) r)
    | i >= box ^. highAt dim = KDSplit dim i (go (bbox & highAt dim .~ i) l) r
    | otherwise = KDSplit dim i (go (bbox & highAt dim .~ i) l) (go (bbox & lowAt dim .~ i + 1) r)
  carve = carveLow [X, Y, Z]
  carveLow [] bbox = KDLeaf mode
  carveLow (dim : dims) bbox
    | box ^. lowAt dim > bbox ^. lowAt dim && box ^. lowAt dim <= bbox ^. highAt dim =
      KDSplit dim (box ^. lowAt dim - 1) (KDLeaf (not mode)) (carveHigh dim dims (bbox & lowAt dim .~ (box ^. lowAt dim)))
    | otherwise = carveHigh dim dims bbox
  carveHigh dim dims bbox
    | box ^. highAt dim >= bbox ^. lowAt dim && box ^. highAt dim < bbox ^. highAt dim =
      KDSplit dim (box ^. highAt dim) (carveLow dims (bbox & highAt dim .~ (box ^. highAt dim))) (KDLeaf (not mode))
    | otherwise = carveLow dims bbox

kdInsert :: KDTree -> Box -> KDTree
kdInsert = kdModify True

kdRemove :: KDTree -> Box -> KDTree
kdRemove = kdModify False

boxVolume :: Box -> Int
boxVolume (Box (Range minx maxx) (Range miny maxy) (Range minz maxz)) = (maxx - minx + 1) * (maxy - miny + 1) * (maxz - minz + 1)

kdVolume :: KDTree -> Int
kdVolume = go maxBox
 where
  go bbox (KDLeaf True) = boxVolume bbox
  go _ (KDLeaf False) = 0
  go bbox (KDSplit dim i l r)
    | i < bbox ^. lowAt dim || i > bbox ^. highAt dim = go bbox l + go bbox r
    | otherwise = go (bbox & highAt dim .~ i) l + go (bbox & lowAt dim .~ i + 1) r
