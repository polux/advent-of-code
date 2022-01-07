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

import Control.Arrow (Arrow ((***)))
import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import Control.Monad.Writer
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import Data.MemoTrie
import Data.Monoid (Sum)
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
import Linear (V2 (..), (*^), _x, _y)
import Safe
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

data Status = Weakened | Infected | Flagged
  deriving (Eq)

type Input = (V2 Int, Map (V2 Int) Status)

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = (startingPos, UA.assocs arr & filter ((== '#') . snd) & map (mirror.fst) & map (,Infected) & M.fromList)
  where
    arr :: UA.Array (V2 Int) Char
    arr = arrayFromList2D (lines str)
    (V2 w h) = arraySize arr
    startingPos = V2 (w `div` 2) (h `div` 2)
    mirror (V2 x y) = V2 x (h - y -1)

data St = St {grid :: Map (V2 Int) Status, pos :: V2 Int, dir :: V2 Int}

pretty :: St -> String
pretty (St set pos dir) = show (pos, dir) <> "\n" <> unlines [prettyRow j | j <- [10, 9 .. (-10)]]
  where
    prettyRow j = concat [prettyCell i j | i <- [-10 .. 10]]
    prettyCell i j =
      let content = case set M.!? V2 i j of
            Nothing -> '.'
            Just Weakened -> 'W'
            Just Infected -> '#'
            Just Flagged -> 'F'
       in if V2 i j == pos then ['[', content, ']'] else [' ', content, ' ']

right (V2 x y) = V2 y (- x)

left (V2 x y) = V2 (- y) x

flipCell :: V2 Int -> Map (V2 Int) Status -> Map (V2 Int) Status
flipCell = M.alter next
 where
   next Nothing = Just Weakened
   next (Just Weakened) = Just Infected
   next (Just Infected) = Just Flagged
   next (Just Flagged) = Nothing

step :: St -> Writer (Sum Int) St
step (St grid pos dir) = do
  when (grid M.!? pos == Just Weakened) (tell (Sum 1))
  return (St newGrid newPos newDir)
  where
    newDir =
      dir & case grid M.!? pos of
        Nothing -> left
        Just Weakened -> id
        Just Infected -> right
        Just Flagged -> ((-1) *^)
    newPos = pos + newDir
    newGrid = flipCell pos grid

solve :: Input -> Output
solve (startPos, startGrid) = go 10000000 (St startGrid startPos (V2 0 1)) & execWriter & getSum
  where
    go :: Int -> St -> Writer (Sum Int) ()
    --go _ st | trace (pretty st <> "\n") False = undefined
    go 0 st = return ()
    go n st = go (n -1) =<< step st
