-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

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
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe)
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

type Input = (UA.Array (V2 Int) Char, [Char])

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse (splitOn "\n\n" -> [before, after]) =
  ( arrayFromList2D (map (concatMap pre) (lines before))
  , concat (lines after)
  )
 where
  pre '#' = "##"
  pre 'O' = "[]"
  pre '@' = "@."
  pre '.' = ".."
  pre _ = error "pre"
parse _ = error "parse"

toDir '<' = V2 (-1) 0
toDir '>' = V2 1 0
toDir '^' = V2 0 (-1)
toDir 'v' = V2 0 1
toDir _ = error "parse dir"

pattern E = V2 1 0
pattern W = V2 (-1) 0

--solve (input, prog) = scanl step (initMap, initPos) prog & map display & unlines
solve :: Input -> Int
solve (input, prog) = foldl' step (initMap, initPos) prog & fst & score
 where
  initPos = head [p | (p, c) <- UA.assocs input, c == '@']
  initMap = M.fromList [(p, c) | (p, c) <- UA.assocs input, c `elem` "#[]"]
  score grid = sum [100 * y + x | (V2 x y, c) <- M.toList grid, c == '[']
  step (grid, pos) c =
    case move Nothing (pos + toDir c) (toDir c) grid of
      Nothing -> (grid, pos)
      Just grid' -> (grid', pos + toDir c)
  move mbChar pos dir grid =
    case grid M.!? pos of
      Nothing -> Just (grid & at pos .~ mbChar)
      Just '#' -> Nothing
      Just '[' -> do
        case dir of
          E -> do
            grid' <- move (Just ']') (pos + E + E) E grid
            pure (grid' & at pos .~ mbChar & at (pos + E) ?~ '[')
          _ -> do
            grid' <- move (Just ']') (pos + E + dir) dir grid
            grid'' <- move (Just '[') (pos + dir) dir grid'
            pure (grid'' & at pos .~ mbChar & at (pos + E) .~ Nothing)
      Just ']' -> do
        case dir of
          W -> do
            grid' <- move (Just '[') (pos + W + W) W grid
            pure (grid' & at pos .~ mbChar & at (pos + W) ?~ ']')
          _ -> do
            grid' <- move (Just '[') (pos + W + dir) dir grid
            grid'' <- move (Just ']') (pos + dir) dir grid'
            pure (grid'' & at pos .~ mbChar & at (pos + W) .~ Nothing)
      _ -> error "unexpected cell content"