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

import Algorithm.Search (aStar, dijkstra)
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isUpper)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, maximumBy, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import Data.MemoTrie
import Data.Ord (comparing)
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
import Graphics.Gloss
import Linear (V2 (..), _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Pos = V2 Int

type Input = UA.Array Pos Char

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . lines

data Location = Inside | Outside
  deriving (Eq, Ord, Show)

data Door = Door String Location
  deriving (Eq, Ord, Show)

type Labels = (Map Pos Door, Map Door Pos)

labels :: Input -> Labels
labels arr = (M.fromList posToDoor, M.fromList [(door, pos) | (pos, door) <- posToDoor])
  where
    posToDoor = concat [left, right, above, below]
    (V2 w h) = arraySize arr
    hTripletAt i j = (arr UA.! V2 i j, arr UA.! V2 (i + 1) j, arr UA.! V2 (i + 2) j)
    vTripletAt i j = (arr UA.! V2 i j, arr UA.! V2 i (j + 1), arr UA.! V2 i (j + 2))
    left = [(V2 (i + 2) j, Door [a, b] (if i == 0 then Outside else Inside)) | j <- [0 .. h -1], i <- [0 .. w -3], let (a, b, c) = hTripletAt i j, isUpper a, isUpper b, c == '.']
    right = [(V2 i j, Door [b, c] (if i == w -3 then Outside else Inside)) | j <- [0 .. h -1], i <- [0 .. w -3], let (a, b, c) = hTripletAt i j, a == '.', isUpper b, isUpper c]
    above = [(V2 i (j + 2), Door [a, b] (if j == 0 then Outside else Inside)) | i <- [0 .. w -1], j <- [0 .. h -3], let (a, b, c) = vTripletAt i j, isUpper a, isUpper b, c == '.']
    below = [(V2 i j, Door [b, c] (if j == h -3 then Outside else Inside)) | i <- [0 .. w -1], j <- [0 .. h -3], let (a, b, c) = vTripletAt i j, a == '.', isUpper b, isUpper c]

data State = State Pos Int
  deriving (Eq, Ord, Show)

neighbors :: Input -> Labels -> State -> [State]
neighbors arr (posToDoor, doorToPos) (State pos lvl) = doorNeighbors ++ dotNeighbors
  where
    doorNeighbors =
      case M.lookup pos posToDoor of
        Just (Door d Outside) | lvl > 0 ->
          case M.lookup (Door d Inside) doorToPos of
            Just pos' -> [State pos' (lvl -1)]
            Nothing -> []
        Just (Door d Inside) -> [State (doorToPos M.! Door d Outside) (lvl + 1)]
        _ -> []
    dotNeighbors = flip State lvl <$> [pos' | pos' <- neighbors2D (arraySize arr) pos, arr UA.! pos' == '.']

start :: Labels -> Pos
start (_, doorToPos) = doorToPos M.! Door "AA" Outside

end :: Labels -> Pos
end (_, doorToPos) = doorToPos M.! Door "ZZ" Outside

solve input = length (fromJust result) - 1
  where
    result = bfs (neighbors input lbls) (State (start lbls) 0) (== State (end lbls) 0)
    lbls = labels input
