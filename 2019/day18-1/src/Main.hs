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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- #endregion
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

-- #region imports

import Algorithm.Search (aStar)
import Control.Lens (at, each, folded, foldrOf, isn't, ix, to, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4, (^?!))
import Control.Lens.Extras (is)
import Control.Monad (forM, forM_, when)
import Control.Monad.State.Strict (MonadState (get), execState, gets, modify, runState)
import qualified Control.Monad.State.Strict
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.Bits (Bits (popCount, setBit), testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isAlpha, isLower, ord, toUpper)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, foldl', inits, sortOn, tails)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
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
import Text.Regex.PCRE (RegexOptions (setExecOpts), (=~))
import Util

-- #endregion

type Pos = V2 Int

type Input = UA.Array Pos Char

data State = State {pos :: Pos, keys :: CharSet}
  deriving (Eq, Ord, Show)

type Output = Int

type CharSet = Int

hasChar :: CharSet -> Char -> Bool
hasChar set c = testBit set (ord c - ord 'A')

insertChar :: CharSet -> Char -> CharSet
insertChar set c = setBit set (ord c - ord 'A')

numChars :: CharSet -> Int
numChars = popCount

includes :: CharSet -> CharSet -> Bool
includes a b = a .&. b == b

fromChars :: [Char] -> CharSet
fromChars = foldr (flip insertChar) 0

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . lines

data Cell = Source | Wall | Floor | KeyFor Char | Door Char
  deriving (Eq, Ord, Show, Generic)

toCell :: Char -> Cell
toCell '@' = Source
toCell '.' = Floor
toCell '#' = Wall
toCell c
  | isLower c = KeyFor (toUpper c)
  | otherwise = Door c

cellAt :: Input -> Pos -> Cell
cellAt arr pos = toCell (arr UA.! pos)

neighborCells :: Input -> Pos -> [Pos]
neighborCells arr pos = filter (isn't #_Wall . cellAt arr) (neighbors2D (arraySize arr) pos)

type KeyGraph = Map Pos (Map Pos (Int, CharSet))

keyGraph :: Input -> Map Pos (Map Pos (Int, CharSet))
keyGraph arr = M.unionsWith M.union assocs
  where
    assocs = do
      (p1 : ps) <- tails pois
      p2 <- ps
      case shortestPath p1 p2 of
        Just path ->
          let summary = summarize path
           in [ M.singleton p1 (M.singleton p2 summary),
                M.singleton p2 (M.singleton p1 summary)
              ]
        Nothing -> []
    shortestPath p1 p2 = bfs (neighborCells arr) p1 (== p2)
    summarize path = (length path - 1, fromChars (path ^.. folded . to (cellAt arr) . #_Door))
    pois = [pos | (pos, toCell -> c) <- UA.assocs arr, is #_KeyFor c || is #_Source c]

reachableKeys :: KeyGraph -> Pos -> CharSet -> [Pos]
reachableKeys graph pos keys =
  M.findWithDefault mempty pos graph
    & M.filter (\(_, doors) -> keys `includes` doors)
    & M.keys

reachableStates :: Input -> KeyGraph -> State -> [State]
reachableStates arr tree (State pos keys) = map toState (reachableKeys tree pos keys)
  where
    toState newPos =
      State newPos $
        case cellAt arr newPos of
          KeyFor c -> insertChar keys c
          _ -> keys

costBetweenStates :: KeyGraph -> State -> State -> Int
costBetweenStates graph (State p1 _) (State p2 _) = graph ^?! ix p1 . ix p2 . _1

costOfState :: Int -> State -> Int
costOfState numKeys (State _ keys) = numKeys - numChars keys

isFinalState :: Int -> State -> Bool
isFinalState numKeys (State _ keys) = numKeys == numChars keys

initState :: Pos -> State
initState sourcePos = State sourcePos 0

prettyHistory :: Input -> [State] -> [Char]
prettyHistory arr = map toChar
  where
    toChar (State pos _) = arr UA.! pos

solve :: UA.Array Pos Char -> Maybe (Int, [Char])
solve arr =
  fmap (prettyHistory arr)
    <$> aStar
      (reachableStates arr graph)
      (costBetweenStates graph)
      (costOfState numKeys)
      (isFinalState numKeys)
      (initState sourcePos)
  where
    graph = keyGraph arr
    sourcePos = head [pos | (pos, toCell -> Source) <- UA.assocs arr]
    numKeys = length (filter (is #_KeyFor) (map toCell (UA.elems arr)))