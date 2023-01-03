-- Copyright 2022 Google LLC.
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

import Control.Arrow (Arrow (first, second, (&&&)), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
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

type Input = UA.Array (V2 Int) Char

data Blizz = B {initPos :: Int, dir :: Int}
  deriving (Show)
type Column = [Blizz]
type Row = [Blizz]
type Spec = (Vector Row, Vector Column, V2 Int)
type Time = Int

type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . lines

vDir 'v' = 1
vDir '^' = -1
vDir _ = error "vDir parse error"

hDir '>' = 1
hDir '<' = -1
hDir _ = error "hDir parse error"

toRows :: Input -> Vector Row
toRows b =
  V.fromList
    [ [ B (x - 1) (hDir c)
      | x <- [1 .. w - 2]
      , let c = b UA.! V2 x y
      , c `elem` "><"
      ]
    | y <- [1 .. h - 2]
    ]
 where
  V2 w h = arraySize b

toColumns :: Input -> Vector Column
toColumns b =
  V.fromList
    [ [ B (y - 1) (vDir c)
      | y <- [1 .. h - 2]
      , let c = b UA.! V2 x y
      , c `elem` "v^"
      ]
    | x <- [1 .. w - 2]
    ]
 where
  V2 w h = arraySize b

toSpec :: Input -> Spec
toSpec input = (toRows input, toColumns input, arraySize input - V2 2 2)

free :: Spec -> Time -> V2 Int -> Bool
free (rows, cols, V2 w h) t (V2 x y) =
  (x == 0 && y == -1)
    || ( and [(x0 + dir * t) `mod` w /= x | B x0 dir <- rows V.! y]
          && and [(y0 + dir * t) `mod` h /= y | B y0 dir <- cols V.! x]
       )

data State = S {pos :: V2 Int, time :: Time}
  deriving (Show, Eq, Ord)

neighbors :: Spec -> State -> [State]
neighbors spec@(_, _, dims) (S pos time) = wait ++ moves
 where
  wait = [S pos time' | free spec time' pos]
  moves = [S pos' time' | pos' <- neighbors2D dims pos, free spec time' pos']
  time' = time + 1

isTarget :: Spec -> State -> Bool
isTarget (_, _, V2 w h) (S (V2 x y) _) = x == w - 1 && y == h - 1

initState :: State
initState = S (V2 0 (-1)) 0

-- solve :: Input -> Output
solve input = succ . time . last <$> bfs (neighbors spec) (isTarget spec) initState
 where
  spec = toSpec input
