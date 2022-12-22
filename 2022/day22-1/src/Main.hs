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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Combinators (view)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isDigit)
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

data Instr = Move Int | L | R
  deriving (Show)
type Board = UA.Array (V2 Int) Char
type Input = (Board, [Instr])

-- type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse (splitOn "\n\n" -> [parseBoard -> board, parseInstrs.head.lines -> instrs]) = (board, instrs)
parseBoard :: String -> UA.Array (V2 Int) Char
parseBoard str = arrayFromList2D (map (pad len) ls)
 where
  ls = lines str
  len = maximum (map length ls)
  pad l (c : cs) = c : pad (l - 1) cs
  pad l [] = replicate l ' '
parseInstrs :: String -> [Instr]
parseInstrs [] = []
parseInstrs (span isDigit -> (ds, [])) = [Move (read ds)]
parseInstrs (span isDigit -> (ds, t : is)) = Move (read ds) : parseTurn t : parseInstrs is
parseTurn :: Char -> Instr
parseTurn 'R' = R
parseTurn 'L' = L

data Cell = Open | Wall | Door (V2 Int)
  deriving (Show, Eq)
type MBoard = Map (V2 Int) Cell

doors :: Board -> [(V2 Int, V2 Int)]
doors b =
  [(V2 i1 j, V2 i2 j) | j <- [0 .. h - 1], let (i1, i2) = hDoors j]
    ++ [(V2 i j1, V2 i j2) | i <- [0 .. w - 1], let (j1, j2) = vDoors i]
 where
  (V2 w h) = arraySize b
  hDoors j =
    let i1 = head [i | i <- [-1 ..], b UA.! V2 (i + 1) j /= ' ']
        i2 = head [i | i <- [i1 + 1 ..], i == w || b UA.! V2 i j == ' ']
     in (i1, i2)
  vDoors i =
    let j1 = head [j | j <- [-1 ..], b UA.! V2 i (j + 1) /= ' ']
        j2 = head [j | j <- [j1 + 1 ..], j == h || b UA.! V2 i j == ' ']
     in (j1, j2)

toMBoard :: Board -> MBoard
toMBoard b =
  M.fromList $
    concat
      [ [(v, if c == '#' then Wall else Open) | (v, c) <- UA.assocs b, c /= ' ']
      , [(c1, Door c2) | (c1, c2) <- doors b]
      , [(c2, Door c1) | (c1, c2) <- doors b]
      ]

startingPos :: MBoard -> V2 Int
startingPos b = V2 (minimum [i | (V2 i 0, Open) <- M.toList b]) 0

pattern E = V2 1 0
pattern S = V2 0 1
pattern W = V2 (-1) 0
pattern N = V2 0 (-1)

data State = State {pos :: V2 Int, dir :: V2 Int}
  deriving (Eq, Ord, Show)

turn R (State pos (V2 x y)) = State pos (V2 (-y) x)
turn L (State pos (V2 x y)) = State pos (V2 y (-x))
fwd b (State pos dir) = flip State dir <$> check pos'
 where
  pos' = tele dir (pos + dir)
  tele d p | Door p' <- b M.! p = p' + d
           | otherwise = p
  check p | Open <- b M.! p = Just p
          | otherwise = Nothing

value (State (V2 i j) dir) = (1_000 * (j + 1)) + 4 * (i + 1) + facing dir
 where
  facing E = 0
  facing S = 1
  facing W = 2
  facing N = 3

exec :: MBoard -> [Instr] -> State -> State
--exec _ is s | traceShow (s, headMay is) False = undefined
exec b (Move 0 : is) s = exec b is s
exec b (Move n : is) s = maybe (exec b is s) (exec b (Move (pred n): is)) (fwd b s)
exec b (i : is) s = exec b is (turn i s)
exec b [] s = s

-- solve :: Input -> Output
solve (b, is) = value $ exec mb is initState
  where
    mb = toMBoard b
    initState = State (startingPos mb) E
