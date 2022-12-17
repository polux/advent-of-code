-- Copyright 2022 Google LLC.
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

import Control.Arrow ((>>>),(***), Arrow (first, second))
import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4, maximumOf)
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
import Data.List (sortOn, elemIndex)
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

type Input = [Move]

type Output = Int

type Move = V2 Int

type Piece = Set (V2 Int)

pieces :: [Piece]
pieces = map S.fromList
  [
    [V2 x 0 | x <- [0..3]],
    [V2 1 0, V2 0 1, V2 1 1, V2 2 1, V2 1 2],
    [V2 x 0 | x <- [0..2]] ++ [V2 2 y | y <- [1..2]],
    [V2 0 y | y <- [0..3]],
    [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
  ]

type Board = Set (V2 Int)

maxY :: Board -> Int
maxY board = maximumDef (-1) (board ^.. folded . _y)

renderBoard :: Board -> String
renderBoard board = unlines [renderLine y | y <- [maxY board, maxY board -1..0]]
  where
    renderLine y = [if V2 x y `S.member` board then '#' else '.' | x <- [0..6]]

move :: V2 Int -> Piece -> Piece
move delta = S.map (+delta)

placePiece :: Board -> Piece -> Piece
placePiece board = move (V2 2 (maxY board + 4))

dump note x = trace ("\n--"++note++"---\n" ++ renderBoard x ++ "-----------\n") x

fall :: Piece -> [Move] -> Board -> (Board, [Move])
fall piece jet board = goWind jet (placePiece board piece)
  where
    goWind (j:js) piece = goGravity js (fromJustDef piece (tryMove j piece))
    goWind [] _ = error "no more wind"
    goGravity js piece =
      case tryMove (V2 0 (-1)) piece of
        Just piece' -> goWind js piece'
        Nothing -> (S.union piece board, js)
    tryMove delta piece = let piece' = move delta piece in if valid piece' then Just piece' else Nothing
    valid piece = S.null (S.intersection piece board) && all validCoord piece
    validCoord (V2 x y) = x >= 0 && x < 7 && y >= 0

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map toMove . head . lines
 where
  toMove '>' = V2 1 0
  toMove '<' = V2 (-1) 0
  toMove _ = error "parse error"

linesTopToBottom :: Board -> [[Int]]
linesTopToBottom board = [[x | x <- [0..6], V2 x y `S.member` board] | y <- [maxY board, maxY board-1..0]]

topLines :: Board -> [[Int]]
topLines board = takePrefix mempty (linesTopToBottom board)
 where
  takePrefix seen [] = []
  takePrefix seen (l:ls)
    | length seen == 7 = []
    | otherwise = l : takePrefix (S.union seen (S.fromList l)) ls

--solve :: Input -> Output
solve input =
  let (s,e) = findRecurringElement $ go (cycle input) (cycle pieces) mempty
      (quot,rem) = (1_000_000_000_000-s) `quotRem` (e-s)
      prefixHeight = maxY (playFor s)
      deltaHeight = maxY (playFor e) - prefixHeight
      suffixHeight = maxY (playFor (s+rem)) - prefixHeight
  in prefixHeight + quot*deltaHeight + suffixHeight + 1
  where
    playFor n = play (cycle input) (take n $ cycle pieces) mempty

    play [] _ _ = error "unexpected no more wind"
    play _ [] board = board
    play js (p:ps) board =
      let (board',js') = fall p js board
      in play js' ps board'

    go [] _ _ = error "unexpected no more wind"
    go _ [] board = error "unexpected no more pieces"
    go js (p:ps) board =
      let (board',js') = fall p js board
      in (p, take (length input) js, topLines board) : go js' ps board'

