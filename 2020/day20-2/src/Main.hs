-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (each, folded, isn't, traversed, view, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (guard)
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
import Data.List (transpose, (\\))
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Safe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Input = [Tile]

type Coord = V2 Int

type Bitmap = UA.UArray Coord Bool

type Tile = (Int, Bitmap)

type Solution = Map Coord Tile

type Output = [Int]

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = map parseTile (splitOn "\n\n" str)
  where
    parseTile :: String -> Tile
    parseTile (lines -> (title : bitmap)) = (parseId title, arrayFromList2D (map (map (== '#')) bitmap))
    parseId :: String -> Int
    parseId str =
      let [[_, id]] = str =~ "Tile ([0-9]+):"
       in read id

type Border = UA.Array Int Bool

data Borders = Borders {north :: Border, east :: Border, south :: Border, west :: Border}

borders :: Bitmap -> Borders
borders bitmap =
  Borders
    (arrayFromList1D [bitmap UA.! V2 i 0 | i <- [0 .. w - 1]])
    (arrayFromList1D [bitmap UA.! V2 (w - 1) j | j <- [0 .. w - 1]])
    (arrayFromList1D [bitmap UA.! V2 i (w - 1) | i <- [0 .. w - 1]])
    (arrayFromList1D [bitmap UA.! V2 0 j | j <- [0 .. w - 1]])
  where
    (V2 w _) = arraySize bitmap

rotate90 :: Bitmap -> Bitmap
rotate90 bitmap = UA.ixmap (UA.bounds bitmap) newIdx bitmap
  where
    newIdx (V2 i j) = V2 j (w - 1 - i)
    (V2 w _) = arraySize bitmap

mirror :: Bitmap -> Bitmap
mirror bitmap = UA.ixmap (UA.bounds bitmap) newIdx bitmap
  where
    newIdx (V2 i j) = V2 (w - 1 - i) j
    (V2 w _) = arraySize bitmap

variants :: Bitmap -> [Bitmap]
variants bitmap = rotations ++ map mirror rotations
  where
    rotations = take 4 (iterate rotate90 bitmap)

fits :: Solution -> Coord -> Tile -> Bool
fits tiling (V2 i j) (_, bitmap) =
  matches (V2 i (j - 1)) north south
    && matches (V2 (i + 1) j) east west
    && matches (V2 i (j + 1)) south north
    && matches (V2 (i - 1) j) west east
  where
    matches coord selector otherSelector =
      case tiling M.!? coord of
        Just (_, otherBitmap) -> otherSelector (borders otherBitmap) == selector (borders bitmap)
        Nothing -> True

glue :: Solution -> Bitmap
glue solution =
  UA.array
    (V2 0 0, V2 (width - 1) (width - 1))
    [(V2 i j, get i j) | i <- [0 .. width - 1], j <- [0 .. width - 1]]
  where
    width = 12 * 8
    get i j = snd (solution M.! V2 (i `div` 8) (j `div` 8)) UA.! V2 (1 + i `mod` 8) (1 + j `mod` 8)

coordsThatAreOn :: Bitmap -> Set Coord
coordsThatAreOn bitmap =
  bitmap
    & UA.assocs
    & filter snd
    & map fst
    & S.fromList

roughness :: Bitmap -> Int
roughness bitmap = length (coordsThatAreOn bitmap `S.difference` coordsPartOfSnake)
  where
    snake :: Set Coord
    snake =
      [ "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
      ]
        & map (map (== '#'))
        & arrayFromList2D
        & coordsThatAreOn
    coordsPartOfSnake :: Set Coord
    coordsPartOfSnake =
      [S.map (+ coord) snake | coord <- UA.indices bitmap]
        & filter bitmapContainsPattern
        & S.unions
    bitmapContainsPattern :: Set Coord -> Bool
    bitmapContainsPattern = all isOn
      where
        isOn coord = withinBounds coord && bitmap UA.! coord
        withinBounds (V2 i j) = i < w && j < h
        (V2 w h) = arraySize bitmap

solve :: Input -> Output
solve input =
  [V2 i j | i <- [0 .. 11], j <- [0 .. 11]]
    & assemble (S.fromList input) mempty
    & head
    & glue
    & variants
    & map roughness
  where
    assemble :: Set Tile -> Solution -> [Coord] -> [Solution]
    assemble tiles acc [] = return acc
    assemble tiles acc (cell : cells) = do
      tile <- S.toList tiles
      variant <- (fst tile,) <$> variants (snd tile)
      guard (fits acc cell variant)
      assemble (S.delete tile tiles) (M.insert cell variant acc) cells