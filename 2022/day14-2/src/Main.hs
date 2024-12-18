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

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4, both)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
import Control.Monad.State
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (toList)
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

type Input = [(V2 Int, V2 Int)]

-- type Output = Input

main :: IO ()
main =
  readFile "input" >>= print . solve . parse
  --readFile "input" >>= displayGrid . solveToGrid . parse

parse :: String -> Input
parse = concatMap parseLine . lines
 where
  parseLine (parseCoords -> coords) = zip coords (tail coords)
  parseCoords = map parseCoord . splitOn "->"
  parseCoord (splitOn "," -> [x, y]) = V2 (read x) (read y)
  parseCoord _ = error "parse error"

data CellType = Rock | Sand
  deriving (Eq, Show)

type Grid = Map (V2 Int) CellType

displayGrid :: Grid -> IO ()
displayGrid = display FullScreen black . gridToPicture

gridToPicture :: Grid -> Picture
gridToPicture grid = scale 1 (-1) $ pictures $ map toSquare (M.assocs grid)
 where
  toSquare (V2 (fromIntegral -> x) (fromIntegral -> y), cellType) = color (colorOf cellType) (translate x y (rectangleSolid 1 1))
  colorOf Rock = greyN 0.8
  colorOf Sand = yellow

sign :: (Num a1, Num a2, Ord a1) => a1 -> a2
sign n
  | n == 0 = 0
  | n < 0 = -1
  | otherwise = 1

toGrid :: Input -> Grid
toGrid input = M.fromList [(v, Rock) | seg <- input, v <- segmentToCoords seg]
 where
  segmentToCoords (v1, v2) =
    let delta = v2 - v1
        len = abs (sum delta) + 1
     in take len (iterate (+ fmap sign delta) v1)

down (V2 x y) = V2 x (y + 1)
downLeft (V2 x y) = V2 (x - 1) (y + 1)
downRight (V2 x y) = V2 (x + 1) (y + 1)

fall :: Grid -> Int -> V2 Int -> V2 Int
fall grid maxY pos = do
  if pos ^. _y == maxY
    then pos
    else case grid M.!? down pos of
      Nothing -> fall grid maxY (down pos)
      _ -> case grid M.!? downLeft pos of
        Nothing -> fall grid maxY (downLeft pos)
        _ -> case grid M.!? downRight pos of
          Nothing -> fall grid maxY (downRight pos)
          _ -> pos

addSand :: Int -> Grid -> Grid
addSand maxY grid = M.insert (fall grid maxY (V2 500 0)) Sand grid

addAllSand :: Int -> Grid -> Grid
addAllSand maxY grid =
  let grid' = addSand maxY grid
   in if M.member (V2 500 0) grid' then grid' else addAllSand maxY grid'

solveToGrid :: Input -> Grid
solveToGrid input = addAllSand maxY grid
 where
  grid = toGrid input
  maxY = maximum (input ^.. folded . both . _y) + 1

solve input = input & solveToGrid & M.elems & filter (== Sand) & length