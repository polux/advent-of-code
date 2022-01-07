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
import Control.Lens (at, each, folded, isn't, ix, traversed, use, uses, view, (%~), (&), (.~), (?=), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, when)
import Control.Monad.State (State, execState)
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

data Line = HLine Int Int Int | VLine Int Int Int
  deriving (Show)

type Input = [Line]

type Output = Int

main :: IO ()
main =
  getContents >>= print . solve . parse
  --getContents >>= display FullScreen black . gridToPicture . (\g -> execState (flowDown (maxYValue g) (V2 500 0)) g) . toGrid . parse

parse :: String -> Input
parse = map parseLine . lines
  where
    parseLine (splitOn ", " -> [lhs, rhs]) =
      let [d1, read -> v1] = splitOn "=" lhs
          [d2, range] = splitOn "=" rhs
          [read -> v2, read -> v3] = splitOn ".." range
       in (if d1 == "x" then HLine else VLine) v1 v2 v3

toPicture :: Input -> Picture
toPicture lines = scale 1 (-1) $ color white $ pictures (map toLine lines)
  where
    toLine (HLine x y1 y2) = line [(fromIntegral x, fromIntegral y1), (fromIntegral x, fromIntegral y2)]
    toLine (VLine y x1 x2) = line [(fromIntegral x1, fromIntegral y), (fromIntegral x2, fromIntegral y)]

data CellType = Clay | StillWater | FlowingWater
  deriving (Eq, Show)

type Grid = Map (V2 Int) CellType

toGrid :: Input -> Grid
toGrid lines = M.unions (map lineToGrid lines)
  where
    lineToGrid (HLine x y1 y2) = M.fromList [(V2 x y, Clay) | y <- [y1 .. y2]]
    lineToGrid (VLine y x1 x2) = M.fromList [(V2 x y, Clay) | x <- [x1 .. x2]]

gridToPicture :: Grid -> Picture
gridToPicture grid = scale 1 (-1) $ pictures $ map toSquare (M.assocs grid)
  where
    toSquare (V2 (fromIntegral -> x) (fromIntegral -> y), cellType) = color (colorOf cellType) (translate x y (rectangleSolid 1 1))
    colorOf Clay = white
    colorOf StillWater = blue
    colorOf FlowingWater = cyan

clayYs :: Grid -> [Int]
clayYs grid = M.filter (==Clay) grid & M.keys & map (view _y)

flowDown :: Int -> V2 Int -> State Grid Bool
flowDown _ cell | traceShow ("down", cell) False = undefined
flowDown maxY (V2 _ y) | y > maxY = return False
flowDown maxY cell = do
  ty <- use (at cell)
  case ty of
    Nothing -> do
      still <- flowDown maxY (cell + V2 0 1)
      if still
        then do
          left <- flowSideways maxY (V2 (-1) 0) (cell + V2 (-1) 0)
          right <- flowSideways maxY (V2 1 0) (cell + V2 1 0)
          case (left, right) of
            (Just x1, Just x2) -> do
              [x1+1 .. x2-1] `forM_` \x ->
                at (V2 x (view _y cell)) ?= StillWater
              return True
            _ -> do
              at cell ?= FlowingWater
              return False
        else do
          at cell ?= FlowingWater
          return False
    Just FlowingWater -> return False
    _ -> return True

flowSideways :: Int -> V2 Int -> V2 Int -> State Grid (Maybe Int)
flowSideways maxY delta cell | traceShow ("side", delta, cell) False = undefined
flowSideways maxY delta cell = do
  ty <- use (at cell)
  case ty of
    Just Clay -> return (Just (view _x cell))
    Just FlowingWater -> return Nothing
    Just StillWater -> error "wtf"
    Nothing -> do
      at cell ?= FlowingWater
      stillBelow <- flowDown maxY (cell + V2 0 1)
      if stillBelow
        then flowSideways maxY delta (cell + delta)
        else return Nothing

flowLeft :: V2 Int -> State Grid (Maybe Int)
flowLeft = error "not implemented"

solve :: Input -> Output
solve input =
  execState (flowDown maxY (V2 500 0)) grid
  & M.filter (==StillWater)
  & M.filterWithKey (\(V2 _ y) _ -> y >= minY)
  & length
 where
   grid = toGrid input
   clayYs = M.filter (==Clay) grid & M.keys & map (view _y)
   maxY = maximum clayYs
   minY = minimum clayYs
