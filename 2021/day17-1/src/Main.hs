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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, view, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
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

type Input = (V2 Int, V2 Int)

type Output = V2 Int

target = (V2 195 (-93), V2 238 (-67))

toPoint (V2 x y) = (fromIntegral x, fromIntegral y)

main :: IO ()
main = play FullScreen black 0 (V2 1 0) display update (const id)
 where
  display v = pictures [speedVec, targetRec, trajectory, maxYText]
   where
    speedVec = line [(0, 0), toPoint v] & color white
    targetRec =
      let (toPoint -> (w, h)) = snd target - fst target
          (toPoint -> (x, y)) = fst target
       in rectangleSolid w h
            & translate (w / 2) (h / 2)
            & translate x y
            & color green
    trajectory =
      pictures
        [ circleSolid 1
          & translate x y
          & color (if inside pos then red else cyan)
        | pos <- take 1000 (map fst $ trace v)
        , let (x, y) = toPoint pos
        ]
    inside (V2 x y) =
      let (V2 x1 y1, V2 x2 y2) = target
       in x >= x1 && x <= x2 && y >= y1 && y <= y2
    maxY =
      trace v
        & take 100
        & map (view _y . fst)
        & maximum
    maxYText =
      text (show maxY)
        & scale (1 / 10) (1 / 10)
        & translate (-100) 100
        & color white
  trace v = iterate step (V2 0 0, v)
  step (pos, v@(V2 vx vy)) = (pos + v, V2 (drag vx) (vy -1))
  drag vx
    | vx > 0 = vx -1
    | vx < 0 = vx + 1
    | otherwise = 0
  update (EventKey (Char c) Up _ _) v =
    v
      + case c of
        'w' -> V2 0 1
        's' -> V2 0 (-1)
        'd' -> V2 1 0
        'a' -> V2 (-1) 0
        _ -> V2 0 0
  update _ v = v
