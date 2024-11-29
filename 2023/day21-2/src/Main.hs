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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
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
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, mapMaybe)
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
--import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine

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

type Output = Input

main :: IO ()
main =
  -- readFile "example" >>= print . solve . parse
  do
    s <- readFile "input"
    let input = parse s
    display FullScreen black $ render input $ (!! 100) $ states input

-- readFile "example" >>= display FullScreen black . render . solve . parse
-- readFile "example" >>= (\fs -> animate FullScreen black (\i -> fs !! round i)). map render . states . parse

parse :: String -> Input
parse = arrayFromList2D . lines

render :: Input -> [V2 Int] -> Picture
render input vs =
  Pictures
    [ Pictures [rectangleSolid 1 1 & translate x y & color white | V2 (fromIntegral -> x) (fromIntegral -> y) <- vs]
    , Pictures
        [ rectangleSolid 1 1
          & translate (fromIntegral x) (fromIntegral y)
          & color red
        | (x, y) <- (,) <$> [-50 .. 200] <*> [-50 .. 200]
        , input UA.! wrap (V2 x y) == '#'
        ],
     Pictures
        [ rectangleSolid 1 1
          & translate (fromIntegral x) (fromIntegral y)
          & color blue
        | (x, y) <- (,) <$> [-50 .. 200] <*> [-50 .. 200]
        , input UA.! wrap (V2 x y) == 'S'
        ]
    ]
 where
  wrap (V2 x y) = V2 (x `mod` w) (y `mod` h)
  (V2 w h) = arraySize input

{-
   <> (line [(0,-100),(0,100)] & color red)
   <> (line [(11,-100),(11,100)] & color red)
   <> (line [(22,-100),(22,100)] & color red)
   <> (line [(33,-100),(33,100)] & color red)
   <> (line [(-11,-100),(-11,100)] & color red)
   <> (line [(-22,-100),(-22,100)] & color red)
   <> (line [(-100,0),(100,0)] & color red)
   <> (line [(-100,11),(100,11)] & color red)
   <> (line [(-100,22),(100,22)] & color red)
   <> (line [(-100,33),(100,33)] & color red)
   <> (line [(-100,-11),(100,-11)] & color red)
   <> (line [(-100,-22),(100,-22)] & color red)
-}

states input = iterate nextState [start]
 where
  start = input & UA.assocs & filter ((== 'S') . snd) & head & fst
  canMoveTo pos = (input UA.! wrap pos) `elem` ".S"
  nextPositions pos = neighbors pos & filter canMoveTo
  nextState poss = poss & concatMap nextPositions & S.fromList & S.toList
  wrap (V2 x y) = V2 (x `mod` w) (y `mod` h)
  (V2 w h) = arraySize input
  neighbors pos = [pos + delta | delta <- [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]]

solve input = states input & zip [0 ..] & filter (diamond . snd) & head & fst
 where
  V2 w h = arraySize input
  inside (V2 x y) = x >= 0 && x < w && y >= 0 && y < h
  diamond s = all (== squareAt (V2 0 0) s) [squareAt (V2 2 0) s, squareAt (V2 (-2) 0) s, squareAt (V2 0 2) s, squareAt (V2 0 (-2)) s]
  squareAt (V2 i j) s = s & map (+ V2 (-i * w) (-j * h)) & filter inside & S.fromList
