-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, maximumOf, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Combinators (minimumOf)
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
import Data.Maybe (catMaybes, fromJust, maybeToList)
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
import Data.Generics.Product (types)

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

type Input = Set (V2 Int)

-- type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = toSet . (arrayFromList2D :: [String] -> UA.Array (V2 Int) Char) . lines
 where
  toSet a = S.fromList [p | (p, '#') <- UA.assocs a]

data Neighbors = N {n :: V2 Int, ne :: V2 Int, e :: V2 Int, se :: V2 Int, s :: V2 Int, sw :: V2 Int, w :: V2 Int, nw :: V2 Int}
  deriving (Eq, Show, Generic)

neighbors (V2 x y) =
  N
    (V2 x (y - 1))
    (V2 (x + 1) (y - 1))
    (V2 (x + 1) y)
    (V2 (x + 1) (y + 1))
    (V2 x (y + 1))
    (V2 (x - 1) (y + 1))
    (V2 (x - 1) y)
    (V2 (x - 1) (y - 1))

rules =
  [ ([n, ne, nw], V2 0 (-1))
  , ([s, se, sw], V2 0 1)
  , ([w, nw, sw], V2 (-1) 0)
  , ([e, ne, se], V2 1 0)
  ]

rotate rs = tail rs ++ [head rs]

proposal grid rs v
  | all empty (ns ^.. types @(V2 Int)) = Nothing
  | otherwise = (v +) <$> headMay [d | (c, d) <- rs, all (empty . ($ ns)) c]
 where
  ns = neighbors v
  empty cell = cell `S.notMember` grid

proposals grid rs = M.fromListWith (++) [(p, [v]) | v <- S.toList grid, p <- maybeToList (proposal grid rs v)]

-- only singletons
moves ps = [(v, p) | (p, [v]) <- M.toList ps]

applyMoves = foldr applyMove
 where
  applyMove (v, p) g = S.insert p (S.delete v g)

step (grid, rs) = (applyMoves grid (moves (proposals grid rs)), rotate rs)

bbox grid =
  ( V2 (minimum xs) (minimum ys)
  , V2 (maximum xs) (maximum ys)
  )
 where
  xs = grid ^.. folded . _x
  ys = grid ^.. folded . _y

inside (V2 x y) (V2 minx miny, V2 maxx maxy) = x >= minx && x <= maxx && y >= miny && y <= maxy

count grid = area - length grid
 where
  box@(V2 xmin ymin, V2 xmax ymax) = bbox grid
  area = (xmax - xmin + 1) * (ymax - ymin + 1)

display grid = unlines [[if V2 x y `S.member` grid then '#' else '.' | x <- [xmin .. xmax]] | y <- [ymin .. ymax]]
 where
  box@(V2 xmin ymin, V2 xmax ymax) = bbox grid

-- solve :: Input -> Output
solve input = iterate step (input, rules) !! 10 & fst & count
