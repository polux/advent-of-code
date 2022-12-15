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

type Input = [(V2 Int, V2 Int)]

--type Output = Input
limit = 4_000_000

main :: IO ()
main =
  readFile "input" >>= print . solve . parse
  --readFile "example" >>= putStrLn .display . parse

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine
    ( splitOn "=" ->
        [ _
          , splitOn "," -> [read -> x1, _]
          , splitOn ":" -> [read -> y1, _]
          , splitOn "," -> [read -> x2, _]
          , read -> y2
          ]
      ) = (V2 x1 y1, V2 x2 y2)
  parseLine _ = error "parse error"

distance p1 p2 = sum (fmap abs (p2 - p1))

bounds (V2 sx sy, d) y = if dy > d then Nothing else Just (min x1 x2, max x1 x2)
  where
    dy = abs (y-sy)
    dx = d-dy
    x1 = sx-dx
    x2 = sx+dx

type Interval = (Int, Int)
lt (s1,e1) (s2,e2) = e1 < s2
overlap (s1,e1) (s2,e2) = not (s2>e1 || s1>e2)
fuse (s1,e1) (s2,e2) = (min s1 s2, max e1 e2)

insert :: Interval -> [Interval] -> [Interval]
insert i [] = [i]
insert i (j:js)
  | lt i j = i:j:js
  | overlap i j = insert (fuse i j) js
  | otherwise = j : insert i js

insideAtY :: [(V2 Int, Int)] -> Int -> [Interval]
insideAtY beacons y = foldr insert [] (catMaybes [bounds b y | b <- beacons])

display :: Input -> String
display input = unlines [show y ++ [if visible (V2 x y) then '#' else '.' | x <- [0..20]] | y <- [0..20]]
  where
    sensors = [(s, distance s b) | (s,b) <- input]
    visible p = or [distance p s <= d | (s,d) <- sensors]

findFirst :: [Interval] -> Maybe Int
findFirst is
  | (_,e1):_:_ <- filter (`overlap` (0,limit)) is = Just (e1+1)
  | otherwise = Nothing

--solve :: Input -> Output
solve input = solx * limit + soly
  where
    minX = traceShowId $ minimum [s^._x-d | (s,d) <- sensors]
    maxX = traceShowId $ maximum [s^._x+d | (s,d) <- sensors]
    sensors = [(s, distance s b) | (s,b) <- input]
    (solx,soly) = head $ catMaybes [(,y) <$> findFirst is | y <- [0..limit], let is = insideAtY sensors y]
