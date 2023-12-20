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
import Data.List (elemIndex, intercalate, sortOn)
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

data ModuleType = TBroadcast | TFlipFlop | TConj
  deriving (Show)

type Input = [(ModuleType, String, [String])]

type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine (splitOn " -> " -> [parseLhs -> (typ, n), splitOn ", " -> ns]) = (typ, n, ns)
  parseLine _ = error "parse error"
  parseLhs ('%' : s) = (TFlipFlop, s)
  parseLhs ('&' : s) = (TConj, s)
  parseLhs s = (TBroadcast, s)

data Module = Broadcast | FlipFlop Bool | Conj (Map String Bool)
  deriving (Show)

type CState = Map String (Module, [String])

toState :: Input -> CState
toState input = M.fromList [(s, (initModuleState typ s, ts)) | (typ, s, ts) <- input]
 where
  reverseMapping = M.fromListWith (<>) [(t, [s]) | (_, s, ts) <- input, t <- ts]
  initModuleState TBroadcast _ = Broadcast
  initModuleState TFlipFlop _ = FlipFlop False
  initModuleState TConj s = Conj (M.fromList [(p, False) | p <- reverseMapping M.! s])

pretty st = intercalate "," [n <> "=" <> prettys s | (n, (s, _)) <- M.toList st]
prettys Broadcast = "."
prettys (FlipFlop b) = if b then "1" else "0"
prettys (Conj m) = "(" <> intercalate "," [name <> ":" <> (if b then "1" else "0") | (name, b) <- M.toList m] <> ")"

push :: Int -> Int -> CState -> (Int, Int, CState)
push l h = go l h (Seq.singleton ("button", "broadcaster", False))
 where
  -- go l h q st | traceShow (l,h,q, pretty st) False = undefined
  go l h Empty st = (l, h, st)
  go l h ((sender, name, pulse) :<| msgs) st =
    let cont = go (if pulse then l else l + 1) (if pulse then h + 1 else h)
     in case st M.!? name of
          Just (Broadcast, ts) -> cont (msgs Seq.>< Seq.fromList [(name, t, pulse) | t <- ts]) st
          Just (FlipFlop b, ts)
            | pulse -> cont msgs st
            | otherwise -> cont (msgs Seq.>< Seq.fromList [(name, t, not b) | t <- ts]) (st & ix name . _1 .~ FlipFlop (not b))
          Just (Conj m, ts) ->
            let m' = m & ix sender .~ pulse
             in cont (msgs Seq.>< Seq.fromList [(name, t, not (and m')) | t <- ts]) (st & ix name . _1 .~ Conj m')
          Nothing -> cont msgs st

count :: Int -> CState -> (Int, Int)
count n = go 0 0 0
 where
  go :: Int -> Int -> Int -> CState -> (Int, Int)
  go l h i st
    | i == n = (l, h)
    | otherwise = let (l', h', st') = push l h st in go l' h' (i + 1) st'

-- solve :: Input -> Output
solve input = let (l, h) = count 1000 (toState input) in l * h

{-

a b c   inv
0 0 0   c:0    [broad:0]
0 0 0   c:0    [a:0, b:0, c:0]
1 0 0   c:0    [b:0, c:0, b:1]
1 1 0   c:0    [c:0, b:1, c:1]
1 1 1   c:0    [b:1, c:1, inv:1]
1 1 1   c:0    [c:1, inv:1]
1 1 1   c:0    [inv:1]
1 1 1   c:1    [a:0]
0 1 1   c:1    [b:0]
0 0 1   c:1    [c:0]
0 0 0   c:1    [inv:0]
0 0 0   c:0    [a:1]
0 0 0   c:0    []
-}