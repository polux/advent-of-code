-- Copyright 2021 Google LLC.
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

import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (sortOn, elemIndex, sort, maximumBy)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)

-- #endregion

import Text.Regex.Pcre2 (regex)
import Data.Ord (comparing)



type Input = [(Time, Event)]
type Time = (Int,Int,Int,Int,Int)
data Event = Start Int | WakesUp | FallsAsleep
  deriving (Show)


main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . T.lines . T.pack
 where
   toInt = read . T.unpack
   parseLine [regex|\[(?<date>.+)\] (?<event>.+)|] = (parseDate date, parseEvent event)
   parseEvent [regex|Guard #(?<id>\d+) begins shift|] = Start (toInt id)
   parseEvent [regex|wakes up|] = WakesUp
   parseEvent [regex|falls asleep|] = FallsAsleep
   parseDate [regex|(?<y>\d+)-(?<mo>\d+)-(?<d>\d+) (?<h>\d+):(?<mi>\d+)|] =
     (toInt y, toInt mo, toInt d, toInt h, toInt mi)

minute (_,_,_,_,m) = m

shifts :: Input -> Map Int [(Time,Time)]
shifts (sortOn fst->((t, Start id):es)) = go2 id [] es & M.fromListWith (++)
 where
  go2 id spans ((t, FallsAsleep):es) = go3 id spans t es
  go2 id spans ((t, Start newId):es) = (id,spans) : go2 newId [] es
  go2 id spans [] = [(id,spans)]
  go3 id spans t1 ((t2, WakesUp):es) = go2 id ((t1, t2):spans) es

timeAsleep :: [(Time,Time)] -> Int
timeAsleep spans = sum [minute t2 - minute t1 - 1 | (t1,t2) <- spans]

mostSleptMinute :: [(Time,Time)] -> Int
mostSleptMinute spans =
  spans
  & concatMap (\(t1,t2) -> [minute t1..minute t2-1])
  & map (,1)
  & M.fromListWith (+)
  & M.toList
  & maximumBy (comparing snd)
  & fst

solve input =
  let (id, spans) = shifts input & M.toList & maximumBy (comparing (timeAsleep . snd))
  in id * mostSleptMinute spans
