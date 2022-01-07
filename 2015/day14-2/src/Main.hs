-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?))
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (transpose, scanl', sortOn, elemIndex)

-- #endregion

type Input = [(String, Int, Int, Int)]

type Output = Int

main :: IO ()
main = getContents >>= print . solve 2503 . parse

parse :: String -> Input
parse str = map parseLine (lines str)
 where
   parseLine line =
     let [[_, name, speed, runLength, restLength]] = line =~ "(\\S+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds."
     in (name, read speed, read runLength, read restLength)

run :: (String, Int, Int, Int) -> [Int]
run (name, speed, runLen, restLen) = tail $ scanl' (+) 0 deltas
 where
   deltas = fly runLen
   fly 0 = rest restLen
   fly n = speed : fly (n-1)
   rest 0 = fly runLen
   rest n = 0 : rest (n-1)

runs :: Input -> [[Int]]
runs reinders = transpose (map run reinders)

points :: [Int] -> [Int]
points distances = map score distances
 where
   maxDistance = maximum distances
   score d = if d == maxDistance then 1 else 0

scores :: [[Int]] -> [[Int]]
scores = tail . scanl' (zipWith (+)) (repeat 0)

solve :: Int -> Input -> Output
solve n input =
  runs input
  & map points
  & scores
  & (!! (n-1))
  & maximum
