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
{-# LANGUAGE TypeApplications #-}

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
import Data.List (elemIndex, foldl', sortOn)
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
import Data.Tuple (swap)
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

type Input = [String]

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = lines

numPad = ["789", "456", "123", " 0A"]
ctrlPad = [" ^A", "<v>"]

mkGrid ls = (M.fromList coords, M.fromList (map swap coords))
 where
  coords = [(V2 x y, c) | (y, l) <- zip [0 ..] ls, (x, c) <- zip [0 ..] l]

numGrid = mkGrid numPad
ctrlGrid = mkGrid ctrlPad

coordsOf (_, m) c = m M.! c
charAt (m, _) p = m M.! p

seqs grid g p
  | charAt grid p == ' ' = []
  | p == g = ["A"]
  | otherwise = do
      let (V2 di dj) = g - p
      (c, p') <-
        concat
          [ [('<', p + V2 (-1) 0) | di < 0]
          , [('>', p + V2 1 0) | di > 0]
          , [('^', p + V2 0 (-1)) | dj < 0]
          , [('v', p + V2 0 1) | dj > 0]
          ]
      cs <- seqs grid g p'
      pure (c : cs)

seqsBetween grid c1 c2 = seqs grid (coordsOf grid c2) (coordsOf grid c1)

solve input = input & map score & sum
 where
  score s = mtcount s * read (init s)

  mtcount = memo tcount

  tcount s =
    sum
      [ minimum [mcount es 25 | es <- seqsBetween numGrid last c]
      | (last, c) <- zip ('A' : s) s
      ]

  mcount = memo2 @String @Int count

  count s 0 = length s
  count s n =
    sum
      [ minimum [mcount es (n - 1) | es <- seqsBetween ctrlGrid last c]
      | (last, c) <- zip ('A' : s) s
      ]
