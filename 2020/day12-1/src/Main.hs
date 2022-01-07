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
import Data.Complex
import Linear.Vector ((*^))

-- #endregion

type Input = [Instruction]

type Output = Int

type Instruction = (Char, Int)
type ShipState = (V2 Int, V2 Int)

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = map parseEntry (lines str)
 where
  parseEntry [] = error "wrong entry"
  parseEntry (c:cs) = (c, read cs)

move :: Instruction -> ShipState -> ShipState
move ('N', n) (pos, dir) = (pos + V2 0 n, dir)
move ('S', n) (pos, dir) = (pos + V2 0 (-n), dir)
move ('E', n) (pos, dir) = (pos + V2 n 0, dir)
move ('W', n) (pos, dir) = (pos + V2 (-n) 0, dir)
move ('F', n) (pos, dir) = (pos + n *^ dir, dir)
move ('L', n) (pos, dir) = (pos, rotate n dir)
move ('R', n) (pos, dir) = (pos, rotate (-n) dir)
move _ _ = error "unknonw instruction"

rotate 0 dir = dir
rotate 90 (V2 1 0) = V2 0 1
rotate 90 (V2 0 1) = V2 (-1) 0
rotate 90 (V2 (-1) 0) = V2 0 (-1)
rotate 90 (V2 0 (-1)) = V2 1 0
rotate 180 dir = rotate 90 (rotate 90 dir)
rotate 270 dir = rotate 180 (rotate 90 dir)
rotate n dir = rotate (n `mod` 360) dir

solve :: Input -> Output
solve input =
  let (V2 x y, _) = foldl (flip move) (V2 0 0, V2 1 0) input
  in abs x + abs y
