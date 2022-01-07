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
import Data.List (scanl', foldl', sortOn, elemIndex)

-- #endregion

type Input = [[Char]]

type Output = String

type Pos = V2 Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = lines

move pos dir =
  let newPos = delta dir + pos
  in if isKey newPos then newPos else pos
 where
   delta 'U' = V2 0 (-1)
   delta 'D' = V2 0 1
   delta 'L' = V2 (-1) 0
   delta 'R' = V2 1 0
   delta _ = error "unknown instruction"

execLine :: Pos -> [Char] -> Pos
execLine = foldl' move

execLines :: Input -> [Pos]
execLines = scanl' execLine (V2 0 2)

keys =
  [
   "  1  ",
   " 234 ",
   "56789",
   " ABC ",
   "  D  "
  ]

toKey (V2 i j) = (keys !! j) !! i

isKey (V2 i j) = i >= 0 && i < 5 && j >= 0 && j < 5 && toKey (V2 i j) /= ' '

solve :: Input -> Output
solve input =
  execLines input
  & tail
  & map toKey
