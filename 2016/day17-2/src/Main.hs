-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
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
import Safe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

main :: IO ()
main = print (solve "yjjvjgan")

data Node = Node {passcode :: String, pos :: V2 Int}
  deriving (Eq, Ord, Show)

neighbors (Node passcode pos) =
  filter
    withinBounds
    ( [Node (passcode <> "U") (pos + V2 0 (-1)) | up]
        ++ [Node (passcode <> "D") (pos + V2 0 1) | down]
        ++ [Node (passcode <> "L") (pos + V2 (-1) 0) | left]
        ++ [Node (passcode <> "R") (pos + V2 1 0) | right]
    )
  where
    [up, down, left, right] = passcode & md5 & take 4 & map (`elem` "bcdef")
    withinBounds (Node _ (V2 i j)) = i >= 0 && i < 4 && j >= 0 && j < 4

paths :: Node -> [[Node]]
paths node = do
  n <- neighbors node
  if pos n == V2 3 3
    then return [n]
    else do
      path <- paths n
      return (n:path)

solve input = maximum (map length (paths (Node input (V2 0 0))))
