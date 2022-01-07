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
import Data.Foldable (Foldable (toList))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (delete)
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

-- #endregion

type Input = Seq Int

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = Seq.fromList (map read (lines str))

solve :: Input -> Output
solve input = go (Seq.take 25 input) (Seq.drop 25 input)
  where
    go _ Empty = error "no weakness found"
    go prefix@(_ :<| is) (j :<| js) = if valid (toList prefix) j then go (is :|> j) js else j

valid :: [Int] -> Int -> Bool
valid ns n = n `elem` [i + j | i <- ns, j <- delete i ns]