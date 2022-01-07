-- Copyright 2021 Google LLC.
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
import Linear (V2 (..), V3 (..), _x, _y)
import Linear.V3 (V3)
import Safe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
-- #endregion

import Text.Regex.Pcre2 (regex)
import Util
import Data.Foldable (minimumBy, maximumBy)
import Data.Ord (comparing)

parse :: String -> [Particle]
parse = map parseLine . T.lines . T.pack
  where
    toInt = read . T.unpack
    parseLine [regex|p=<(?<x>.+),(?<y>.+),(?<z>.+)>, v=<(?<vx>.+),(?<vy>.+),(?<vz>.+)>, a=<(?<ax>.+),(?<ay>.+),(?<az>.+)>|] =
      Particle
        (V3 (toInt x) (toInt y) (toInt z))
        (V3 (toInt vx) (toInt vy) (toInt vz))
        (V3 (toInt ax) (toInt ay) (toInt az))

data Particle = Particle {pos :: V3 Int, vel :: V3 Int, acc :: V3 Int}
  deriving (Show)

type Input = [Particle]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

solve :: Input -> Output
solve input = input & zip [0..] & minimumBy (comparing (accMag.snd)) & fst
 where
   accMag (Particle _ _ (V3 ax ay az)) = abs ax + abs ay + abs az
