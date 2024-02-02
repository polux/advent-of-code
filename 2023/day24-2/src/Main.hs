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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use underscore" #-}
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
import Data.List (elemIndex, sortOn, tails)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.MemoTrie
import Data.Ratio
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
import Linear (V2 (..), det22, (*^), _x, _xy, _y)
import Linear.V2 (crossZ)
import Linear.V3
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import Data.SBV
import Linear.Vector ((^*))

-- #endregion

type Input = [(V3 Int, V3 Int)]

type Output = Int

main :: IO ()
main = readFile "input" >>= sat . eqns . parse >>= print

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine (splitOn "@" -> [parseV3 -> v1, parseV3 -> v2]) = (v1, v2)
  parseV3 (map read . splitOn "," -> [x, y, z]) = V3 x y z

eqVec :: EqSymbolic a => V3 a -> V3 a -> SBool
eqVec (V3 x1 y1 z1) (V3 x2 y2 z2) = (x1 .== x2) .&& (y1 .== y2) .&& (z1 .== z2)

toSym :: V3 Int -> V3 SReal
toSym = fmap (literal . AlgRational True . fromIntegral)

eqns :: [(V3 Int, V3 Int)] -> Symbolic SBool
eqns ((p1, v1) : (p2, v2) : (p3, v3) : _) = do
    [t1, t2, t3] <- traverse free ["t1", "t2", "t3"]
    p0 <- V3 <$> free "x0" <*> free "y0" <*> free "z0"
    v0 <- V3 <$> free "vx0" <*> free "vy0" <*> free "vz0"
    res <- free "res"
    solve
        [ eqVec (p0 + v0 ^* t1) (toSym p1 + toSym v1 ^* t1)
        , eqVec (p0 + v0 ^* t2) (toSym p2 + toSym v2 ^* t2)
        , eqVec (p0 + v0 ^* t3) (toSym p3 + toSym v3 ^* t3)
        , res .== sum p0
        ]