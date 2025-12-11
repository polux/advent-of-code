-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when, (>=>))
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.Bits (Bits (shiftL, xor), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sort, sortOn)
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

import Numeric.Optimization.MIP ((.==.))
import qualified Numeric.Optimization.MIP as MIP
import Numeric.Optimization.MIP.Solver

-- #endregion

type Entry = (Vector (Set Int), Vector Int)
type Input = [Entry]

type Output = Input

main :: IO ()
main = readFile "input" >>= solvePb . parse >>= print

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine str =
    let ws = words str
        buttonsStr = init (tail ws)
        joltageStr = last ws
     in ( V.fromList $ map (S.fromList . ints) buttonsStr
        , V.fromList $ ints joltageStr
        )

solvePb :: Input -> IO Int
solvePb input = mapM mip input <&> sum

mip :: Entry -> IO Int
mip (buttons, target) = do
  let n = length target
      m = length buttons
      x i = MIP.Var ("x" <> T.pack (show i))
      xe i = MIP.varExpr (x i)
      prob =
        MIP.def
          { MIP.objectiveFunction =
              MIP.def
                { MIP.objDir = MIP.OptMin
                , MIP.objExpr = sum [xe i | i <- [0 .. m - 1]]
                }
          , MIP.constraints =
              [ sum
                [ xe i
                | i <- [0 .. m - 1]
                , j `elem` (buttons V.! i)
                ]
                .==. fromIntegral (target V.! j)
              | j <- [0 .. n - 1]
              ]
          , MIP.varDomains =
              M.fromList
                [ (x i, (MIP.IntegerVariable, (0, MIP.PosInf)))
                | i <- [0 .. n - 1]
                ]
          }
  sol <- solve cbc MIP.def prob
  pure (ceiling (fromJust (MIP.solObjectiveValue sol)))