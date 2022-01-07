-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (elemIndex, sortOn, nub)
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
import Linear (V2 (..), V3, _x, _y)
import Linear.V3 (V3 (V3))
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (StringOutputStyle (EscapeNonPrintable), pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Input = Set (V3 Int)

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str =
  str
    & lines
    & arrayFromList2D @_ @UA.Array @_ @(Int, Int)
    & UA.assocs
    & filter ((== '#') . snd)
    & map (\((x, y), _) -> V3 0 x y)
    & S.fromList

size = V2 5 5

neighbors :: V3 Int -> [V3 Int]
neighbors (V3 l i j) =
  [V3 l i' j' | V2 i' j' <- neighbors2D size (V2 i j), (i', j') /= (2, 2)]
    ++ topOut
    ++ bottomOut
    ++ leftOut
    ++ rightOut
    ++ topIn
    ++ bottomIn
    ++ leftIn
    ++ rightIn
  where
    topOut = [V3 (l -1) 2 1 | j == 0]
    bottomOut = [V3 (l -1) 2 3 | j == 4]
    leftOut = [V3 (l -1) 1 2 | i == 0]
    rightOut = [V3 (l -1) 3 2 | i == 4]
    topIn = [V3 (l + 1) i' 0 | (i, j) == (2, 1), i' <- [0 .. 4]]
    bottomIn = [V3 (l + 1) i' 4 | (i, j) == (2, 3), i' <- [0 .. 4]]
    leftIn = [V3 (l + 1) 4 j' | (i, j) == (3, 2), j' <- [0 .. 4]]
    rightIn = [V3 (l + 1) 0 j' | (i, j) == (1, 2), j' <- [0 .. 4]]

numNeighborsOn :: Input -> V3 Int -> Int
numNeighborsOn on v = neighbors v & filter (`S.member` on) & length

candidates :: Input -> Set (V3 Int)
candidates on = on `S.union` (on & S.toList & concatMap neighbors & S.fromList)

step :: Input -> Input
step on = candidates on & S.filter (shouldLive on)

shouldLive :: Input -> V3 Int -> Bool
shouldLive on v = if v `S.member` on then numNeighbors == 1 else numNeighbors `elem` [1, 2]
  where
    numNeighbors = numNeighborsOn on v

display :: Input -> String
display input = unlines (map displayLevel levels)
 where
   levels = nub (input ^.. folded . _1)
   displayLevel n = unlines (("Depth " <> show n <> ":") : [displayRow n j | j <- [0..4]])
   displayRow n j = [if V3 n i j `S.member` input then '#' else '.' | i <- [0..4]]

solve input = iterate step input !! 200 & length
