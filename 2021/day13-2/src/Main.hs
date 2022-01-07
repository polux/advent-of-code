-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- #endregion

module Main where

-- #region imports

import Control.Arrow ((>>>),(***), Arrow (first, second))
import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4, view)
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
import qualified Data.Vector.Unboxed.Mutable as MUV
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..), _x, _y)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe hiding (at)
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_, unless, when)

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

data FoldInstr = AlongX Int | AlongY Int
  deriving (Show)

type Input = (Set (V2 Int), [FoldInstr])

type Output = String

main :: IO ()
main = readFile "input" >>= putStrLn . solve . parse

parse :: String -> Input
parse str =
  let [dots, instrs] = splitOn "\n\n" str
  in (S.fromList (map parseDot (lines dots)), map parseInstr (lines instrs))
 where
   parseDot (splitOn "," -> [x,y]) = V2 (read x) (read y)
   parseInstr (splitOn "="-> ["fold along x", n]) = AlongX (read n)
   parseInstr (splitOn "="-> ["fold along y", n]) = AlongY (read n)

executeFold :: FoldInstr -> Set (V2 Int) -> Set (V2 Int)
executeFold (AlongX x) dots = S.map (mirrorX x) dots
  where
    mirrorX a (V2 x y)
      | x > a = V2 (2*a-x) y
      | otherwise = V2 x y
executeFold (AlongY y) dots = S.map (mirrorY y) dots
  where
    mirrorY a (V2 x y)
      | y > a = V2 x (2*a-y)
      | otherwise = V2 x y

display :: Set (V2 Int) -> String
display dots = unlines [[if V2 x y `S.member` dots then '#' else ' ' | x <- [0..w]] | y <- [0..h]]
 where
   w = maximum (S.map (view _x) dots)
   h = maximum (S.map (view _y) dots)

solve :: Input -> Output
solve (dots, is) = foldl (flip executeFold) dots is & display
