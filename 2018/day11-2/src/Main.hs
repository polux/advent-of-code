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
-- #endregion

module Main where

-- #region imports

-- #region imports
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
import Data.List (sortOn, elemIndex, maximumBy)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)
import Data.Ord (comparing)

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

w = 300
gridSerialNumber = 2866

main :: IO ()
main =  print solve

powerLevel :: V2 Int -> Int
powerLevel (V2 x y) = ((((((x+10)*y)+gridSerialNumber)*(x+10)) `div` 100) `mod` 10)-5

levels :: UA.Array (V2 Int) Int
levels = UA.array (V2 1 1, V2 w w) [(V2 i j, powerLevel (V2 i j)) | i <- [1..w], j<-[1..w]]

totalPowers ::  V2 Int -> [(Int, Int)]
totalPowers p@(V2 x y) = drop 3 (zip [0..] (scanl (+) 0 layers))
 where
   layer s = sum [levels UA.! (p+V2 (s-1) j) | j<-[0..s]] +  sum [levels UA.! (p+V2 i (s-1)) | i<-[0..s]]
   layers = map layer [1..w-max x y]

solve =
  [V2 i j | i <-[1..w-2], j<-[1..w-2]]
  & concatMap (\p -> map (p,) (totalPowers p))
  & maximumBy (comparing (view (_2._2)))