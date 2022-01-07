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

-- #region imports
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (-~), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (elemIndex, sortOn, (\\))
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe)
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

type Input = Map Char [Char]

type Output = String

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = M.fromListWith (++) . map parseLine . lines
  where
    parseLine (words -> [_, [s], _, _, _, _, _, [t], _, _]) = (s, [t])

solve :: Input -> Output
solve = kahn

inDegrees :: Input -> Map Char Int
inDegrees g =
  M.unionWith
    (+)
    (M.fromListWith (+) [(t, 1) | (_, ts) <- M.toList g, t <- ts])
    (M.fromList [(s, 0) | s <- M.keys g])


kahn :: Input -> String
kahn g = sort initialInDegrees initialQueue
  where
    initialInDegrees = inDegrees g
    initialQueue = S.fromList (M.keys (M.filter (== 0) initialInDegrees))
    decrement v = ix v -~ 1
    sort inDegrees queue =
      if S.null queue
        then []
        else
          let (v, vs) = S.deleteFindMin queue
              children = fromMaybe [] (g M.!? v)
              newInDegrees = foldr decrement inDegrees children
              newQueue = vs `S.union` S.fromList [c | c <- children, inDegrees M.! c == 1]
           in v : sort newInDegrees newQueue