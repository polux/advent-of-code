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

import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (sortOn, elemIndex, unfoldr, isInfixOf, isPrefixOf, tails)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)
import Data.Foldable (toList)

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


main :: IO ()
main = print (solve 637061)

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse (unfoldr step n)
 where
   step 0 = Nothing
   step m = let (q,r) = m `divMod` 10 in Just (r, q)

solve :: Int -> Int
solve input = go (Seq.fromList [3, 7]) 0 1
 where
   inputLen = length inputDigitsReversed
   inputDigitsReversed = reverse (digits input)
   go :: Seq Int -> Int -> Int -> Int
   go seq i j =
      let Just iv = Seq.lookup i seq
          Just jv = Seq.lookup j seq
          suffix = Seq.fromList (digits (iv+jv))
          suffixLen = length suffix
          newSeq = seq <> Seq.fromList (digits (iv+jv))
          candidates = fmap toList (Seq.take suffixLen (Seq.tails (Seq.reverse newSeq)))
      in case Seq.filter (inputDigitsReversed `isPrefixOf`) candidates of
        Empty ->
          let newLen = length newSeq
              newI = (i + 1 + iv) `mod` newLen
              newJ = (j + 1 + jv) `mod` newLen
          in go newSeq newI newJ
        (res :<| _) -> length res - inputLen
