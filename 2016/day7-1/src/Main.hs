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

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Safe
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (tails, sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)
import Control.Arrow ((>>>))

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

type Input = [String]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = lines

-- using multiplication to simulate exceptions
-- 1 is neutral
-- 2 means found abba outside of brackets
-- 0 means found abba inside brackets
supportsTls :: Int -> String -> Int
supportsTls l ('[':cs) = supportsTls (l+1) cs
supportsTls l (']':cs) = supportsTls (l-1) cs
supportsTls 0 (a:b:c:d:cs) | a /= b && a == d && b == c && not (any (`elem` "[]") [a,b,c,d])= 2 * supportsTls 0 (b:c:d:cs)
supportsTls 0 (_:cs) = supportsTls 0 cs
supportsTls l (a:b:c:d:_) | a /= b && a == d && b == c && not (any (`elem` "[]") [a,b,c,d])= 0
supportsTls l (_:cs) = supportsTls l cs
supportsTls _ [] = 1

solve :: Input -> Output
solve input = input & filter ((>1) . supportsTls 0) & length
