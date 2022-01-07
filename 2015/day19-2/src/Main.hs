-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
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
import Data.List (intercalate, findIndex, isPrefixOf, sortOn, elemIndex)
import Data.List.Split
import Data.Foldable (Foldable(toList))
import Data.Char (isAsciiLower, isAsciiUpper)
import Grammar
-- #endregion

type Rules = Map String [String]
type IntStr = [Int]
type IntRules = Map Int [IntStr]
type Input = (Rules, String)
type IntInput = (IntRules, IntStr)

type Output = Integer

main :: IO ()
main = getContents >>= print . solve . parse

atoms :: [String] -> Set String
atoms ss = S.unions (map go ss)
 where
   go [] = mempty
   go (c:cs) =
     let (suffix, rest) = span isAsciiLower cs
     in S.insert (c:suffix) (go rest)

rulesToIntRules :: Input -> IntInput
rulesToIntRules (rules, target) = (M.fromList (map translate (M.toList rules)), tokenize target)
  where
    names = "e" : toList (atoms (concat (M.elems rules)))
    ids = M.fromList (zip names [0..])
    tokenize "" = []
    tokenize s =
      let [name] = filter (`isPrefixOf` s) names
      in ids M.! name : tokenize (drop (length name) s)
    translate (lhs, rhss) = (single (tokenize lhs), map tokenize rhss)
    single [x] = x

parse :: String -> Input
parse str =
  let [prefix, suffix] = splitOn "\n\n" str
  in (M.unionsWith (++) (map parseLine (lines prefix)), head (lines suffix))
 where
   parseLine line =
     let [lhs, rhs] = splitOn " => " line
     in M.singleton lhs [rhs]

solve :: Input -> Output
solve input =
  let (_, target) = rulesToIntRules input
  in calc target


prhs is = unwords ["N" <> show i | i <- is] <> " { 1 + " <> intercalate " + " [ "$" <> show i | i <- [1..length is]] <> " }"
prhss i rhss = intercalate " | " (map prhs rhss) <> " | t" <> show i <> " { 0 }"
prule (i, rhss) = "T" <> show i <> " : " <> prhss i rhss
prules rules = unlines (map prule (M.toList rules))

--sets :: Input -> [Set String]
--sets (rules, target) = iterate (S.unions . S.map (contract rules)) (S.singleton target)


