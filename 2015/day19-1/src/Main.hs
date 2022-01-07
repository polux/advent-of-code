-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.List (isPrefixOf, sortOn, elemIndex)
import Data.List.Split

-- #endregion

type Input = (Map String [String], String)

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str =
  let [prefix, suffix] = splitOn "\n\n" str
  in (M.unionsWith (++) (map parseLine (lines prefix)), suffix)
 where
   parseLine line =
     let [lhs, rhs] = splitOn " => " line
     in M.singleton lhs [rhs]

expand :: Map String [String] -> String -> Set String
expand rules = go
 where
   next :: String -> Maybe (String, [String], String)
   next s =
     case filter (`isPrefixOf` s) (M.keys rules) of
       [key] -> Just (key, rules M.! key, drop (length key) s)
       [] -> Nothing
   go "" = S.singleton []
   go s =
    case next s of
      Just (lhs, rhss, rest) -> S.fromList (map (++rest) rhss) `S.union` S.map (lhs++) (go rest)
      Nothing -> S.map (head s:) (go (tail s))

solve :: Input -> Output
solve (rules, molecule) = length (expand rules molecule) - 1
