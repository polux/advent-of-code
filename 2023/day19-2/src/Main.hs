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
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- #endregion

module Main where

-- #region imports

import Control.Applicative (Alternative (empty), (<|>))
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
import Data.Char (isAlpha)
import Data.Functor (($>), (<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
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

data Instr = Goto String | Accept | Reject
  deriving (Show)

data CInstr = CInstr Cond Instr
  deriving (Show)

data Cond = AlwaysTrue | Gt Char Int | Lt Char Int
  deriving (Show)

type Input = (Map String [CInstr], [Map Char Int])

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = runParserOrDie (inputParser <* P.eof)
 where
  inputParser =
    (,)
      <$> rulesParser
      <*> ratingsParser
  rulesParser = M.fromList <$> ruleParser `P.endBy` P.space
  idParser = P.some (P.satisfy isAlpha)
  ruleParser = do
    name <- P.try idParser
    cinstrs <-
      P.between
        (P.char '{')
        (P.char '}')
        (cinstrParser `P.sepBy` P.char ',')
    return (name, cinstrs)
  cinstrParser =
    P.try (CInstr <$> condParser <* P.char ':' <*> instrParser)
      P.<|> (CInstr AlwaysTrue <$> instrParser)
  condParser = do
    part <- P.satisfy isAlpha
    op <- opParser
    n <- L.decimal
    return (op part n)
  opParser = (P.char '<' $> Lt) P.<|> (P.char '>' $> Gt)
  instrParser =
    (P.char 'A' $> Accept)
      P.<|> (P.char 'R' $> Reject)
      P.<|> (Goto <$> idParser)
  ratingsParser = ratingParser `P.endBy` P.space
  ratingParser =
    M.fromList
      <$> P.between
        (P.char '{')
        (P.char '}')
        (valueParser `P.sepBy` P.char ',')
  valueParser = do
    part <- P.satisfy isAlpha
    _ <- P.char '='
    n <- L.decimal
    return (part, n)

type AParts = Map Char (Int, Int)

withGt x n aparts =
  let (_, b) = aparts M.! x
   in M.insert x (n + 1, b) aparts

withLt x n aparts =
  let (a, _) = aparts M.! x
   in M.insert x (a, n - 1) aparts

withGe x n aparts =
  let (_, b) = aparts M.! x
   in M.insert x (n, b) aparts

withLe x n aparts =
  let (a, _) = aparts M.! x
   in M.insert x (a, n) aparts

withCond :: Cond -> AParts -> AParts
withCond AlwaysTrue = id
withCond (Gt x n) = withGt x n
withCond (Lt x n) = withLt x n

withNotCond :: Cond -> AParts -> AParts
withNotCond AlwaysTrue = id
withNotCond (Gt x n) = withLe x n
withNotCond (Lt x n) = withGe x n

cardinal aparts = product [b - a + 1 | (a, b) <- M.elems aparts]

---solve :: Input -> Output
solve (rules, partss) =
  aEvalAt "in" (M.fromList [(c, (1, 4000)) | c <- "xmas"])
 where
  aEvalAt r = aEvalCInstrs (rules M.! r)

  aEvalCInstrs (CInstr cond i : cis) parts =
    aEvalInstr i (withCond cond parts) + aEvalCInstrs cis (withNotCond cond parts)
  aEvalCInstrs [] parts = 0

  aEvalCInstr (CInstr cond i) parts = aEvalInstr i (withCond cond parts)

  -- aEvalInstr :: Instr -> AParts -> Int
  aEvalInstr Accept aparts = cardinal aparts
  aEvalInstr Reject aparts = 0
  aEvalInstr (Goto r) aparts = aEvalAt r aparts
