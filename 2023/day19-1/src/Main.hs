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
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- #endregion

module Main where

-- #region imports

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
import Control.Applicative ((<|>))

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

evalCond :: Cond -> Map Char Int -> Bool
evalCond AlwaysTrue _ = True
evalCond (Gt x n) parts = parts M.! x > n
evalCond (Lt x n) parts = parts M.! x < n

evalCInstr :: CInstr -> Map Char Int -> Maybe Instr
evalCInstr (CInstr cond instr) parts
  | evalCond cond parts = Just instr
  | otherwise = Nothing

evalCInstrs :: [CInstr] -> Map Char Int -> Maybe Instr
evalCInstrs (ci:cis) parts = evalCInstr ci parts <|> evalCInstrs cis parts

---solve :: Input -> Output
solve (rules, partss) = partss & filter (go "in") & map sum & sum
  where
    go r parts =
      case evalCInstrs (rules M.! r) parts of
        Just (Goto r') -> go r' parts
        Just Accept -> True
        _ -> False
