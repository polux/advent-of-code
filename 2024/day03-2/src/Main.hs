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

-- #endregion

module Main where

-- #region imports

import Control.Applicative ((<|>))
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
import Data.Functor (($>), (<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, isPrefixOf, sortOn)
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
import qualified Text.Megaparsec.State as P
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Input = [(Int, Int)]

type Output = Int

main :: IO ()
main = readFile "input" >>= print . solve

solve :: String -> Int
solve = sum . map (uncurry (*)) . inDo

inDo :: String -> [(Int, Int)]
inDo "" = []
inDo str =
  case runParserMaybe parseElem str of
    Just (Do, s) -> inDo s
    Just (Dont, s) -> inDont s
    Just (Mul n1 n2, s) -> (n1, n2) : inDo s
    Nothing -> inDo (tail str)

inDont :: String -> [(Int, Int)]
inDont "" = []
inDont str =
  case runParserMaybe parseElem str of
    Just (Do, s) -> inDo s
    Just (Dont, s) -> inDont s
    Just (Mul n1 n2, s) -> inDont s
    Nothing -> inDont (tail str)

data Element = Do | Dont | Mul Int Int

parseElem :: Parser Element
parseElem = parseDo <|> parseDont <|> parseMul

parseDo :: Parser Element
parseDo = P.try (P.string "do()" $> Do)

parseDont :: Parser Element
parseDont = P.try (P.string "don't()" $> Dont)

parseMul :: Parser Element
parseMul = P.try $ do
  _ <- P.string "mul("
  n1 <- L.decimal
  _ <- P.char ','
  n2 <- L.decimal
  _ <- P.char ')'
  pure (Mul n1 n2)
