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

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (+~), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
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
import System.Posix.Types (CDev)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import GHC.Bits (xor)

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

type Input = (Int, Int, Int, Vector Int)

data ComboOperand = Lit Int | RegA | RegB | RegC | Op7
data Instr = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse (lines -> [ints -> [a], ints -> [b], ints -> [c], _, ints -> is]) = (a, b, c, V.fromList is)
parse _ = error "parse"

data Machine = M {areg :: Int, breg :: Int, creg :: Int, pc :: Int, out :: [Int]}
  deriving (Eq, Show, Generic)

toInstr 0 = Adv
toInstr 1 = Bxl
toInstr 2 = Bst
toInstr 3 = Jnz
toInstr 4 = Bxc
toInstr 5 = Out
toInstr 6 = Bdv
toInstr 7 = Cdv
toInstr _ = error "toInstr"

toCombo 0 = Lit 0
toCombo 1 = Lit 1
toCombo 2 = Lit 2
toCombo 3 = Lit 3
toCombo 4 = RegA
toCombo 5 = RegB
toCombo 6 = RegC
toCombo 7 = Op7
toCombo n = error ("toCombo " <> show n)

halted :: Vector Int -> Machine -> Bool
halted prog m = pc m >= length prog

step :: Vector Int -> Machine -> Machine
step prog m =
  case toInstr (prog V.! (m ^. #pc)) of
    Adv ->
      m
        & #areg .~ ((m ^. #areg) `div` (2 ^ combo))
        & next
    Bxl ->
      m
        & #breg .~ ((m ^. #breg) `xor` operand)
        & next
    Bst ->
      m
        & #breg .~ (combo `mod` 8)
        & next
    Jnz ->
      if m ^. #areg == 0
        then next m
        else m & #pc .~ operand
    Bxc ->
      m
        & #breg .~ ((m ^. #breg) `xor` (m ^. #creg))
        & next
    Out ->
      m
        & #out %~ ((combo `mod`8) :)
        & next
    Bdv ->
      m
        & #breg .~ ((m ^. #areg) `div` (2 ^ combo))
        & next
    Cdv ->
      m
        & #creg .~ ((m ^. #areg) `div` (2 ^ combo))
        & next
 where
  instr = toInstr (prog V.! (m ^. #pc))
  operand = prog V.! (m ^. #pc + 1)
  combo = resolve (toCombo operand)

  next m = m & #pc +~ 2

  resolve (Lit n) = n
  resolve RegA = m ^. #areg
  resolve RegB = m ^. #breg
  resolve RegC = m ^. #creg
  resolve _ = error "resolve"

run prog m = if halted prog m then reverse (out m) else run prog (step prog m)

solve (a,b,c,prog) = run prog (M a b c 0 [])
