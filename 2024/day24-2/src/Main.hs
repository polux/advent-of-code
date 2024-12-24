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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move reverse out" #-}
-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, isPrefixOf, sortOn, isInfixOf, intercalate, sort, nub)
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

data Expr = Var String | Or String String | And String String | Xor String String
  deriving (Eq, Ord, Show)
type Input = Map String Expr

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse (splitOn "\n\n" -> [pre, post]) = M.union (parsePre pre) (parsePost post)
 where
  parsePre (lines -> seeds) = M.unions (map parseSeed seeds)
  parseSeed (splitOn ": " -> [gate, num]) = M.singleton gate (Var gate)
  parsePost (lines -> rules) = M.unions (map parseRule rules)
  parseRule (words -> [g1, op, g2, _, g3]) = M.singleton g3 (parseOp op g1 g2)
  parseOp "OR" = Or
  parseOp "XOR" = Xor
  parseOp "AND" = And


class E t where
  exor :: t -> t -> t
  eand :: t -> t-> t
  eor :: t -> t -> t

instance E Bool where
  exor = xor
  eand = (&&)
  eor = (||)

add :: E e => [e] -> [e] -> [e]
add (x0:xs) (y0:ys) = exor x0 y0 : go (eand x0 y0) xs ys
 where
  go c [] [] = []
  go c (x:xs) (y:ys) = let xyxor = exor x y in exor c xyxor : go (eor (eand c xyxor) (eand x y)) xs ys

-- >>> add [True, False] [True, False]
-- >>> add [True, True, False] [True, False, False]
-- >>> add [True, True, False] [False, True, False]
-- [False,True]
-- [False,False,True]
-- [True,False,True]

data FE = FVar String | FOr FE FE | FAnd FE FE | FXor FE FE
  deriving (Eq, Ord, Show)

instance E FE where
  exor = FXor
  eand = FAnd
  eor = FOr

padded i = (if i < 10 then "0" else "") ++ show i

fExpr n = add [FVar ("x" ++ padded i) | i <- [0..n-1]] [FVar ("y" ++ padded i) | i <- [0..n-1]]

swap x y m =
  let mx = m M.! x
      my = m M.! y
  in M.insert x my (M.insert y mx m)

-- >>> fExpr 2
-- [FXor (FVar "x0") (FVar "y0"),FXor (FAnd (FVar "x0") (FVar "y0")) (FXor (FVar "x1") (FVar "y1"))]

solve inpt = intercalate "," $ sort ["z07", "bjm", "z13", "hsw", "z18", "skf", "wkr", "nvr"]
--take 45 diffs
 where
  input = inpt & swap "z07" "bjm" & swap "z13" "hsw" & swap "z18" "skf" & swap "wkr" "nvr"

  rendergraph input = "digraph G {" ++ subgraphs input ++ concatMap render (M.toList input) ++ "}"
  subgraphs input = subgraph input "x" ++ subgraph input "y" ++ subgraph input "z"
  subgraph input s = "subgraph " ++ s ++ "{rank=\"same\";" ++ (input & M.keys & filter (s `isInfixOf`) & intercalate ";") ++ "}"
  render (g, Or x y) = label g (g ++ ":OR") ++ x ++ " -> " ++ g ++ ";" ++ y ++ " -> " ++ g ++ ";"
  render (g, And x y) = label g (g ++ ":AND") ++ x ++ " -> " ++ g ++ ";" ++ y ++ " -> " ++ g ++ ";"
  render (g, Xor x y) = label g (g ++ ":XOR") ++ x ++ " -> " ++ g ++ ";" ++ y ++ " -> " ++ g ++ ";"
  label g s = g ++ " [label=\"" ++ s ++ "\"];"

  diffs = zipWith diff ["z"++ padded i | i <- [0..45]] expected

  expected = fExpr 45

  --diff x e | traceShow (x, input M.! x, e) False = undefined
  diff x e =
    case (input M.! x, e) of
      (Var a, FVar b) -> [x | a /= b]
      (Xor a b, FXor c d) -> diff2 a b c d
      (Or a b, FOr c d) -> diff2 a b c d
      (And a b, FAnd c d) -> diff2 a b c d
      --_ -> traceShow (x, input M.! x, e) [x]
      _ -> [x]

  diff2 a b c d =
    let res1 = (diff a c <> diff b d)
        res2 = diff a d <> diff b c
    in if null res1 || null res2 then [] else nub (sort (res1++res2))

  meval = memo eval

  eval x = evalExpr (input M.! x)

  evalExpr (Var x) = meval x
  evalExpr (And x1 x2) = meval x1 && meval x2
  evalExpr (Or x1 x2) = meval x1 || meval x2
  evalExpr (Xor x1 x2) = meval x1 `xor` meval x2

  toNum acc [] = acc
  toNum acc (b : bs) = toNum (acc * 2 + if b then 1 else 0) bs
