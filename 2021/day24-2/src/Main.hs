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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (Lens', at, each, folded, isn't, ix, traversed, (%~), (&), (+~), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4, (^?!))
import Control.Lens.Extras (is)
import Control.Monad (forM_, replicateM, unless, when, zipWithM, zipWithM_)
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
import Data.List (elemIndex, sortOn, zip4, foldl')
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import Data.MemoTrie
import Data.SBV (EqSymbolic ((.==)), Goal, Mergeable (symbolicMerge), OrdSymbolic (inRange, (.<=), (.>=)), SDivisible (sDiv, sMod), constrain, maximize, sInt, sInt64, sInteger, optimizeWith, z3, OptimizeStyle (Lexicographic), SWord32, ite, oneIf, sWord32, sWord64, OptimizeResult (LexicographicResult), SMTResult (Satisfiable), minimize)
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
import GHC.IO (unsafePerformIO)
import Linear (V2 (..), _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Printf (printf)
import Text.Regex.PCRE ((=~))
import Util
import Data.SBV.Internals (SMTModel(SMTModel, modelAssocs), modelAssocs, CV (CV), CVal (CInteger))

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

type Input = [Instr]
type VInput = V.Vector Instr

type Name = String
data Val = Var Name | Lit Int
  deriving (Eq, Show)
data Instr = Inp Name | Add Name Val | Mul Name Val | Div Name Val | Mod Name Val | Eql Name Val
  deriving (Eq, Show)

parse :: String -> Input
parse str = map parseLine (lines str)
 where
  parseLine (words -> ["inp", name]) = Inp name
  parseLine (words -> [parseBinCode -> opCode, name, parseVal -> val]) = opCode name val
  parseLine _ = error "cannot parse line"
  parseBinCode "add" = Add
  parseBinCode "mul" = Mul
  parseBinCode "div" = Div
  parseBinCode "mod" = Mod
  parseBinCode "eql" = Eql
  parseBinCode _ = error "unknown bin code"
  parseVal s = maybe (Var s) Lit (readMay s)

eval :: Input -> [SWord32] -> Map String SWord32 -> Map String SWord32
eval [] _ m = m
eval (Inp x : prog) (i:is) m = eval prog is (M.insert x i m)
eval (instr : prog) is m = eval prog is (apply instr)
 where
  apply (Add x v) = applyBin (+) x v
  apply (Mul x v) = applyBin (*) x v
  apply (Div x v) = applyBin sDiv x v
  apply (Mod x v) = applyBin sMod x v
  apply (Eql x v) = applyBin (\a b -> oneIf (a .== b)) x v
  applyBin f x v = M.insert x (f (m M.! x) (resolve v)) m
  resolve (Lit i) = fromIntegral i
  resolve (Var x) = m M.! x

-- names of the symbolic varialbes representing the input
wNames :: [String]
wNames = ["w"++show i | i <- [1..14]]

problem :: Input -> Goal
problem prog = do
  ws <- mapM sWord32 wNames
  forM_ ws $ \wi -> do
    constrain (inRange wi (1, 9))
  constrain (eval prog ws (M.fromList [("w", 0), ("x", 0), ("y", 0), ("z", 0)]) M.! "z" .== 0)
  mapM_ (minimize "") ws

main :: IO ()
main = do
  prog <- parse <$> readFile "input"
  LexicographicResult (Satisfiable _ SMTModel{modelAssocs}) <- optimizeWith z3 Lexicographic (problem prog)
  putStrLn $ concatMap (show . extract modelAssocs) wNames
 where
   extract values var | Just (CV _ (CInteger n)) <- lookup var values = n

-- REVERSE ENGINEERING

prettyProg :: Input -> String
prettyProg prog = unlines (zipWith prettyNumberedInstr [0 :: Int ..] prog)
 where
  prettyNumberedInstr i instr = printf "%03d    %s" i (prettyInstr instr)

prettyInstr :: Instr -> String
prettyInstr (Inp x) = printf "%s = read()" x
prettyInstr (Add x v) = printf "%s += %s " x (prettyVal v)
prettyInstr (Mul x v) = printf "%s *= %s " x (prettyVal v)
prettyInstr (Div x v) = printf "%s /= %s " x (prettyVal v)
prettyInstr (Mod x v) = printf "%s = %s %% %s" x x (prettyVal v)
prettyInstr (Eql x v) = printf "%s = (%s == %s) ? 1 : 0" x x (prettyVal v)

prettyVal (Var n) = n
prettyVal (Lit n) = show n

{-

/ is integer division
% is modulo

z0 = 0
z1  = if (z0  % 26) +  10 == w1  then z0       else z0         * 26 + w1  + 10
z2  = if (z1  % 26) +  13 == w2  then z1       else z1         * 26 + w2  + 5
z3  = if (z2  % 26) +  15 == w3  then z2       else z2         * 26 + w3  + 12
z4  = if (z3  % 26) + -12 == w4  then z3  / 26 else (z3  / 26) * 26 + w4  + 12
z5  = if (z4  % 26) +  14 == w5  then z4       else z4         * 26 + w5  + 6
z6  = if (z5  % 26) +  -2 == w6  then z5  / 26 else (z5  / 26) * 26 + w6  + 4
z7  = if (z6  % 26) +  13 == w7  then z6       else z6         * 26 + w7  + 15
z8  = if (z7  % 26) + -12 == w8  then z7  / 26 else (z7  / 26) * 26 + w8  + 3
z9  = if (z8  % 26) +  15 == w9  then z8       else z8         * 26 + w9  + 7
z10 = if (z9  % 26) +  11 == w10 then z9       else z9         * 26 + w10 + 11
z11 = if (z10 % 26) +  -3 == w11 then z10 / 26 else (z10 / 26) * 26 + w11 + 2
z12 = if (z11 % 26) + -13 == w12 then z11 / 26 else (z11 / 26) * 26 + w12 + 12
z13 = if (z12 % 26) + -12 == w13 then z12 / 26 else (z12 / 26) * 26 + w13 + 4
z14 = if (z13 % 26) + -13 == w14 then z13 / 26 else (z13 / 26) * 26 + w14 + 11
z14 = 0

1 <= wi <= 9

find the wis that maximize w1*10^13 + w2*10^12 + ... + w14

-}

{-
0 = if (z13 `mod` 26) - 13 == w14 then z13 `div` 26 else (z13 `div` 26) * 26 + w14 + 11
=> (z13 `mod` 26) - 13 == w14 && z13 < 26
=> z13 - 13 = w14
=> (z13, w14) in [(14,1),(15,2),(16,3),(17,4),(18,5),(19,6),(20,7),(21,8),(22,9)]

z1 = if (z0  % 26) +  10 == w1  then z0       else z0         * 26 + w1  + 10
=> z1 = if 10 == w1 then 0 else 26 + w1  + 10
=> since w1 <10, z1 = w1 + 10

z2  = if (z1  % 26) +  13 == w2  then z1       else z1         * 26 + w2  + 5
=> since w2 < 10,  z2 = z1 * 26 + w2 + 5

-}

