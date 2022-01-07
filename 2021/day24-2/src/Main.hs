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

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (Lens', at, each, folded, isn't, ix, traversed, (%~), (&), (+~), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, replicateM, unless, when)
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
import Data.List (elemIndex, sortOn, zip4)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import Data.MemoTrie
import Data.SBV (EqSymbolic ((.==)), Goal, Mergeable (symbolicMerge), OrdSymbolic (inRange, (.<=), (.>=)), SDivisible (sDiv, sMod), constrain, maximize, sInt, sInt64, sInteger, minimize, optimizeWith, z3, OptimizeStyle (Lexicographic))
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

data Name = W | X | Y | Z
  deriving (Eq, Show)
data Val = Var Name | Lit Int
  deriving (Eq, Show)
data Instr = Inp Name | Add Name Val | Mul Name Val | Div Name Val | Mod Name Val | Eql Name Val
  deriving (Eq, Show)

data MachineState = MachineState {pc :: Int, input :: [Int], w :: Int, x :: Int, y :: Int, z :: Int}
  deriving (Eq, Show, Generic)

atName :: Name -> Lens' MachineState Int
atName W = #w
atName X = #x
atName Y = #y
atName Z = #z

main :: IO ()
main = optimizeWith z3 Lexicographic problem >>= print

--readFile "input" >>= putStrLn . prettyProg . parse

parse :: String -> Input
parse str = map parseLine (lines str)
 where
  parseLine (words -> ["inp", parseName -> name]) = Inp name
  parseLine (words -> [parseBinCode -> opCode, parseName -> name, parseVal -> val]) = opCode name val
  parseLine _ = error "cannot parse line"
  parseBinCode "add" = Add
  parseBinCode "mul" = Mul
  parseBinCode "div" = Div
  parseBinCode "mod" = Mod
  parseBinCode "eql" = Eql
  parseBinCode _ = error "unknown bin code"
  parseName "w" = W
  parseName "x" = X
  parseName "y" = Y
  parseName "z" = Z
  parseName _ = error "unkonwn name"
  parseVal s = maybe (Var (parseName s)) Lit (readMay s)

step :: VInput -> MachineState -> Maybe MachineState
step prog m = apply <$> prog ^? ix (pc m)
 where
  apply (Inp x) =
    m & #input %~ tail
      & atName x .~ head (input m)
      & #pc +~ 1
  apply (Add x v) = applyBin (+) x v
  apply (Mul x v) = applyBin (*) x v
  apply (Div x v) = applyBin div x v
  apply (Mod x v) = applyBin safeMod x v
  apply (Eql x v) = applyBin (\a b -> if a == b then 1 else 0) x v
  applyBin f x v =
    m & atName x .~ f (m ^. atName x) (resolve v)
      & #pc +~ 1
  safeMod a b
    | a < 0 || b <= 0 = error "mod"
    | otherwise = a `mod` b
  resolve (Lit i) = i
  resolve (Var x) = m ^. atName x

eval :: Input -> [Int] -> MachineState
eval prog input = go (MachineState 0 input 0 0 0 0)
 where
  go m = maybe m go (step (V.fromList prog) m)

solve :: Input -> [Int]
solve prog = head [n | n <- replicateM 14 [9, 8 .. 1], process prog n == 0]

prettyProg :: Input -> String
prettyProg prog = unlines (zipWith prettyNumberedInstr [0 :: Int ..] prog)
 where
  prettyNumberedInstr i instr = printf "%03d    %s" i (prettyInstr instr)

prettyInstr :: Instr -> String
prettyInstr (Inp x) = printf "%s = read()" (prettyName x)
prettyInstr (Add x v) = printf "%s += %s " (prettyName x) (prettyVal v)
prettyInstr (Mul x v) = printf "%s *= %s " (prettyName x) (prettyVal v)
prettyInstr (Div x v) = printf "%s /= %s " (prettyName x) (prettyVal v)
prettyInstr (Mod x v) = printf "%s = %s %% %s" (prettyName x) (prettyName x) (prettyVal v)
prettyInstr (Eql x v) = printf "%s = (%s == %s) ? 1 : 0" (prettyName x) (prettyName x) (prettyVal v)

prettyName W = "w"
prettyName X = "x"
prettyName Y = "y"
prettyName Z = "z"

prettyVal (Var n) = prettyName n
prettyVal (Lit n) = show n

extract :: Input -> [(Int, Int, Int)]
extract prog = map extractChunk (chunksOf 18 prog)
 where
  extractChunk [_, _, _, _, Div Z (Lit a), Add X (Lit b), _, _, _, _, _, _, _, _, _, Add Y (Lit c), _, _] = (a, b, c)
  extractChunk _ = error "extract"

process :: Input -> [Int] -> Int
process prog input = go (extract prog) input 0
 where
  go _ [] z = z
  go ((a, b, c) : fs) (w : ws) z = go fs ws (if (z `mod` 26) + b == w then z `div` a else (z `div` a) * 26 + w + c)

prog = parse $ unsafePerformIO (readFile "/home/polux/projects/aoc/2021/day24-1/input")

-- >>> process prog [1..14] == z (eval prog [1..14])
-- True
-- >>> process prog [10,13,15,-12,14,-2,13,-12,15,11,-3,-13,-12,-13]
-- 0

prettyEqn :: (Int, Int, Int) -> String
prettyEqn (1, b, c) = printf "z = if (z `mod` 26) + %d == w then z else z * 26 + w + %d" b c
prettyEqn (26, b, c) = printf "z = if (z `mod` 26) + %d == w then z `div` 26 else (z `div` 26) * 26 + w + %d" b c
prettyEqn _ = error "prettyEqn"

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

find the wis that maximize w1*10^14 + w2*10^13 + ... + w14

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

problem :: Goal
problem = do
  zs@[z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14] <- mapM sInteger ["z" ++ show i | i <- [0 .. 14]]
  ws@[w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14] <- mapM sInteger ["w" ++ show i | i <- [1 .. 14]]
  forM_ ws $ \wi -> do
    constrain (inRange wi (1, 9))
  constrain $ z0 .== 0
  constrain $ z14 .== 0

  --constrain $ z13 .== symbolicMerge True (z12 `sMod` 26 - 12 .== w13) (z12 `sDiv` 26) ((z12 `sDiv` 26) * 26 + w13 + 4)
  --constrain $ z14 .== symbolicMerge True (z13 `sMod` 26 - 13 .== w14) (z13 `sDiv` 26) ((z13 `sDiv` 26) * 26 + w14 + 11)

  forM_ (zip4 (extract prog) zs ws (tail zs)) $ \((a, b, c), oldz, w, newz) ->
    constrain $
      newz
        .== symbolicMerge
          True
          (oldz `sMod` 26 + fromIntegral b .== w)
          (oldz `sDiv` fromIntegral a)
          ((oldz `sDiv` fromIntegral a) * 26 + w + fromIntegral c)

  minimize "goal" $
    w1 * 10000000000000
      + w2 * 1000000000000
      + w3 * 100000000000
      + w4 * 10000000000
      + w5 * 1000000000
      + w6 * 100000000
      + w7 * 10000000
      + w8 * 1000000
      + w9 * 100000
      + w10 * 10000
      + w11 * 1000
      + w12 * 100
      + w13 * 10
      + w14
