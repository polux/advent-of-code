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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

-- #region imports
import Control.Lens (at, each, folded, isn't, ix, traversed, view, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, when)
import Control.Monad.Writer (MonadWriter (tell), execWriter, runWriter)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isSpace)
import Data.Foldable (Foldable (foldl'), toList)
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
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Printf
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

type Instr = (OpCode, Int, Int, Int)

type Input = (Int, V.Vector Instr)

main :: IO ()
main =
  getContents >>= print . solve . parse

--getContents >>= putStrLn . pretty . snd . parse

parse :: String -> Input
parse (lines -> (header : instrs)) = (parseHeader header, V.fromList (map parseInstr instrs))
  where
    parseHeader (words -> [_, i]) = read i
    parseInstr (words -> [op, a, b, c]) = (parseOpCode op, read a, read b, read c)
    parseOpCode "addr" = AddR
    parseOpCode "addi" = AddI
    parseOpCode "mulr" = MulR
    parseOpCode "muli" = MulI
    parseOpCode "banr" = BanR
    parseOpCode "bani" = BanI
    parseOpCode "borr" = BorR
    parseOpCode "bori" = BorI
    parseOpCode "setr" = SetR
    parseOpCode "seti" = SetI
    parseOpCode "gtir" = GtIR
    parseOpCode "gtri" = GtRI
    parseOpCode "gtrr" = GtRR
    parseOpCode "eqir" = EqIR
    parseOpCode "eqri" = EqRI
    parseOpCode "eqrr" = EqRR

data OpCode = AddR | AddI | MulR | MulI | BanR | BanI | BorR | BorI | SetR | SetI | GtIR | GtRI | GtRR | EqIR | EqRI | EqRR
  deriving (Eq, Ord, Show, Enum)

allOpCodes :: [OpCode]
allOpCodes = [AddR .. EqRR]

apply :: Int -> Int -> Int -> UV.Vector Int -> OpCode -> UV.Vector Int
apply a b c regs = go
  where
    go AddR = applyBinOpR (+)
    go AddI = applyBinOpI (+)
    go MulR = applyBinOpR (*)
    go MulI = applyBinOpI (*)
    go BanR = applyBinOpR (.&.)
    go BanI = applyBinOpI (.&.)
    go BorR = applyBinOpR (.|.)
    go BorI = applyBinOpI (.|.)
    go SetR = setAt c (valueAt a)
    go SetI = setAt c a
    go GtIR = applyCompIR (>)
    go GtRI = applyCompRI (>)
    go GtRR = applyCompRR (>)
    go EqIR = applyCompIR (==)
    go EqRI = applyCompRI (==)
    go EqRR = applyCompRR (==)
    valueAt i = regs UV.! i
    setAt i v = regs UV.// [(i, v)]
    applyBinOpR op = setAt c (op (valueAt a) (valueAt b))
    applyBinOpI op = setAt c (op (valueAt a) b)
    applyCompIR op = setAt c (if a `op` valueAt b then 1 else 0)
    applyCompRI op = setAt c (if valueAt a `op` b then 1 else 0)
    applyCompRR op = setAt c (if valueAt a `op` valueAt b then 1 else 0)

prettyInstr :: Instr -> String
prettyInstr (AddR, a, b, c) = printf "r%d = r%d + r%d" c a b
prettyInstr (AddI, a, b, c) = printf "r%d = r%d + %d" c a b
prettyInstr (MulR, a, b, c) = printf "r%d = r%d * r%d" c a b
prettyInstr (MulI, a, b, c) = printf "r%d = r%d * %d" c a b
prettyInstr (BanR, a, b, c) = printf "r%d = r%d & r%d" c a b
prettyInstr (BanI, a, b, c) = printf "r%d = r%d & %d" c a b
prettyInstr (BorR, a, b, c) = printf "r%d = r%d | r%d" c a b
prettyInstr (BorI, a, b, c) = printf "r%d = r%d | %d" c a b
prettyInstr (SetR, a, b, c) = printf "r%d = r%d" c a
prettyInstr (SetI, a, b, c) = printf "r%d = %d" c a
prettyInstr (GtIR, a, b, c) = printf "r%d = %d > r%d" c a b
prettyInstr (GtRI, a, b, c) = printf "r%d = r%d > %d" c a b
prettyInstr (GtRR, a, b, c) = printf "r%d = r%d > r%d" c a b
prettyInstr (EqIR, a, b, c) = printf "r%d = %d == r%d" c a b
prettyInstr (EqRI, a, b, c) = printf "r%d = r%d == %d" c a b
prettyInstr (EqRR, a, b, c) = printf "r%d = r%d == r%d" c a b

pretty :: V.Vector Instr -> String
pretty prog = V.toList prog & map prettyInstr & zipWith ann [0 ..] & unlines
  where
    ann i str = show i <> ": " <> str

{-

Reverse-engineered program below.

while (123 & 456 != 72) {}

6: r4 = r5 | 65536 # set bit 16 to 1 (counting from 0)
7: r5 = 15466939 # "111011000000000110111011"

8: r3 = r4 & 255 # truncate at 8 bits
9: r5 = r5 + r3
10: r5 = r5 & 16777215 # truncate at 24 bits
11: r5 = r5 * 65899
12: r5 = r5 & 16777215 # truncate at 24 bits

if (256 > r4) {
  if (r5 == r0) {  # comparison is on line 28
    return # win
  } else {
    jmp 6 # loop
  }
} else {
  17: r3 = 0
  18: r1 = r3 + 1
  19: r1 = r1 * 256 # r1 << 8
  if (r1 > r4) {
    r4 = r3
    jmp 8
  } else {
    r3++
    jmp 18
  }
}

we can speed it up by finding computing which r1 will be > r4, and the resulting r4

the smallest r3 such that (r3+1)*256 > r4 is r3 = (r4/256)+1-1 = r4/256

we patch the program on line 17 in collectValues to implement that optimization

-}

collectValues :: Input -> [Int]
collectValues (ip, prog) = execWriter (go (UV.fromList [1, 0, 0, 0, 0, 0]))
  where
    go mem =
      let n = mem UV.! ip
       in if n == 17
            then go (mem UV.// [(4, (mem UV.! 4) `div` 256), (ip, 8)])
            else do
              when (n == 28) (tell [mem UV.! 5])
              case prog V.!? n of
                Nothing -> error "reached end of execution"
                Just (op, a, b, c) ->
                  let newMem = apply a b c mem op
                   in go (newMem UV.// [(ip, newMem UV.! ip + 1)])

solve :: Input -> Int
solve input = go mempty 0 (collectValues input)
  where
    -- we search the latest value of r5 on line 28 before we loop
    go seen last (i : is)
      | S.member i seen = last
      | otherwise = go (S.insert i seen) i is