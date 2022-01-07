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
import Control.Monad (forM_)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isSpace)
import Data.Foldable (toList, Foldable (foldl'))
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

type Instr = (Int, Int, Int, Int)

type Sample = (UV.Vector Int, Instr, UV.Vector Int)

type Input = ([Sample], [Instr])

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse s =
  let [part1, part2] = splitOn "\n\n\n\n" s
   in (map parseBlock (splitOn "\n\n" part1), map parseInstr (lines part2))
  where
    parseBlock (lines -> [before, instr, after]) = (parseState before, parseInstr instr, parseState after)
    parseState str = UV.fromList (read (dropWhile (not . isSpace) str))
    parseInstr str =
      let [a, b, c, d] = map read (words str)
       in (a, b, c, d)

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

explains :: Sample -> OpCode -> Bool
explains (before, (_, a, b, c), after) op = apply a b c before op == after

candidates :: Sample -> Set OpCode
candidates sample = S.fromList (filter (explains sample) allOpCodes)

deduce mapping = M.map removeSingles mapping
  where
    singles = M.elems mapping & filter ((== 1) . length) & S.unions
    removeSingles set = if length set == 1 then set else set S.\\ singles

runProgram :: Map Int OpCode -> [Instr] -> UV.Vector Int -> UV.Vector Int
runProgram dict prog regs = foldl' step regs prog
 where
   step mem (i, a, b, c) = apply a b c mem (dict M.! i)

solve (samples, program) = runProgram dict program (UV.fromList [0,0,0,0])
  where
    dict =
      samples
        & groupBy (view (_2 . _1))
        & M.map (foldr1 S.intersection . map candidates)
        & converge deduce
        & M.map (head . toList)
