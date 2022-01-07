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
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (foldl', foldl1')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Safe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

data Mask = Mask {ones :: Int, zeros :: Int}
  deriving (Show)

type Address = Int

data Instruction = SetMem Address Int | SetMask [Mask]
  deriving (Show)

type Input = [Instruction]

type Memory = Map Int Int

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = runParserOrDie (P.many lineP <* P.eof) str
  where
    symbol = L.symbol P.space
    lexeme = L.lexeme P.space
    decimal = lexeme L.decimal
    lineP = setMaskP P.<|> setMemP
    setMaskP = do
      symbol "mask"
      symbol "="
      mask <- lexeme (P.many P.alphaNumChar)
      return (SetMask (parseMask mask))
    setMemP = do
      symbol "mem"
      address <- P.between (symbol "[") (symbol "]") decimal
      symbol "="
      SetMem address <$> decimal
    parseMask :: String -> [Mask]
    parseMask str = map toBinary (generate str)
    generate :: String -> [([Int], [Int])]
    generate ('1' : bits) = do
      (ones, zeros) <- generate bits
      return (1 : ones, 1 : zeros)
    generate ('0' : bits) = do
      (ones, zeros) <- generate bits
      return (0 : ones, 1 : zeros)
    generate ('X' : bits) = do
      (ones, zeros) <- generate bits
      [(1 : ones, 1 : zeros), (0 : ones, 0 : zeros)]
    generate [] = return ([], [])
    generate _ = error "unexpected character"
    toBinary :: ([Int], [Int]) -> Mask
    toBinary (ones, zeros) = Mask (parseBinary ones) (parseBinary zeros)
    parseBinary :: [Int] -> Int
    parseBinary = foldl' (\acc b -> acc * 2 + b) 0

{-

001X0X

Mask 001000 111010
Mask 001001 111011
Mask 001100 111110
Mask 001101 111111

-}

solve :: Input -> Output
solve input =
  let (mem, _) = foldl' (uncurry step) (mempty, undefined) input
   in sum (M.elems mem)

step :: Memory -> [Mask] -> Instruction -> (Memory, [Mask])
step mem _ (SetMask masks) = (mem, masks)
step mem masks (SetMem addr val) = (foldl' update mem masks, masks)
  where
    update :: Memory -> Mask -> Memory
    update mem (Mask ones zeros) =
      M.insert ((addr .|. ones) .&. zeros) val mem