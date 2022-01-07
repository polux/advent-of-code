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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Bits
import Data.List (foldl')

-- #endregion

data Mask = Mask { ones :: Int, zeros :: Int }
  deriving Show
type Address = Int
data Instruction = SetMem Address Int | SetMask Mask
  deriving Show

type Input = [Instruction]

type Memory = Map Int Int

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = runParserOrDie (lineP `P.endBy` P.newline <* P.eof) str
 where
  symbol = L.symbol P.space
  lexeme = L.lexeme P.space
  lineP = setMaskP P.<|> setMemP
  setMaskP = do
    symbol "mask"
    symbol "="
    SetMask . parseMask <$> P.many P.alphaNumChar
  setMemP = do
    symbol "mem"
    address <- P.between (symbol "[") (symbol "]") (lexeme L.decimal)
    symbol "="
    value <- L.decimal
    return (SetMem address value)
  parseMask :: String -> Mask
  parseMask str =
     let oneIndices = map fst (filter ((=='1').snd) (zip [35,34..] str))
         zeroIndices = map fst (filter ((=='0').snd) (zip [35,34..] str))
     in Mask (foldr (flip setBit) zeroBits oneIndices)
      (foldr (flip clearBit) (complement zeroBits) zeroIndices)

solve :: Input -> Output
solve input =
  let (mem, _) = foldl' (\(mem, mask) instr -> step mem mask instr) (mempty, undefined) input
  in sum (M.elems mem)

step :: Memory -> Mask -> Instruction -> (Memory, Mask)
step mem _ (SetMask mask) = (mem, mask)
step mem mask@(Mask ones zeros) (SetMem addr val) =
  (M.insert addr ((val .|. ones) .&. zeros) mem, mask)