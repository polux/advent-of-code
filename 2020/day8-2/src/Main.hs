-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Applicative (Alternative ((<|>)))
import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?))
import Control.Lens.Extras (is)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
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
import Control.Applicative (Alternative(empty))

-- #endregion

type Input = Program

type Output = Accumulator

data Instruction = Acc Int | Jmp Int | Nop Int
  deriving (Show)

type Program = Vector Instruction

type Accumulator = Int

type PC = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = V.fromList (runParserOrDie instructionsP str)
  where
    operations = [("nop", Nop), ("acc", Acc), ("jmp", Jmp)]
    instructionsP = instructionP `P.endBy` P.newline
    instructionP = operationP <*> signedInteger
    operationP = asum [symbol str $> ctr | (str, ctr) <- operations]
    signedInteger = L.signed P.space L.decimal
    symbol = L.symbol P.space

solve :: Input -> Output
solve input = fromJust $ asum (map interpret (alterations input))

alterations :: Program -> [Program]
alterations prog = map V.fromList (go (V.toList prog))
  where
    go [] = [[]]
    go (Acc n : tail) = (Acc n :) <$> go tail
    go (Nop n : tail) = ((Nop n :) <$> go tail) ++ [Jmp n : tail]
    go (Jmp n : tail) = ((Jmp n :) <$> go tail) ++ [Nop n : tail]

interpret :: Program -> Maybe Accumulator
interpret input = go 0 0 mempty
  where
    go :: Accumulator -> PC -> Set PC -> Maybe Accumulator
    go acc pc seen
      | pc == length input = Just acc
      | pc `S.member` seen = Nothing
      | otherwise = go' acc pc (S.insert pc seen)
    go' acc pc seen =
      case input V.! pc of
        Acc i -> go (acc + i) (pc + 1) seen
        Jmp offset -> go acc (pc + offset) seen
        Nop _ -> go acc (pc + 1) seen
