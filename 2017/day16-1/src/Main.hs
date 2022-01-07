-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
-- #endregion

module Main where

-- #region imports

import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import qualified Data.Vector.Unboxed.Mutable as MUV
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..), _x, _y)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe hiding (at)
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortOn, elemIndex, foldl')
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)
import Control.Arrow ((>>>))

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

data Instr = Shift Int | SwapChars Char Char | SwapPos Int Int
  deriving (Show)

type Input = [Instr]

main :: IO ()
main = getContents >>= (parse >>> solve >>> print)

parse :: String -> Input
parse = map parseInstr . splitOn "," . head . lines
 where
   parseInstr ('s':n) = Shift (read n)
   parseInstr ('x' :  (splitOn "/" -> [n, m])) = SwapPos (read n) (read m)
   parseInstr ['p',a,'/',b] = SwapChars a b
   parseInstr s = error ("unknown instr " <> s)

solve :: [Instr] -> String
solve input =
  let
    (offset, UV.toList->str) = foldl' step (0, UV.fromList chars) input
    n = (len-(offset `mod` len))
  in drop n str ++ take n str
 where
   chars = ['a'..'p']
   len = length chars
   step :: (Int, UV.Vector Char) -> Instr -> (Int, UV.Vector Char)
   step (offset, vec) (Shift n) = (offset+n, vec)
   step (offset, vec) (SwapPos i j) = (offset, vec UV.// [(i', vec UV.! j'),(j', vec UV.! i')])
     where
       i' = (i - offset) `mod` len
       j' = (j - offset) `mod` len
   step (offset, vec) (SwapChars a b) = (offset, vec UV.// [(i, b),(j, a)])
    where
      i = fromJust (UV.elemIndex a vec)
      j = fromJust (UV.elemIndex b vec)
