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
import Data.Maybe (fromJust, catMaybes)
import Data.List (foldl', sortOn, elemIndex)

-- #endregion

data Instruction = TurnOn | TurnOff | Toggle

type Input = [(Instruction, Coord, Coord)]

type Output = Int

type Coord = V2 Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = runParserOrDie (lineP `P.endBy` P.newline) str
 where
   lineP :: Parser (Instruction, Coord, Coord)
   lineP = do
     instr <- instrP
     x1 <- L.decimal
     P.char ','
     y1 <- L.decimal
     P.string " through "
     x2 <- L.decimal
     P.char ','
     y2 <- L.decimal
     return (instr, V2 x1 y1, V2 x2 y2)
   instrP :: Parser Instruction
   instrP =
     (do { P.string "turn on " ; pure TurnOn })
     P.<|> (do { P.string "turn off " ; pure TurnOff})
     P.<|> (do { P.string "toggle " ; pure Toggle})

type Grid = Set Coord

rectangle :: Coord -> Coord -> [Coord]
rectangle (V2 i1 j1) (V2 i2 j2) = [V2 i j | j <- [j1..j2], i <- [i1..i2]]

on :: Grid -> Coord -> Grid
on = flip S.insert

off :: Grid -> Coord -> Grid
off = flip S.delete

toggle :: Grid -> Coord -> Grid
toggle grid coord
  | S.member coord grid = off grid coord
  | otherwise = on grid coord

execute :: Grid -> (Instruction, Coord, Coord) -> Grid
execute grid (instr, c1, c2) = foldl' apply grid (rectangle c1 c2)
 where
   apply = fun instr
   fun TurnOn = on
   fun TurnOff = off
   fun Toggle = toggle

solve :: Input -> Output
solve input = foldl' execute mempty input & length
