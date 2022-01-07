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

import Control.Lens ((*~), Ixed(ix), at, (+~), each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)

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

data Atom = Lit Int | Reg Char
  deriving (Show)
data Instr = Snd Char | Set Char Atom | Add Char Atom | Mul Char Atom | Mod Char Atom | Rcv Char | Jgz Char Atom
  deriving (Show)

type Input = Vector Instr

type Output = Int

data MState = MState { regs :: Map Char Int, pc :: Int, lastPlayed :: Maybe Int }
  deriving (Show, Generic)

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = V.fromList . map parseLine . lines
 where
   parseLine str =
     case words str of
       ["snd", [x]] -> Snd x
       ["set", [x], y] -> Set x (parseAtom y)
       ["add", [x], y] -> Add x (parseAtom y)
       ["mul", [x], y] -> Mul x (parseAtom y)
       ["mod", [x], y] -> Mod x (parseAtom y)
       ["rcv", [x]] -> Rcv x
       ["jgz", [x], y] -> Jgz x (parseAtom y)
       _ -> error ("unknown instr " <> str)
   parseAtom [c] | c `elem` ['a'..'z'] = Reg c
   parseAtom s = Lit (read s)

step :: Input -> MState -> Either Int MState
step instrs mstate =
  case instrs V.! pc mstate of
    Snd r -> Right (mstate & #lastPlayed ?~ valueOf r & #pc +~ 1)
    Set r a -> Right (mstate & #regs . at r ?~ resolve a & #pc +~ 1)
    Add r a -> Right (mstate & #regs . ix r +~ resolve a & #pc +~ 1)
    Mul r a -> Right (mstate & #regs . ix r *~ resolve a & #pc +~ 1)
    Mod r a -> Right (mstate & #regs . ix r %~ (`mod` resolve a) & #pc +~ 1)
    Rcv r -> if valueOf r == 0 then Right (mstate & #pc +~ 1) else Left (fromJust (lastPlayed mstate))
    Jgz r a -> Right (mstate & #pc +~ if valueOf r > 0 then resolve a else 1)
 where
  valueOf c = regs mstate M.! c
  resolve (Lit i) = i
  resolve (Reg c) = valueOf c


solve :: Input -> Output
solve input = go (MState (M.fromList (zip ['a'..'z'] (repeat 0))) 0 Nothing)
 where
   go mstate =
     case traceShow mstate $ step input mstate of
       Left i -> i
       Right mstate' -> go mstate'
