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
import Data.List (sortOn, elemIndex, foldl', scanl')
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

data Op = Dec | Inc
 deriving (Show)
data Action = Action Op String Int
 deriving (Show)
data Comp = LT | LE | GT | GE | EQ | NEQ
 deriving (Show)
data Cond = Cond Comp String Int
 deriving (Show)
data Instr = Instr { action :: Action, cond :: Cond }
 deriving (Show)

type Input = [Instr]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
   parseLine str =
     let [reg1, act, n, "if", reg2, comp, m] = words str
     in Instr (Action (parseOp act) reg1 (read n)) (Cond (parseComp comp) reg2 (read m))
   parseOp "inc" = Inc
   parseOp "dec" = Dec
   parseComp "<" = Main.LT
   parseComp "<=" = LE
   parseComp ">" = Main.GT
   parseComp ">=" = GE
   parseComp "==" = Main.EQ
   parseComp "!=" = NEQ

step :: Map String Int -> Instr -> Map String Int
step regs (Instr action cond)
  | evalCond cond = evalAction action
  | otherwise = regs
 where
  evalCond (Cond comp reg i) = evalComp comp (valueOf reg) i
  evalAction (Action op reg i) = M.insert reg (evalOp op (valueOf reg) i) regs
  evalOp Inc = (+)
  evalOp Dec = (-)
  evalComp Main.LT = (<)
  evalComp LE = (<=)
  evalComp Main.GT = (>)
  evalComp GE = (>=)
  evalComp Main.EQ = (==)
  evalComp NEQ = (/=)
  valueOf reg = fromJustDef 0 (M.lookup reg regs)

solve :: Input -> Output
solve input = scanl' step mempty input & concatMap M.elems & maximum
