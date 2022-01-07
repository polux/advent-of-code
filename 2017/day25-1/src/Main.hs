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

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Safe
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

type Input = String

type Output = Input

{-
data St = A | B | C | D | E | F
  deriving (Show, Eq, Ord)
data Dir = L | R
  deriving (Show)

data Rules = Rules { when0 :: (Bool, Dir, St), when1 :: (Bool, Dir, St) }
type Instructions = Map St Rules

instructions :: Instructions
instructions = M.fromList
  [
    (A, Rules (True, R, B) (False, L, C)),
    (B, Rules ())
  ]
-}

type Tape = Set Int

isZero :: Int -> Tape -> Bool
isZero i t = not (S.member i t)

setBit :: Int -> Tape -> Tape
setBit = S.insert

unsetBit :: Int -> Tape -> Tape
unsetBit = S.delete

evalA :: Int -> Int -> Tape -> Tape
evalA 0 _ tape = tape
evalA n i tape | isZero i tape = evalB (n-1) (i+1) (setBit i tape)
               | otherwise = evalC (n-1) (i-1) (unsetBit i tape)
evalB :: Int -> Int -> Tape -> Tape
evalB 0 _ tape = tape
evalB n i tape | isZero i tape = evalA (n-1) (i-1) (setBit i tape)
               | otherwise = evalD (n-1) (i+1) (setBit i tape)
evalC :: Int -> Int -> Tape -> Tape
evalC 0 _ tape = tape
evalC n i tape | isZero i tape = evalA (n-1) (i+1) (setBit i tape)
               | otherwise = evalE (n-1) (i-1) (unsetBit i tape)
evalD :: Int -> Int -> Tape -> Tape
evalD 0 _ tape = tape
evalD n i tape | isZero i tape = evalA (n-1) (i+1) (setBit i tape)
               | otherwise = evalB (n-1) (i+1) (unsetBit i tape)
evalE :: Int -> Int -> Tape -> Tape
evalE 0 _ tape = tape
evalE n i tape | isZero i tape = evalF (n-1) (i-1) (setBit i tape)
               | otherwise = evalC (n-1) (i-1) (setBit i tape)
evalF :: Int -> Int -> Tape -> Tape
evalF 0 _ tape = tape
evalF n i tape | isZero i tape = evalD (n-1) (i+1) (setBit i tape)
               | otherwise = evalA (n-1) (i+1) (setBit i tape)

main :: IO ()
main = print (evalA 12919244 0 mempty & length)
