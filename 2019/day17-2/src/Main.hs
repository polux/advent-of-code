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
{-# LANGUAGE PatternSynonyms #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn, intercalate)
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
import IntCode
import Linear (V2 (..), _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import GHC.IO (unsafePerformIO)

-- #endregion

type Pos = V2 Int

type Input = UA.Array Pos Char

--type Output = Int

main :: IO ()
main = readFile "input.txt" >>= parseAndExecuteIO


input :: Input
input = arrayFromList2D $ lines $ unsafePerformIO (readFile "output.txt")

data Instr = L | R | F Int

instance Show Instr where
  show L = "L"
  show R = "R"
  show (F i) = show i

cons (F n) (F m : is) = F (n+m) : is
cons i is = i : is

turnRight (V2 x y) = V2 (-y) x
turnLeft (V2 x y) = V2 y (-x)

step pos dir
  | at (pos+dir) == '#' = cons (F 1) (step (pos+dir) dir)
  | at (pos + turnLeft dir) == '#' = cons L (step pos (turnLeft dir))
  | at (pos + turnRight dir) == '#' = cons R (step pos (turnRight dir))
  | otherwise = []
 where
  at p | UA.inRange (UA.bounds input) p = input UA.! p
       | otherwise = '.'

path = step (V2 24 0) (V2 0 (-1)) & map show & intercalate ","

{-
L,10,L,8,R,8,L,8,R,6,    A
L,10,L,8,R,8,L,8,R,6,    A
R,6,R,8,R,8,             B
R,6,R,6,L,8,L,10,        C
R,6,R,8,R,8,             B
R,6,R,6,L,8,L,10,        C
R,6,R,8,R,8,             B
R,6,R,6,L,8,L,10,        C
R,6,R,8,R,8,             B
L,10,L,8,R,8,L,8,R,6     A
-}