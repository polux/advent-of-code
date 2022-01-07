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
import Data.List (elemIndex, foldl', scanl', sortOn)
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
import GHC.IO (unsafePerformIO)
import Linear (V2 (..), _x, _y)
import Safe hiding (at)
import Test.QuickCheck (NonNegative (NonNegative), quickCheck)
import Test.QuickCheck.Modifiers (Positive (Positive))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Printf (printf)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

data Instr = NewStack | Cut Integer | Increment Integer
  deriving (Show)

type Input = [Instr]


parse :: String -> Input
parse str = map (parseLine . words) (lines str)
  where
    parseLine ["deal", "into", "new", "stack"] = NewStack
    parseLine ["deal", "with", "increment", n] = Increment (read n)
    parseLine ["cut", n] = Cut (read n)
    parsLine other = error (unwords other)

input = parse (unsafePerformIO (readFile "input.txt"))

newDeckSize :: Integer
newDeckSize = 119315717514047

oldDeckSize :: Integer
oldDeckSize = 10007

numIterations :: Integer
numIterations = 101741582076661

data Expr = Expr {multiplier :: Integer, offset :: Integer}

instance Show Expr where
  show (Expr m o) = printf "\\i -> i*%d + %d" m o

symStep :: Expr -> Instr -> Expr
symStep (Expr m o) NewStack = Expr (- m) (- o -1)
symStep (Expr m o) (Cut j) = Expr m (o - j)
symStep (Expr m o) (Increment n) = Expr (m * n) (o * n)

symApply :: Integer -> [Instr] -> Expr
symApply deckSize is = let (Expr m o) = foldl' symStep (Expr 1 0) is in Expr (m `mod` deckSize) (o `mod` deckSize)

simplify deckSize (Expr m o) = Expr (m `mod` deckSize) (o `mod` deckSize)

{-

a*i+b = r (mod D)
a*i = (r-b) (mod D)

-}

symInv :: Integer -> Expr -> (Integer -> Integer)
symInv deckSize (Expr a b) r = ((r-b) * minv a deckSize) `mod` deckSize-- - o -- head [a | k <- [0 ..], let (a, b) = divMod (k * deckSize + r - o) m, b == 0]

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD 0 0 = undefined
extGCD a 0 = (1, 0, a)
extGCD a b =
  let (q, r) = a `quotRem` b
      (c, x, y) = extGCD b r
   in (x, c - q * x, y)

minv :: Integer -> Integer -> Integer
minv a n = x `mod` n -- a * x + n * y
  where
    (x, y, _) = extGCD a n


-- >>> sum (map (2^) exponentiation) == numIterations
-- True
exponentiation = go 0 numIterations
 where
   go _ 0 = []
   go p n = let (a,b) = divMod n 2 in if b == 1 then p : go (p+1) a else go (p+1) a

combine :: Expr -> Expr -> Expr
combine (Expr a1 b1) (Expr a2 b2) = simplify newDeckSize $ Expr (a1*a2) (a2*b1+b2)

combine2NTimes e 0 = e
combine2NTimes e n = let r = combine2NTimes e (n-1) in combine r r

finalForwardExpression e = foldl' combine (Expr 1 0) (map (combine2NTimes e) exponentiation)

result = symInv newDeckSize (finalForwardExpression (symApply newDeckSize input)) 2020

main = print result