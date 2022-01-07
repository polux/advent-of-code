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
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (maximumBy, minimumBy, sortOn)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.MemoTrie
import Data.Numbers.Primes (isPrime)
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

type Input = [(Int, Int)]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str =
  let [_, line2] = lines str
   in catMaybes $ zipWith parseEntry (splitOn "," line2) [0 ..]
  where
    parseEntry "x" _ = Nothing
    parseEntry num n = Just (read num, n)

timeToNextBus :: Int -> Int -> Int
timeToNextBus earliest period = period - (earliest `mod` period)

solve :: Input -> Output
solve periods = sieve moduli
  where
    moduli =
      sortOn
        (negate . fst)
        [(period, (period - offset) `mod` period) | (period, offset) <- periods]
    sieve ((n1, a1) : rest) = sieve' n1 a1 rest
    sieve' _ x  [] = x
    sieve' ns x ((ni, ai) : rest) =
      let x' = head (filter ((== ai) . (`mod` ni)) [x, x+ns..])
      in sieve' (ns*ni) x' rest

{-

(n1, a1), (n2, a2), ...., (ni, ai)

je cherche t tel que

t mod n1 = n1-a1
t mod n2 = n2-a2
t mod n3 = n3-a3
...
t mod ni = ni-ai

je sais que n1, n2 ... sont tous premiers

a1, a1+n1, a1+2n1...

t mod n2 = a2
t2

t2, t2+n1n2, t2+2n1n2

-}