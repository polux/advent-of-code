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
import Data.List (sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)
import IntCode
import Data.Ratio
import GHC.IO (unsafePerformIO)

-- #endregion

type Input = Machine

--type Output = Input

main' :: IO ()
main' = getContents >>= putStrLn . solve . parse

parse :: String -> Input
parse = parseToMachine

--solve :: Input -> Output
solve input = unlines [
  [evalCell i j | i <- [0..300] ] | j <- [0..300]]
 where
   evalCell i j = if evalList [i,j] input == [1] then '#' else '.'

slope1 :: Ratio Int
slope1 = 300 % 265
slope2 :: Ratio Int
slope2 = 300 % 240

betweenSlopes 0 0 = True
betweenSlopes 0 y = False
betweenSlopes x y = (y % x >= slope1) && (y % x <= slope2)

render = unlines [
  [if betweenSlopes i j then '#' else '.' | i <- [0..300] ] | j <- [0..300]]

height x = floor (x%1 * slope2 - x%1 * slope1)

bisect _ l u | traceShow (l,u) False = undefined
bisect p l u
  | u == (l+1) = l
  | otherwise =
      let m = l + (u-l) `div` 2
      in if p m then bisect p m u else bisect p l m

{-# NOINLINE input #-}
input = unsafePerformIO (parse <$> readFile "input.txt")

largeEnoughX = bisect ((<10_000) . height) 0 1_000_000_000_000

y1For x = floor (x%1 * slope1)
y2For x = floor (x%1 * slope2)

cellAt x y = evalList [x,y] input == [1]

trueY1ForLargeEnoughX = bisect (not . cellAt largeEnoughX) (candidate - 3000) candidate + 1
 where
   candidate = y1For largeEnoughX

trueY2ForLargeEnoughX = bisect (cellAt largeEnoughX) candidate (candidate + 3000)
 where
   candidate = y2For largeEnoughX

trueSlope1 = trueY1ForLargeEnoughX % largeEnoughX
trueSlope2 = trueY2ForLargeEnoughX % largeEnoughX


betweenTrueSlopes 0 0 = True
betweenTrueSlopes 0 y = False
betweenTrueSlopes x y = (y % x >= trueSlope1) && (y % x <= trueSlope2)

trueRender = unlines [
  [if betweenTrueSlopes i j then '#' else '.' | i <- [0..300] ] | j <- [0..300]]

trueY1For x = floor (x%1 * trueSlope1)
trueY2For x = floor (x%1 * trueSlope2)

isSolutionX x = let y2 = trueY2For x in betweenTrueSlopes (x+99) (y2-99)

solutionX = bisect (not.isSolutionX) 0 1_000_000_000 + 1
solutionY = trueY2For solutionX - 99

renderNearSol = unlines [[evalCell i j | i <- [(solutionX-50)..(solutionX+150)]] | j <- [trueY2For solutionX-150..trueY2For solutionX+50]]
 where
   evalCell i j
     | (i,j) == (solutionX + 99, solutionY) = 'O'
     | (i,j) == (solutionX, solutionY + 99) = 'X'
     | cellAt i j = '#'
     | otherwise = '.'

main =
  --writeFile "renderNearSol" renderNearSol
  print (solutionX * 10000 + solutionY)