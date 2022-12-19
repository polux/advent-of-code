-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, pre, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), (^?!), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (Foldable (foldl'), foldrM)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, maximumBy, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.MemoTrie
import Data.Ord (comparing)
import qualified Data.OrdPSQ as PQueue
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
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

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

type Resource = Int
type BluePrint = (Int, [Recipe])
type Recipe = (Resource, [Dependency])
type Dependency = (Resource, Int)
type Input = [BluePrint]

type Output = Int

main :: IO ()
main = readFile "input" >>= print . solve . parse

kMaxTime :: Int
kMaxTime = 24

kOre, kClay, kObsidian, kGeode :: Resource
kOre = 0
kClay = 1
kObsidian = 2
kGeode = 3

parse :: String -> Input
parse = map parseLine . lines
 where
  parseLine (splitOn ":" -> [words -> [_, read -> i], map parseRobot . (init . splitOn ".") -> robots]) = (i, robots)
  parseRobot (splitOn "costs" -> [words -> [_, parseResource -> ty, _], map parseDep . splitOn "and" -> deps]) = (ty, deps)
  parseDep (words -> [read -> c, parseResource -> dep]) = (dep, c)
  parseResource "ore" = kOre
  parseResource "clay" = kClay
  parseResource "obsidian" = kObsidian
  parseResource "geode" = kGeode

data State = S {robots :: V.Vector Int, resources :: V.Vector Int, time :: Int}
  deriving (Show, Eq, Ord, Generic)

initState :: State
initState = S{robots = V.fromList [1, 0, 0, 0], resources = V.fromList [0, 0, 0, 0], time = 0}

quantity :: State -> Resource -> Int
quantity state res = state ^?! #resources . ix res

apply :: State -> Recipe -> Maybe State
apply state (res, deps) = do
  resources' <- foldrM consume (resources state) deps
  return $
    state
      & #resources .~ resources'
      & #robots . ix res %~ (+ 1)
 where
  consume :: Dependency -> V.Vector Int -> Maybe (V.Vector Int)
  consume (r, q) m =
    if m ^?! ix r < q then Nothing else Just (m & ix r %~ (\x -> x - q))

neighbors :: [Recipe] -> State -> [State]
neighbors recipes state = map advance (state : mapMaybe (apply state) recipes)
 where
  advance s =
    s
      & #resources %~ V.zipWith (+) (state ^. #robots)
      & #time %~ (+ 1)

maxNumGeodes :: [Recipe] -> Int
maxNumGeodes recipes
  | upperBound initState simpleRecipes == 0 = 0
  | otherwise = go mempty 0 (Seq.singleton initState)
 where
  simpleRecipes = simplify recipes
  go :: S.Set State -> Int -> Seq State -> Int
  go seen m Empty = m
  go seen m (state :<| states) =
    let m' = max m (quantity state kGeode)
     in go
          (S.insert state seen)
          m'
          ((neighbors recipes state & filter (`S.notMember` seen) & filter (valid m') & Seq.fromList) <> states)
   where
    valid m s = s ^. #time <= kMaxTime && upperBound s simpleRecipes >= m
    qGeode = quantity state kGeode

best :: State -> Int
best state = foldl' (+) g0 (take (kMaxTime - t0) [r0 ..])
 where
  g0 = quantity state kGeode
  t0 = time state
  r0 = state ^?! #robots . ix kGeode

simplify :: [Recipe] -> [Recipe]
simplify = map simplifyRecipe
 where
  simplifyRecipe (res, deps) =
    ( res
    , if
          | res == kOre -> []
          | res == kGeode -> filter ((== kObsidian) . fst) deps
          | res == kObsidian -> filter ((== kClay) . fst) deps
          | otherwise -> deps
    )

upperBound :: State -> [Recipe] -> Int
upperBound state recipes = go state
 where
  maybeApply s recipe = fromMaybe s (apply s recipe)
  go s
--    | traceShow s False = undefined
    | s ^. #time == kMaxTime = quantity s kGeode
    | otherwise =
        go
          ( foldl' maybeApply s recipes
              & #resources %~ V.zipWith (+) (s ^. #robots)
              & #time %~ (+ 1)
          )

solve input =
  sum [i * traceShowId (maxNumGeodes rs) | (i, rs) <- input]
