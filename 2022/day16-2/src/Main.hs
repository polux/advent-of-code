{-# LANGUAGE BangPatterns #-}
-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second, (&&&)), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
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
import Data.List (elemIndex, inits, minimumBy, permutations, sort, sortOn, tails, nub, foldl')
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, isJust, fromMaybe)
import Data.MemoTrie
import qualified Data.PQueue.Max as PQ
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
import Linear (V2 (..), vector, _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

import Data.Bits
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import GHC.Exts (Down (..))
import Text.Regex.Pcre2 (regex)
import qualified Text.Megaparsec as S

parse :: String -> Input
parse = M.fromList . map parseLine . T.lines . T.pack
 where
  toInt = read . T.unpack
  parseLine [regex|Valve (?<v>.*) has flow rate=(?<x>\d+); tunnels? leads? to valves? (?<vs>.*)|] = (T.unpack v, (toInt x, splitOn ", " (T.unpack vs)))

type Input = Map String (Int, [String])

-- type Output = Int

main :: IO ()
main = do
  --readFile "example" >>= print . solve . parse
  --readFile "input" >>= putStrLn. toDot . convert . parse
  readFile "input" >>= print . solve . parse

-- readFile "example" >>= print . convert . parse

type Graph = Map Int (Int, [Int])

toDot :: Graph -> String
toDot g = unlines vertices ++ unlines (map edgeToDot $ edges $ M.toList g)
  where
    vertices = ["v" ++ show v ++ " [label = \"" ++ show i ++ "\"];" | (v, (i,_)) <- M.toList g]
    edges g = [(v, v') | (v, (i, vs)) <- g, v' <- vs] -- & sort & nub
    edgeToDot (v, v') = "v" ++ show v ++ " -> v" ++ show v' ++ ";"

convert :: Input -> Graph
convert input = M.fromList [(number v, (c, map number vs)) | (v, (c, vs)) <- M.toList input]
 where
  numbers = M.fromList (zip (M.keys input) [1 ..])
  number v = numbers M.! v

type BitSet = Int
type Vertex = Int
type Time = Int
type Duration = Int

data State = S {v1 :: Vertex, v1Hist :: BitSet, v2 :: Vertex, v2Hist :: BitSet, valves :: BitSet, time :: Time, production :: Int}
  deriving (Eq, Ord, Generic, Show)


data SState = SS {v1 :: Vertex, v2 :: Vertex, valves :: BitSet, time :: Time}
  deriving (Eq, Ord, Generic, Show)

simplify :: State -> SState
simplify (S v1 _ v2 _ valves t p) = SS v1 v2 valves t

mkS :: Vertex -> BitSet -> Vertex -> BitSet -> BitSet -> Time -> Int -> State
mkS v1 v1Hist v2 v2Hist
  | v1 < v2 = S v1 v1Hist v2 v2Hist
  | otherwise = S v2 v2Hist v1 v1Hist


percentSame :: Ord a => (State -> a) -> PQ.MaxQueue (Int, State) -> Int
percentSame f q = ((PQ.toList q & map (f . snd) & S.fromList & length) * 100) `div` PQ.size q

-- solve :: Input -> Output
solve input = traceShow (upperBound initState) $ go mempty 0 (PQ.singleton (upperBound initState, initState))
 where
  initState = mkS 1 0 1 0 initValves 25 0
  initValves = foldl' setBit 0 [v | (v,(0,_)) <- M.toList graph]
  graph = convert input
  vertices = M.keys graph
  yield v = fst (graph M.! v)
  vertexNeighbors v = snd (graph M.! v)

  go :: Map SState Int -> Int -> PQ.MaxQueue (Int, State) -> Int
  go seen m (PQ.maxView -> Nothing) = m
  go seen m (PQ.maxView -> Just ((bound, state), states))
    | bound < m = go seen m states
    | state.production < fromMaybe minBound (M.lookup (simplify state) seen) = go seen m states
    | otherwise =
        let pm' = max m state.production
            m' = (if state.production > m then traceShow (m, PQ.size states) pm' else pm')
            ns = neighbors state & S.fromList & S.toList & filter (valid m' seen)
         in go
              (foldr (\n m -> M.insert (simplify n) n.production m) seen ns)
              m'
              (foldr (PQ.insert . (upperBound &&& id)) states ns)

  minDelta valves = minimumDef maxBound [distance v v' | (v : vs) <- tails (closed valves), v' <- vs]
  mMinDelta = memo minDelta

  valid m seen s = s.time > 0 && (upperBound s >= m) && (fromMaybe minBound (M.lookup (simplify s) seen) < s.production)

  neighbors :: State -> [State]
  neighbors (S v vHist e eHist valves t c) =
    concat
      [ [ mkS v 0 e 0 (valves `setBit` v `setBit` e) (t - 1) (c + t * yield v + t * yield e)
        | not (testBit valves v)
        , not (testBit valves e)
        , e /= v
        ]
      , [ mkS v' (setBit vHist v') e 0 (setBit valves e) (t - 1) (c + t * yield e)
        | not (testBit valves e)
        , v' <- vertexNeighbors v
        , not (testBit vHist v')
        ]
      , [ mkS v 0 e' (setBit eHist e') (setBit valves v) (t - 1) (c + t * yield v)
        | not (testBit valves v)
        , e' <- vertexNeighbors e
        , not (testBit eHist e')
        ]
      , [ mkS v' (setBit vHist v') e' (setBit eHist e') valves (t - 1) c
        | v' <- vertexNeighbors v
        , not (testBit vHist v')
        , e' <- vertexNeighbors e
        , not (testBit eHist e')
        ]
      ]

  closed :: BitSet -> [Vertex]
  closed set = [v | v <- vertices, not (testBit set v)]

  upperBound :: State -> Int
  upperBound (S _ _ _ _ valves _ p) | null (closed valves) = p
  upperBound (S v1 _ v2 _ valves t p) = p + sum (zipWith (*) ts products)
   where
    sortDesc = sortOn Down
    products = sortDesc (map yield (closed valves))
    v1MinMoves = minimum [distance v1 v | v <- closed valves]
    v2MinMoves = minimum [distance v2 v | v <- closed valves]
    minD = 1 -- mMinDelta valves
    t1 = t - v1MinMoves
    t2 = t - v2MinMoves
    ts = merge [t1, t1 - (minD+1) .. 0] [t2, t2 - (minD+1) .. 0]
    merge (x : xs) (y : ys)
      | x > y = x : merge xs (y : ys)
      | otherwise = y : merge (x : xs) ys
    merge [] ys = ys
    merge xs [] = xs

  distance :: Vertex -> Vertex -> Int
  distance i j = mFW i j (maximum vertices)

  mFW :: Vertex -> Vertex -> Int -> Int
  mFW = memo3 floydWarshall

  floydWarshall :: Vertex -> Vertex -> Int -> Int
  floydWarshall i j 0
    | i == j = 0
    | j `elem` vertexNeighbors i = 1
    | otherwise = 1_000_000_000_000
  floydWarshall i j k = min (mFW i j (k - 1)) (mFW i k (k - 1) + mFW k j (k - 1))