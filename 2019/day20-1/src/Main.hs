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

import Algorithm.Search (aStar, dijkstra)
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isUpper)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, maximumBy, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import Data.MemoTrie
import Data.Ord (comparing)
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
import Graphics.Gloss
import Linear (V2 (..), _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Pos = V2 Int

type Input = UA.Array Pos Char

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = arrayFromList2D . lines

type Graph = Map Pos [Pos]

toGraph :: Input -> (Graph, Pos, Pos)
toGraph arr = (M.unionWith (++) (dotsGraph arr) (labelsGraph lbls), start lbls, end lbls)
  where
    lbls = labels arr

type Labels = Map String [Pos]

labels :: Input -> Labels
labels arr = M.unionsWith (++) [left, right, above, below]
  where
    (V2 w h) = arraySize arr
    hTripletAt i j = (arr UA.! V2 i j, arr UA.! V2 (i + 1) j, arr UA.! V2 (i + 2) j)
    vTripletAt i j = (arr UA.! V2 i j, arr UA.! V2 i (j + 1), arr UA.! V2 i (j + 2))
    left = M.fromListWith (++) [([a, b], [V2 (i + 2) j]) | j <- [0 .. h -1], i <- [0 .. w -3], let (a, b, c) = hTripletAt i j, isUpper a, isUpper b, c == '.']
    right = M.fromListWith (++) [([b, c], [V2 i j]) | j <- [0 .. h -1], i <- [0 .. w -3], let (a, b, c) = hTripletAt i j, a == '.', isUpper b, isUpper c]
    above = M.fromListWith (++) [([a, b], [V2 i (j + 2)]) | i <- [0 .. w -1], j <- [0 .. h -3], let (a, b, c) = vTripletAt i j, isUpper a, isUpper b, c == '.']
    below = M.fromListWith (++) [([b, c], [V2 i j]) | i <- [0 .. w -1], j <- [0 .. h -3], let (a, b, c) = vTripletAt i j, a == '.', isUpper b, isUpper c]

dotsGraph :: Input -> Graph
dotsGraph arr = M.unionsWith (++) (map localGraph dots)
  where
    dots = [pos | (pos, '.') <- UA.assocs arr]
    localGraph pos1 = M.singleton pos1 [pos2 | pos2 <- neighbors2D (arraySize arr) pos1, arr UA.! pos2 == '.']

labelsGraph :: Labels -> Graph
labelsGraph labels = M.fromList $ concat [[(p1, [p2]), (p2, [p1])] | [p1, p2] <- M.elems labels]

start :: Labels -> Pos
start labels = head (labels M.! "AA")

end :: Labels -> Pos
end labels = head (labels M.! "ZZ")

solve input = length (fromJust (bfs (graph M.!) start (== end))) - 1
  where
    (graph, start, end) = toGraph input

-- DISPLAY

bfss ::
  Ord a =>
  -- | neighbors
  (a -> [a]) ->
  -- | source
  a ->
  -- | paths
  [[a]]
bfss neighbors source = go (S.singleton source) (Seq.singleton [source])
  where
    go _ Empty = []
    go seen ([] :<| _) = error "unexpected empty path"
    go seen (path@(cell : _) :<| paths) =
      path :
      let ns = S.fromList (neighbors cell)
       in go
            (S.union ns seen)
            (paths <> (S.difference ns seen & S.toList & map (: path) & Seq.fromList))

toPoint :: V2 Int -> Point
toPoint (V2 i j) = (fromIntegral i, fromIntegral j)

pathToPicture :: [Pos] -> Picture
pathToPicture path = color green (line (map toPoint path))

graphToPicture :: (Graph, Pos, Pos) -> Picture
graphToPicture (graph, toPoint -> (startx, starty), toPoint -> (endx, endy)) =
  color white (translate startx starty (circleSolid 1))
    <> color blue (translate endx endy (circleSolid 1))
    <> pictures [edge p1 p2 | (p1, ps) <- M.toList graph, p2 <- ps]
  where
    edge p1 p2 = color (if close p1 p2 then greyN 0.5 else red) $ line [toPoint p1, toPoint p2]
    close p1 p2 = sum (fmap abs (p2 - p1)) == 1

displayGraph :: (Graph, Pos, Pos) -> IO ()
displayGraph graph = display FullScreen black (scale 1 (-1) (graphToPicture graph))

displayFrame :: (Graph, Pos, Pos) -> [Pos] -> Picture
displayFrame graph path = scale 1 (-1) $ graphToPicture graph <> pathToPicture path

frames :: (Graph, Pos, Pos) -> [[Pos]] -> [Picture]
frames graph = map (displayFrame graph)

animateGraph :: (Graph, Pos, Pos) -> IO ()
animateGraph g@(graph, start, end) = animate FullScreen black (\t -> frs V.! floor (t * 10))
  where
    paths = bfss (graph M.!) start
    frs = V.fromList (frames g paths)

displayLongestPath :: (Graph, Pos, Pos) -> IO ()
displayLongestPath g@(graph, start, end) = display FullScreen black (displayFrame g longestPath)
  where
    longestPath = maximumBy (comparing length) (bfss (graph M.!) end)

test = do
  arr <- parse <$> readFile "input.txt"
  displayLongestPath (toGraph arr)