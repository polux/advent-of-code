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

import Control.Lens ((+~), each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Control.Arrow ((>>>))

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

type Input = [(Arr, Arr)]


type Arr = UA.Array (V2 Int) Char
type Size = V2 Int
data Grid = Leaf Size Arr | HAppend Size Grid Grid | VAppend Size Grid Grid

size :: Grid -> Size
size (Leaf s _) = s
size (HAppend s _ _) = s
size (VAppend s _ _) = s

toGrid :: Arr -> Grid
toGrid a = Leaf (arraySize a) a

happend :: Grid -> Grid -> Grid
happend a1 a2 = HAppend (V2 (a1w+a2w) h) a1 a2
 where
    (V2 a1w h) = size a1
    (V2 a2w _) = size a2

vappend :: Grid -> Grid -> Grid
vappend a1 a2 = VAppend (V2 w (a1h+a2h)) a1 a2
  where
    (V2 w a1h) = size a1
    (V2 _ a2h) = size a2

toArr :: Grid -> Arr
toArr grid = UA.array (V2 0 0, V2 (w-1) (h-1)) (go (V2 0 0) grid)
 where
  (V2 w h) = size grid
  go offset (Leaf _ arr) = UA.assocs arr & traverse . _1 +~ offset
  go offset (HAppend _ a1 a2) = go offset a1 ++ go (offset + V2 w 0) a2
   where
     (V2 w _) = size a1
  go offset (VAppend _ a1 a2) = go offset a1 ++ go (offset + V2 0 h) a2
   where
     (V2 _ h) = size a1

divide :: Int -> Arr -> [[Arr]]
divide step a = [[subArray i j | i <- [0,step..w-1]] | j <- [0,step..h-1]]
 where
   (V2 w h) = arraySize a
   subArray i j = UA.ixmap (V2 0 0, V2 (step-1) (step-1)) (+ V2 i j) a

main :: IO ()
main = getContents >>= putStrLn . solve . parse

parse :: String -> Input
parse = map parseLine . lines
 where
   parseLine str =
     let [lhs,rhs] = splitOn " => " str
     in (parseArray lhs, parseArray rhs)
   parseArray str = arrayFromList2D (splitOn "/" str)

collage :: [[Arr]] -> Grid
collage as = vconcat (map (hconcat . map toGrid) as)
  where
    hconcat = foldr1 happend
    vconcat = foldr1 vappend

step :: Map Arr Arr -> Arr -> Arr
step rules a = a & divide step & map (map (rules M.!)) & collage & toArr
  where
    (V2 w h) = arraySize a
    step = if even w then 2 else 3

count :: Arr -> Int
count a = UA.elems a & filter (=='#') & length

initArr :: Arr
initArr = arrayFromList2D [".#.","..#","###"]

rotate90 :: Arr -> Arr
rotate90 bitmap = UA.ixmap (UA.bounds bitmap) newIdx bitmap
  where
    newIdx (V2 i j) = V2 j (w - 1 - i)
    (V2 w _) = arraySize bitmap

mirror :: Arr -> Arr
mirror bitmap = UA.ixmap (UA.bounds bitmap) newIdx bitmap
  where
    newIdx (V2 i j) = V2 (w - 1 - i) j
    (V2 w _) = arraySize bitmap

variants :: Arr -> [Arr]
variants bitmap = rotations ++ map mirror rotations
  where
    rotations = take 4 (iterate rotate90 bitmap)

solve input =
  --count (iterate (step (complete input)) initArr !! 18) & show
  --iterate (step (complete input)) initArr & take 6 & map (listFromArray2D >>> unlines) & unlines
  iterate (step (complete input)) initArr & map count & zip [0..] & map show & unlines
  --iterate (step (complete input)) initArr & map arraySize & zip [0..] & map show & unlines
 where
   complete rules = M.fromList $ concatMap expand rules
   expand (lhs, rhs) = map (,rhs) (variants lhs)
