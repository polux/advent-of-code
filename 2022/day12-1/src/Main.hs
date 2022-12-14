-- Copyright 2022 Google LLC.
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

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
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
import Linear (V2 (..), V3 (..), _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import Vis

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

type Input = UA.Array (V2 Int) Char

-- type Output = Input

main :: IO ()
main = do
  s <- readFile "input"
  let input = parse s
  let paths = V.fromList (solve input)
  animate defaultOpts (vis input paths)


parse :: String -> Input
parse = arrayFromList2D . lines

-- solve :: Input -> Output
solve input = bfsTrace neighbors isEnd start
 where
  bounds = arraySize input
  start = head [pos | (pos, v) <- UA.assocs input, v == 'S']
  isEnd v = input UA.! v == 'E'
  neighbors v = [v' | v' <- neighbors2D bounds v, elevation (input UA.! v') - elevation (input UA.! v) <= 1]

elevation 'S' = 0
elevation 'E' = 25
elevation c = ord c - ord 'a'

vis :: Input -> Vector [V2 Int] -> Float -> VisObject Float
vis input paths t = VisObjects [visTerrain input, visPath input (paths V.! floor (t*100))]

visTerrain :: Input -> VisObject Float
visTerrain input =
  VisObjects
    [ Trans (V3 x y (-h/2)) (Box (1, 1, h) Solid (colorFor h))
    | (fmap fromIntegral -> V2 x y, fromIntegral . elevation -> h) <- UA.assocs input
    ]
 where
  colorFor h = makeColor 0 (h / 25) 0 1

visPath :: Input -> [V2 Int] -> VisObject Float
visPath input path =
  Line
    (Just 5)
    [ V3 (fromIntegral x) (fromIntegral y) (-fromIntegral h - 0.5)
    | (V2 x y) <- path
    , let h = elevation (input UA.! V2 x y)
    ]
    red

bfsTrace
  :: Ord a
  => (a -> [a])  -- ^ neighbors
  -> (a -> Bool) -- ^ isTarget
  -> a           -- ^ source
  -> [[a]]   -- ^ path from source to target
bfsTrace neighbors isTarget source = go (S.singleton source) (Seq.singleton [source])
 where
  go _ Empty = []
  go seen ([] :<| _) = error "unexpected empty path"
  go seen (path@(cell : _) :<| paths)
    | isTarget cell = [reverse path]
    | otherwise =
        let ns = neighbors cell & filter (`S.notMember` seen)
         in reverse path : go
              (foldr S.insert seen ns)
              (paths <> Seq.fromList [n : path | n <- ns])