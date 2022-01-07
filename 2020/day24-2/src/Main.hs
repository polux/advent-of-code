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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V3, V2 (..))
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
import Linear.V3 (V3, V3)
import Linear (V3(V3))
import Data.List (foldl')

-- #endregion

type Input = [[Dir]]

type Output = Int

data Dir = E | SE | SW | W | NW | NE
  deriving (Show, Enum)

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = map parseLine (lines str)
 where
   parseLine [] = []
   parseLine ('e':cs) = E : parseLine cs
   parseLine ('s':'e':cs) = SE : parseLine cs
   parseLine ('s':'w':cs) = SW : parseLine cs
   parseLine ('w':cs) = W : parseLine cs
   parseLine ('n':'w':cs) = NW : parseLine cs
   parseLine ('n':'e':cs) = NE : parseLine cs
   parseLine _ = error "cannot parse"

move :: Dir -> V3 Int -> V3 Int
move E (V3 x y z) = V3 (x+1) (y-1) z
move W (V3 x y z) = V3 (x-1) (y+1) z
move SE (V3 x y z) = V3 x (y-1) (z+1)
move NW (V3 x y z) = V3 x (y+1) (z-1)
move SW (V3 x y z) = V3 (x-1) y (z+1)
move NE (V3 x y z) = V3 (x+1) y (z-1)

coord :: [Dir] -> V3 Int
coord = foldl' (flip move) (V3 0 0 0)

flipCell :: Set (V3 Int) -> V3 Int -> Set (V3 Int)
flipCell set cell | cell `S.member` set = S.delete cell set
                  | otherwise = S.insert cell set

neighbors :: V3 Int -> [V3 Int]
neighbors cell = map (`move` cell) [E .. NE]

numBlackNeighbors :: Set (V3 Int) -> V3 Int -> Int
numBlackNeighbors set cell =
  length (filter (`S.member` set) (neighbors cell))

candidates :: Set (V3 Int) -> Set (V3 Int)
candidates set = set <> S.unions (S.map (S.fromList.neighbors) set)

step :: Set (V3 Int) -> Set (V3 Int)
step set = S.filter isBlackNext (candidates set)
 where
   isBlackNext cell =
     let n = numBlackNeighbors set cell
     in if cell `S.member` set
       then n == 1 || n == 2
       else n == 2

solve :: Input -> Output
solve input = length (iterate step first !! 100)
  where
    first = foldl' flipCell mempty (map coord input)
