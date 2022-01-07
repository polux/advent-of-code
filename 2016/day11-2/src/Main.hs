-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
-- #endregion

module Main where

-- #region imports

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (intercalate, sort, subsequences, (\\), sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (guard, forM_)
import Algorithm.Search (aStar)
import Control.Arrow ((>>>))

-- #endregion

{-
The first floor contains a promethium generator and a promethium-compatible microchip.
The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
The fourth floor contains nothing relevant.
-}

type Floor = Int

data Element = Promethium | Cobalt | Curium | Ruthenium | Plutonium | Elerium | Dilithium
  deriving (Eq, Ord)

instance Show Element where
  show e = case e of
     Promethium -> "P"
     Cobalt -> "Co"
     Curium -> "Cu"
     Ruthenium -> "R"
     Plutonium -> "P"
     Elerium -> "E"
     Dilithium -> "D"

data Item = Generator Element | Microchip Element
 deriving (Eq, Ord)

instance Show Item where
  show (Generator e) = show e <> "+"
  show (Microchip e) = show e <> "-"

type Items = Map Item Floor


{-

we reach this input with a sequence of 9 steps

A+,A-,B+,B-,C+,C-     D+,E+,F+
A+,A-,B+,B-           C+,C-,D+,E+,F+
A+,A-,B+,B-,D+        C+,C-,E+,F+
A+,A-,D+              B+,B-,C+,C-,E+,F+
A+,A-,D+,E+           B+,B-,C+,C-,F+
D+,E+                 A+,A-,B+,B-,C+,C-,F+
D+,E+,F+              A+,A-,B+,B-,C+,C-
D+                    E+,F+,A+,A-,B+,B-,C+,C-
D+,E+                 F+,A+,A-,B+,B-,C+,C-
                      D+,E+,F+,A+,A-,B+,B-,C+,C-
-}

input = M.fromList
  [
    (Generator Promethium, 2),
    (Microchip Promethium, 2),
    (Generator Elerium, 2),
    (Microchip Elerium, 2),
    (Generator Dilithium, 2),
    (Microchip Dilithium, 2),
    (Generator Cobalt, 2),
    (Generator Curium, 2),
    (Generator Ruthenium, 2),
    (Generator Plutonium, 2),
    (Microchip Cobalt, 3),
    (Microchip Curium, 3),
    (Microchip Ruthenium, 3),
    (Microchip Plutonium, 3)
  ]

main :: IO ()
main = print (solve + 9) -- nine step to reach input

data Node = Node { elevator :: Floor, items :: Items }
  deriving (Eq, Ord, Show, Generic)

showNode :: Node -> String
showNode Node{elevator,items} = show (elevator, map (sort . itemsOnFloor items) [1..4])

itemsOnFloor :: Items -> Floor -> [Item]
itemsOnFloor items floor = items & M.filter (==floor) & M.keys

validFloor :: [Item] -> Bool
validFloor items = S.null generators || S.null (chips `S.difference` generators)
  where
    chips = S.fromList [x | Microchip x <- items]
    generators = S.fromList [x | Generator x <- items]

validNode :: Node -> Bool
--validNode node | trace (showNode node) False = undefined
validNode Node{elevator, items} = all (validFloor . itemsOnFloor items) [1..4]

nextFloors :: Floor -> [Floor]
nextFloors floor = [floor+1, floor-1] & filter valid
 where
   valid floor = floor >= 2 && floor <= 4 -- we never need to go back to floor 1

choseOneOrTwo :: [Item] -> [[Item]]
choseOneOrTwo xs = subsequences xs & filter oneOrTwo & filter validFloor
 where
   oneOrTwo ys = length ys == 1 || length ys == 2

neighbors :: Node -> [Node]
neighbors Node{elevator, items} = filter validNode $ do
  destinationFloor <- nextFloors elevator
  itemsToMove <- choseOneOrTwo (itemsOnFloor items elevator)
  return (Node destinationFloor (M.fromList (map (,destinationFloor) itemsToMove) <> items))

wins :: Node -> Bool
wins Node{items} = all (==4) (M.elems items)

nodeCost :: Node -> Int
nodeCost Node{items} = (length (itemsOnFloor items 1) `div` 2)*3 + (length (itemsOnFloor items 2) `div` 2)*2 + (length (itemsOnFloor items 3) `div` 2)

allSolutions :: Set Node -> Int -> Node -> [[Node]]
allSolutions _ (-1) _ = []
allSolutions _ l node | wins node = return [node]
allSolutions seen l node = do
  n <- neighbors node
  guard $ n `S.notMember` seen
  path <- allSolutions (S.insert n seen) (l-1) n
  return (node:path)

solve = (bfs neighbors (Node 2 input) wins & length) - 1


