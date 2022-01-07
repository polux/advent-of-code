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
-- #endregion
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

-- #region imports

-- #region imports
-- #region imports

-- #region imports
import Control.Arrow ((>>>),(***), Arrow (second))
import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict (MonadState (get), State, execState, gets, modify, runState)
import Control.Monad.Trans.Maybe
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.MemoTrie
import Data.Ord (comparing)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic, K1 (K1))
import Linear (V2 (..), _x, _y)
import Optics (AffineTraversal', Prism', Traversal', at, each, filtered, filteredBy, folded, has, isn't, ix, lensVL, only, preuse, sans, sumOf, to, traversed, use, view, (%), (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4, lengthOf)
import Optics.Label ()
import Optics.Operators.Unsafe ((^?!))
import Optics.State.Operators ((.=), (?=))
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Input = Map (V2 Int) Char

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse input = M.fromList [(V2 i j, c) | (row, j) <- zip (lines input) [0 ..], (c, i) <- zip row [0 ..]]

toLists :: Grid -> [[Cell]]
toLists grid = [row j | j <- [0 .. maxJ]]
  where
    row j = [grid M.! V2 i j | i <- [0 .. maxI]]
    maxJ = maximum (map (view (lensVL _y)) (M.keys grid))
    maxI = maximum (map (view (lensVL _x)) (M.keys grid))

display :: Grid -> String
display grid = unlines (map (map displayCell) (toLists grid))
  where
    displayCell (Creature (C Goblin _)) = 'G'
    displayCell (Creature (C Elf _)) = 'E'
    displayCell Wall = '#'
    displayCell Floor = '.'

data CreatureType = Goblin | Elf
  deriving (Eq, Ord, Show, Generic)

data Creature = C {cType :: CreatureType, cHp :: Int}
  deriving (Eq, Show, Generic)

data Cell = Creature Creature | Wall | Floor
  deriving (Eq, Show, Generic)

type Grid = Map (V2 Int) Cell

toGrid :: Input -> Grid
toGrid = M.map toCell
  where
    toCell 'G' = Creature (C Goblin 200)
    toCell 'E' = Creature (C Elf 200)
    toCell '#' = Wall
    toCell '.' = Floor
    toCell c = error ("unknown cell type " <> [c])

orderedUnits :: Grid -> [V2 Int]
orderedUnits grid =
  M.toList grid
    & sortOn (\(V2 x y, _) -> (y, x))
    & filter (has (_2 % #_Creature))
    & map fst

neighboringCells :: Num a => V2 a -> [V2 a]
neighboringCells (V2 i j) = [V2 i (j -1), V2 (i + 1) j, V2 i (j + 1), V2 (i -1) j]

isFloorAt :: Grid -> V2 Int -> Bool
isFloorAt grid coords = has (ix coords % #_Floor) grid

creatureAt :: V2 Int -> AffineTraversal' Grid Creature
creatureAt cell = ix cell % #_Creature

creatureTypeAt :: V2 Int -> AffineTraversal' Grid CreatureType
creatureTypeAt cell = creatureAt cell % #cType

creatureHpAt :: V2 Int -> AffineTraversal' Grid Int
creatureHpAt cell = creatureAt cell % #cHp

bfsLevels :: Grid -> V2 Int -> [Set (V2 Int)]
bfsLevels grid origin = origSet : go origSet origSet
  where
    origSet = S.singleton origin
    concatMapSet f s = S.unions (S.map (S.fromList . f) s)
    go seen xs
      | S.null xs = []
      | otherwise =
        let newLevel = xs & concatMapSet neighboringCells & S.filter (isFloorAt grid) & (`S.difference` seen)
         in newLevel : go (seen `S.union` newLevel) newLevel

touchesEnemyUnit :: Grid -> CreatureType -> V2 Int -> Bool
touchesEnemyUnit grid enemyType coord = neighboringCells coord & any (\cell -> has (creatureTypeAt cell % only enemyType) grid)

shortestInranges :: Grid -> CreatureType -> V2 Int -> [V2 Int]
shortestInranges grid enemyType origin =
  bfsLevels grid origin
    & map (S.filter (touchesEnemyUnit grid enemyType))
    & filter (not . S.null)
    & fmap S.toList
    & headDef []

bestInRange :: Grid -> CreatureType -> V2 Int -> Maybe (V2 Int)
bestInRange grid enemyType origin =
  shortestInranges grid enemyType origin
    & minimumByMay (comparing (\(V2 x y) -> (y, x)))

shortestPathLength :: Grid -> V2 Int -> V2 Int -> Maybe Int
shortestPathLength grid source target =
  length
    <$> bfs
      (neighboringCells >>> filter (isFloorAt grid))
      source
      (== target)

enemyOf :: CreatureType -> CreatureType
enemyOf Elf = Goblin
enemyOf Goblin = Elf

bestMove :: V2 Int -> Grid -> Maybe (V2 Int)
bestMove coords grid
  | any isEnemy neighbors = Just coords
  | otherwise =
    case bestInRange grid (enemyOf typ) coords of
      Nothing -> Nothing
      Just target ->
        neighbors
          & filter (isFloorAt grid)
          & mapMaybe (\cell -> (,cell) <$> shortestPathLength grid cell target)
          -- we compare on length first, then cell coordinates
          & minimumByMay (comparing (\(len, V2 x y) -> (len, y, x)))
          & fmap snd
  where
    neighbors = neighboringCells coords
    isEnemy c = has (creatureTypeAt c % only (enemyOf typ)) grid
    typ = grid ^?! ix coords % #_Creature % #cType

bestTarget :: V2 Int -> Grid -> Maybe (V2 Int)
bestTarget coords grid =
  neighboringCells coords
    -- scores of neighboring enemies
    & mapMaybe (\cell -> grid ^? creatureAt cell % filteredBy (#cType % only enemyType) % #cHp % to (,cell))
    -- we compare on hit points first, then cell coordinates
    & minimumByMay (comparing (\(hp, V2 x y) -> (hp, y, x)))
    & fmap snd
  where
    enemyType = enemyOf (grid ^?! creatureTypeAt coords)

attack :: Int -> V2 Int -> Grid -> Grid
attack elfAp target grid =
  if grid ^?! creatureHpAt target <= attackPoints
    then grid & ix target .~ Floor
    else grid & creatureHpAt target %~ (\hp -> hp - attackPoints)
 where
   attackPoints =
     case grid ^?! creatureTypeAt target of
       Elf -> 3
       Goblin -> elfAp

move :: V2 Int -> V2 Int -> Grid -> Grid
move source target grid =
  grid
    & ix source .~ Floor
    & ix target .~ (grid M.! source)

hasEnemiesLeft :: V2 Int -> Grid -> Bool
hasEnemiesLeft coords grid = has (folded % #_Creature % #cType % only enemyType) grid
  where
    enemyType = enemyOf (grid ^?! creatureTypeAt coords)

playRound :: Int -> ExceptT () (State Grid) ()
playRound elfAp = do
  units <- gets orderedUnits
  units `forM_` \unit -> do
    exists <- gets (has (creatureAt unit))
    when exists $ do
      hasEnemies <- gets (hasEnemiesLeft unit)
      unless hasEnemies (throwError ())
      newPosM <- gets (bestMove unit)
      newPosM `forM_` \newPos -> do
        modify (move unit newPos)
        targetM <- gets (bestTarget newPos)
        targetM `forM_` \target ->
          modify (attack elfAp target)

playGame :: Int -> Int -> State Grid Int
playGame elfAp i = do
  grid <- get
  if length (S.fromList (grid ^.. folded % #_Creature % #cType)) < 2
    then return i
    else do
      res <- runExceptT (playRound elfAp)
      case res of
        Left () -> return i
        Right () -> playGame elfAp (i + 1)

playGrid :: Grid -> Int -> (Int, Grid)
playGrid grid elfAp = runState (playGame elfAp 0) grid

noElfDies :: Grid -> Int -> Bool
noElfDies grid elfAp = numElves grid == numElves finalGrid
  where
    numElves g = lengthOf (folded % #_Creature % #cType % only Elf) g
    (numRounds, finalGrid) = playGrid grid elfAp
    finalNumElves = lengthOf (folded % #_Creature % #cType % only Elf) finalGrid

search :: (Show t, Integral t) => (t -> Bool) -> t -> t -> t
search _ l u | traceShow ("search", l,u) False = undefined
search p l u
  | l+1 == u = u
  | otherwise = let m = l+((u-l) `div` 2) in if p m then search p l m else search p m u

-- On my input, and only on my input, the total hit point sum is 3 points too low
-- (opposite of day15-1), no idea why.
solve input =
  let hp = search (noElfDies grid) 3 300
  in  playGrid grid hp & second (sumOf (folded % #_Creature % #cHp))
 where
   grid = toGrid input
