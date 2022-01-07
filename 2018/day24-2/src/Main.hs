-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow ((&&&))
import Control.Lens (at, each, filteredBy, folded, isn't, ix, only, traversed, (%~), (&), (+~), (.=), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import Control.Monad.State (State, execState, gets)
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
import Data.List (elemIndex, sortOn, (\\), nub)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.MemoTrie
import Data.Ord
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

data Army = Infection | ImmuneSystem
  deriving (Show, Generic, Eq)

data Group = Group
  { groupId :: Int,
    army :: Army,
    numUnits :: Int,
    hitPoints :: Int,
    weakTo :: [String],
    immuneTo :: [String],
    damageAmount :: Int,
    damageType :: String,
    initiative :: Int
  }
  deriving (Show, Generic, Eq)

type Input = [Group]

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = runParserOrDie inputParser
  where
    parens = P.between (P.char '(') (P.char ')')
    word :: Parser String
    word = P.some P.alphaNumChar
    immuneParser = do
      P.string "immune to "
      word `P.sepBy1` P.string ", "
    weakToParser = do
      P.string "weak to "
      word `P.sepBy1` P.string ", "
    weakOrImmuneParser = do
      typ <- P.string "immune" P.<|> P.string "weak"
      P.string " to "
      words <- word `P.sepBy1` P.string ", "
      return (typ, words)
    weakImmuneParser =
      P.option [] (parens (weakOrImmuneParser `P.sepBy1` P.string "; ") <* P.char ' ')
    inputParser = do
      P.string "Immune System:"
      P.newline
      group1 <- groupParser ImmuneSystem `P.endBy1` P.newline
      P.newline
      P.string "Infection:"
      P.newline
      group2 <- groupParser Infection `P.endBy1` P.newline
      P.eof
      return (zipWith (\g i -> g {groupId = i}) (group1 ++ group2) [0 ..])
    groupParser army = do
      numUnits <- L.decimal
      P.string " units each with "
      hitPoints <- L.decimal
      P.string " hit points "
      weakImmune <- weakImmuneParser
      P.string "with an attack that does "
      damageAmount <- L.decimal
      P.string " "
      damageType <- word
      P.string " damage at initiative "
      initiative <- L.decimal
      return
        Group
          { groupId = 0,
            weakTo = fromMaybe [] (lookup "weak" weakImmune),
            immuneTo = fromMaybe [] (lookup "immune" weakImmune),
            ..
          }

effectivePower :: Group -> Int
effectivePower Group {..} = numUnits * damageAmount

damage :: Group -> Group -> Int
damage a@Group {damageType} Group {immuneTo, weakTo}
  | damageType `elem` immuneTo = 0
  | damageType `elem` weakTo = effectivePower a * 2
  | otherwise = effectivePower a

playRound :: Input -> Input
playRound groups =
  let targets = selectTargets targetSelectionOrder groups
   in M.elems (execState (attack targets attackOrder) (M.fromList (map (groupId &&& id) groups)))
  where
    targetSelectionOrder = sortOn (Down . (\g -> (effectivePower g, initiative g))) groups
    attackOrder = map groupId (sortOn (Down . initiative) groups)
    selectTargets :: [Group] -> [Group] -> [(Int, Int)]
    selectTargets [] _ = []
    selectTargets (a : as) ds =
      let potentialTargets = [d | d <- ds, army d /= army a && damage a d > 0]
       in case sortOn (Down . (\d -> (damage a d, effectivePower d, initiative d))) potentialTargets of
            (d : _) -> (groupId a, groupId d) : selectTargets as (ds \\ [d])
            [] -> selectTargets as ds
    attack :: [(Int, Int)] -> [Int] -> State (Map Int Group) ()
    attack _ [] = return ()
    attack targets (aid : aids) = do
      ma <- gets (M.!? aid)
      case ma of
        Nothing -> attack targets aids
        Just a ->
          case lookup aid targets of
            Nothing -> attack targets aids
            Just did -> do
              d <- gets (M.! did)
              let newNumUnits = numUnits d - (damage a d `div` hitPoints d)
              if newNumUnits > 0
                then ix did . #numUnits .= newNumUnits
                else at did .= Nothing
              attack targets aids

playout :: Input -> Input
playout = converge playRound

boost :: Input -> Int -> Input
boost groups i = groups & traversed . filteredBy (#army . only ImmuneSystem) . #damageAmount +~ i

immuneWins :: Input -> Bool
immuneWins groups = nub (map army (playout groups)) == [ImmuneSystem]

bisect :: (Int -> Bool) -> Int -> Int -> Int
bisect _ d u | traceShow (d, u) False = undefined
bisect pred d u
  | u == d+1 = u
  | otherwise =
      let i = d + ((u-d) `div` 2)
      in if pred i then bisect pred d i else bisect pred i u

solve input =
  let i = bisect (immuneWins . boost input) 0 1_000_000
  in playout (boost input i) & map numUnits & sum
