-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens ((%=), each, folded, isn't, traversed, use, (%~), (&), (+=), (-=), (.=), (.~), (?~), (^.), (^..), (^?))
import Control.Lens.Extras (is)
import Control.Monad (when)
import Control.Monad.State (gets, MonadState(get), State, evalState, runState)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (Foldable (toList), maximumBy, minimumBy)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Safe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

main :: IO ()
main = print solve

(bossInitialHitPoints, bossDamage, playerInitialHitPoints, playerInitialMana, hard) = (51, 9, 50, 500, True)

--(bossInitialHitPoints, bossDamage, playerInitialHitPoints, playerInitialMana, hard) = (14, 8, 10, 250, False)

missileCost = 53

drainCost = 73

shieldCost = 113

poisonCost = 173

rechargeCost = 229

playout :: GameState -> [Spell] -> [Result]
playout state [] = []
playout state (spell : spells) =
  case next state spell of
    Playing state' -> Playing state' : playout state' spells
    other -> [other]

solve :: Int
solve = sum $ traceShowId $ minimumBy (comparing sum) $ go 0 initialGameState
  where
    go :: Int -> GameState -> [[Spell]]
    --go s | traceShow s False  = undefined
    go n state = do
      spell <- reverse [missileCost, drainCost, shieldCost, poisonCost, rechargeCost]
      spells <-
        case next state spell of
          Win -> return []
          Lose -> []
          Playing state' -> go (n + 1) state'
      return (spell : spells)

data GameState = GameState {bossHp :: Int, playerHp :: Int, armor :: Int, mana :: Int, shieldTimer :: Int, poisonTimer :: Int, rechargeTimer :: Int}
  deriving (Show, Eq, Generic)

data Result = Playing GameState | Win | Lose
  deriving (Show, Eq)

type Spell = Int

initialGameState :: GameState
initialGameState = GameState bossInitialHitPoints playerInitialHitPoints 0 playerInitialMana 0 0 0

next :: GameState -> Spell -> Result
next s spell = flip evalState s $ do
      when hard (#playerHp -= 1)
      playerHp <- use #playerHp
      if playerHp == 0
        then return Lose
        else do
          applyEffects
          canApply <- canApplySpell spell
          if not canApply
            then return Lose
            else do
              applySpell spell
              #mana -= spell
              checkVictoryOrLoss playBoss
  where
    playBoss = do
      --st <- get
      --traceShow st (return ())
      applyEffects
      armor <- use #armor
      #playerHp -= max 1 (bossDamage - armor)
      checkVictoryOrLoss (gets Playing)
    canApplySpell :: Spell -> State GameState Bool
    canApplySpell spell = do
      mana <- use #mana
      shieldTimer <- use #shieldTimer
      poisonTimer <- use #poisonTimer
      rechargeTimer <- use #rechargeTimer
      return $
        (mana >= spell)
          && ( if
                   | spell == shieldCost -> shieldTimer == 0
                   | spell == poisonCost -> poisonTimer == 0
                   | spell == rechargeCost -> rechargeTimer == 0
                   | otherwise -> True
             )
    applySpell :: Spell -> State GameState ()
    applySpell 53 = do
      #bossHp -= 4
    applySpell 73 = do
      #playerHp += 2
      #bossHp -= 2
    applySpell 113 = do
      #armor .= 7
      #shieldTimer .= 6
    applySpell 173 = do
      #poisonTimer .= 6
    applySpell 229 = do
      #rechargeTimer .= 5
    applySpell _ = error "unknown spell"
    applyEffects :: State GameState ()
    applyEffects = do
      poisonTimer <- use #poisonTimer
      when (poisonTimer > 0) (#bossHp -= 3)
      rechargeTimer <- use #rechargeTimer
      when (rechargeTimer > 0) (#mana += 101)
      #shieldTimer %= (\n -> max 0 (n-1))
      #poisonTimer %= (\n -> max 0 (n-1))
      #rechargeTimer %= (\n -> max 0 (n-1))
      shieldTimer <- use #shieldTimer
      when (shieldTimer == 0) (#armor .= 0)
    checkVictoryOrLoss :: State GameState Result -> State GameState Result
    checkVictoryOrLoss cont = do
      bossHp <- use #bossHp
      if bossHp <= 0
        then return Win
        else do
          playerHp <- use #playerHp
          if playerHp <= 0
            then return Lose
            else cont


