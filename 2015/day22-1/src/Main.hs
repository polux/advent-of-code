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
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
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

solve =
  orderedSums [missileCost, drainCost, shieldCost, poisonCost, rechargeCost]
    & filter (wins bossInitialHitPoints bossDamage playerInitialHitPoints playerInitialMana)
    & head

sums :: [Int] -> [Set [Int]]
sums choices = map go' [0 ..]
  where
    go' = memo go
    go n
      | n == 0 = S.singleton []
      | n < 0 = mempty
      | otherwise =
        S.fromList choices `bind` \k ->
          go' (n - k) `bind` \ks ->
            S.singleton (k : ks)
    bind set f = S.unions (S.map f set)

orderedSums :: [Int] -> [[Int]]
orderedSums choices = concatMap toList (sums choices)

bossInitialHitPoints = 51

bossDamage = 9

playerInitialHitPoints = 50

playerInitialMana = 500

missileCost = 53

drainCost = 73

shieldCost = 113

poisonCost = 173

rechargeCost = 229

wins :: Int -> Int -> Int -> Int -> [Int] -> Bool
wins bossInitialHitPoints bossDamage playerInitialHitPoints playerInitialMana = go bossInitialHitPoints playerInitialHitPoints 0 playerInitialMana 0 0 0
  where
    applyShield 113 0 0 = (7, 6)
    applyShield _ 0 armor = (0, 0)
    applyShield _ 1 armor = (0, 0)
    applyShield _ n armor = (7, n -1)
    applyPoison 173 0 hp = (hp, 6)
    applyPoison _ 0 hp = (hp, 0)
    applyPoison _ n hp = (hp -3, n -1)
    applyRecharge 229 n mana = (mana, 5)
    applyRecharge _ 0 mana = (mana, 0)
    applyRecharge _ n mana = (mana + 101, n -1)
--    go2 bossHp playerHp armor mana shield poison recharge spells | traceShow ( "go2", "bossHp=" <> show bossHp, "playerHp=" <> show playerHp, "armor=" <> show armor, "mana=" <> show mana, "shield=" <> show shield, "poison=" <> show poison, "recharge=" <> show recharge, "spells=" <> show spells) False = undefined
    go2 bossHp playerHp armor mana shield poison recharge spells =
      let (newArmor, newShield) = applyShield 0 shield armor
          (newbossHp, newPoison) = applyPoison 0 poison bossHp
          (newMana, newRecharge) = applyRecharge 0 recharge mana
       in if newbossHp <= 0
            then True
            else
              go
                newbossHp
                (playerHp - max 1 (bossDamage - armor))
                newArmor
                newMana
                newShield
                newPoison
                newRecharge
                spells
--    go bossHp playerHp armor mana shield poison recharge spells | traceShow ( "go", "bossHp=" <> show bossHp, "playerHp=" <> show playerHp, "armor=" <> show armor, "mana=" <> show mana, "shield=" <> show shield, "poison=" <> show poison, "recharge=" <> show recharge, "spells=" <> show spells) False = undefined
    go _ _ _ _ _ _ _ [] = False
    go bossHp playerHp armor mana shield poison recharge (spell : spells)
      | playerHp <= 0 = False
      | spell == shieldCost && shield > 1 = False -- invalid move
      | spell == poisonCost && poison > 1 = False -- invalid move
      | spell == rechargeCost && recharge > 1 = False -- invalid move
      | otherwise =
        let (newArmor, newShield) = applyShield spell shield armor
            (newbossHp, newPoison) = applyPoison spell poison bossHp
            (newMana, newRecharge) = applyRecharge spell recharge mana
         in if newMana < spell
              then False -- invalid move or no more mana
              else
                go2
                  (newbossHp - if spell == missileCost then 4 else if spell == drainCost then 2 else 0)
                  (playerHp + if spell == drainCost then 2 else 0)
                  newArmor
                  (newMana - spell)
                  newShield
                  newPoison
                  newRecharge
                  spells