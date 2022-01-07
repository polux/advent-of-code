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

import Control.Lens (view, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1)
import Control.Lens.Extras (is)
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
import Data.List (subsequences, elemIndex, sortOn, (\\))
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

type Input = (Int, Int, Int)

type Output = Int

main :: IO ()
main = print (solve (104, 8, 1))

weapons =
  [ (8, 4, 0),
    (10, 5, 0),
    (25, 6, 0),
    (40, 7, 0),
    (74, 8, 0)
  ]

armors =
  [ (13, 0, 1),
    (31, 0, 2),
    (53, 0, 3),
    (75, 0, 4),
    (102, 0, 5)
  ]

rings =
  [ (25, 1, 0),
    (50, 2, 0),
    (100, 3, 0),
    (20, 0, 1),
    (40, 0, 2),
    (80, 0, 3)
  ]

orders = do
  weapon <- weapons
  armor <- (0, 0, 0) : armors
  ring <- [] : map single rings ++ pickTwo rings
  return $ weapon `plus` armor `plus` foldr plus (0,0,0) ring
  where
    single x = [x]
    pickTwo xs = filter ((2==).length) (subsequences xs)
    plus (c1, d1, a1) (c2, d2, a2) = (c1 + c2, d1 + d2, a1 + a2)

solve :: Input -> Output
solve (bossHit, bossDamage, bossArmor) = orders & filter playerWins & map (view _1) & minimum
  where
    playerWins (_, damage, armor) = bossHit - (numRoundsBeforePlayerLoss+1) * bossLoss <= 0
      where
        playerLoss = max 1 (bossDamage - armor)
        bossLoss = max 1 (damage - bossArmor)
        numRoundsBeforePlayerLoss = 99 `div` playerLoss