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
import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), view)
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
import Linear (V2 (..), V3 (..), _x, _y, _z)
import Safe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import Control.Arrow ((***))

-- #endregion

type Input = [V3 Int]

example =
  [ V3 (-8) (-10) 0,
    V3 5 5 10,
    V3 2 (-7) 3,
    V3 9 (-8) (-3)
  ]

input =
  [ V3 (-14) (-4) (-11),
    V3 (-9) 6 (-7),
    V3 4 1 4,
    V3 2 (-14) (-9)
  ]

select :: [a] -> [(a, [a])]
select xs = go [] xs
 where
   go _ [] = []
   go prefix (x:xs) = (x, reverse prefix ++ xs) : go (x:prefix) xs

main :: IO ()
main = do
  print (solve example)
  print (solve input)

simulate :: Input -> [[(V3 Int, V3 Int)]]
simulate planets = iterate step initialState
 where
   initialState = zip planets (repeat (V3 0 0 0))
   step = map stepPlanet . select
   stepPlanet ((position@(V3 x y z), velocity), others) = (position+velocity', velocity')
     where
       velocity' :: V3 Int
       velocity' = sum (map deltaV others) + velocity
       deltaV (V3 x' y' z', _) = V3 (sign (x'-x)) (sign (y'-y)) (sign (z'-z))
       sign x | x==0 = 0
              | x>0 = 1
              | otherwise = -1
   potentialEnergy (V3 x y z, _) = abs x + abs y + abs z
   kineticEnergy (_, V3 dx dy dz) = abs dx + abs dy + abs dz
   totalEnergy planet = potentialEnergy planet * kineticEnergy planet

findPeriod :: Ord a => [a] -> Int
findPeriod = go 0 mempty
  where
    go i seen (x : xs)
      | x `S.member` seen = i
      | otherwise = go (i + 1) (S.insert x seen) xs


solve planets = foldr1 lcm [findPeriodFor _x, findPeriodFor _y, findPeriodFor _z]
 where
   simulation = simulate planets
   findPeriodFor lens = findPeriod $ map (map (view lens *** view lens)) simulation