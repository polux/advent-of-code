-- Copyright 2022 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.FingerTree (ViewL ((:<)), ViewR (..), (<|), (><), (|>))
import qualified Data.FingerTree as F
import Data.Foldable (toList)
import Data.Semigroup (Max (..))
import Data.Function ((&))
import Util (singleton)

main :: IO ()
main = readFile "input" >>= print . solve . parse

data Block
  = Free Int -- length
  | File Int Int -- length, indentifier
  deriving (Show)
type Input = [Block]

parse :: String -> Input
parse = go 0 . map (read . singleton) . head . lines
 where
  go _ [] = []
  go id [n] = [File n id]
  go id (n : m : ns) = File n id : Free m : go (id + 1) ns

newtype BlockMeasure = BlockMeasure (Max Int)
  deriving newtype (Semigroup, Monoid)

instance F.Measured BlockMeasure Block where
  measure (Free l) = BlockMeasure (Max l)
  measure (File _ _) = BlockMeasure (Max 0)

maxFreeLen (BlockMeasure (Max l)) = l

solve :: [Block] -> Int
solve input =
  input
    & defrag
    & expand
    & zipWith (*) [0 ..]
    & sum
 where
  defrag s = toList (go (F.fromList s))
   where
    go ft =
      case F.viewr ft of
        EmptyR -> ft
        ft :> Free l -> go ft |> Free l
        ft :> File l n ->
          let (prefix, suffix) = F.split (\m -> maxFreeLen m >= l) ft
           in case F.viewl suffix of
                Free l' :< suffix' -> go (prefix >< File l n <| Free (l' - l) <| suffix') |> Free l
                _ -> go ft |> File l n

  expand [] = []
  expand (Free l : ns) = replicate l 0 <> expand ns
  expand (File l n : ns) = replicate l n <> expand ns