-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import qualified Data.Set as S
import Data.Ratio
import Data.Function
import Data.List
import Data.Semigroup
import Debug.Trace (traceShowId, traceShow)

type Point = (Int, Int)
type Vec = Point

minus :: Point -> Point -> Vec
(x1, y1) `minus` (x2, y2) = (x1 - x2, y1 - y2)

data Angle = Angle { slope :: Maybe (Ratio Int), xSign :: Bool, ySign :: Bool }
  deriving (Eq, Ord, Show)

angle :: Vec -> Angle
angle (dx, dy) = Angle (if dy == 0 then Nothing else Just (abs dx % abs dy)) (dx > 0) (dy > 0)

coordinates :: [[Point]]
coordinates = [[(i,j) | i <- [0..]] | j <- [0..]]

annotate :: [[a]] -> [[(a, Point)]]
annotate xss = zipWith zip xss coordinates

numAnglesFrom :: [Point] -> Point -> Int
numAnglesFrom ps p = map angleTo (ps \\ [p]) & S.fromList & S.size
 where angleTo p' = angle (p' `minus` p)

main :: IO ()
main = do
  str <- getContents
  let asteroids = str
        & lines
        & annotate
        & concat
        & filter ((== '#') . fst)
        & map snd
  asteroids
    & foldMap (Max . numAnglesFrom asteroids)
    & getMax
    & print