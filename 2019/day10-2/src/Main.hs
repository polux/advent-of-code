-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ord
import Data.Ratio
import Data.Function
import Data.List ((\\), maximumBy, sortOn, transpose, (!!))
import Data.Semigroup
import Debug.Trace (traceShowId, traceShow)

type Point = (Int, Int)
type Vec = Point

minus :: Point -> Point -> Vec
(x1, y1) `minus` (x2, y2) = (x1 - x2, y1 - y2)

data Angle =
  Angle
    { slope :: Maybe (Ratio Int) -- Nothing means infinity
    , xSign :: Bool
    , ySign :: Bool
    }
  deriving (Eq, Show)

quadrant True False = 1
quadrant True True = 2
quadrant False True = 3
quadrant False False = 4

leq Nothing Nothing = True
leq Nothing (Just x) = False
leq (Just x) Nothing = True
leq (Just x) (Just y) = x <= y

leqInQuadrant 1 r1 r2 = r2 `leq` r1
leqInQuadrant 2 r1 r2 = r1 `leq` r2
leqInQuadrant 3 r1 r2 = r2 `leq` r1
leqInQuadrant 4 r1 r2 = r1 `leq` r2

instance Ord Angle where
  Angle s1 x1 y1 <= Angle s2 x2 y2
    | q1 == q2 = leqInQuadrant q1 s1 s2
    | otherwise = q1 < q2
   where
    q1 = quadrant x1 y1
    q2 = quadrant x2 y2

angle :: Vec -> Angle
angle (dx, dy) =
  Angle (if dx == 0 then Nothing else Just (abs dy % abs dx))
        (dx >= 0)
        (dy >= 0)

angleFrom :: Point -> Point -> Angle
angleFrom p q = angle (q `minus` p)

coordinates :: [[Point]]
coordinates = [[(i,j) | i <- [0..]] | j <- [0..]]

annotate :: [[a]] -> [[(a, Point)]]
annotate xss = zipWith zip xss coordinates

numAsteroidsSeenFrom :: [Point] -> Point -> Int
numAsteroidsSeenFrom ps p =
  map (angleFrom p) (ps \\ [p])
    & S.fromList
    & S.size

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x2-x1) + abs (y2-y1)

readAsteroids :: String -> [Point]
readAsteroids str =
  str
    & lines
    & annotate
    & concat
    & filter ((== '#') . fst)
    & map snd

groupBy :: Ord k => (a -> k) -> [a] -> M.Map k [a]
groupBy f xs = M.fromListWith (<>) [(f x, [x]) | x <- xs]

main :: IO ()
main = do
  str <- getContents
  let asteroids = readAsteroids str
  let best = maximumBy (comparing (numAsteroidsSeenFrom asteroids)) asteroids
  let (x, y) = (asteroids \\ [best])
                 & groupBy (angleFrom best)
                 & fmap (sortOn (distance best))
                 & M.elems
                 & transpose
                 & concat
                 & (!! 199)
  print (x*100+y)
