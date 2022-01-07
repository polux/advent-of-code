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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- #endregion

module Main where

-- #region imports

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, join, unless, when)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn, tails)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, maybeToList, isJust)
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
import qualified Data.Vector.Unboxed.Mutable as MUV
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..), V3 (V3), _x, _y)
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

type Input = [Beacons]

--type Output = Input

main :: IO ()
main = readFile "input" >>= print . solve . parse

parse :: String -> Input
parse = map parseScanner . splitOn "\n\n"
 where
  parseScanner = S.fromList . map parseBeacon . tail . lines
  parseBeacon (splitOn "," -> [x, y, z]) = V3 (read x) (read y) (read z)

roll (V3 x y z) = V3 x z (- y)
turn (V3 x y z) = V3 (- y) x z

cubeSymmetries = [rot . face | face <- faces, rot <- turns]
 where
  faces =
    [ id
    , roll
    , roll . roll
    , roll . roll . roll
    , roll . turn
    , roll . turn . turn . turn
    ]
  turns =
    [ id
    , turn
    , turn . turn
    , turn . turn . turn
    ]

type Beacons = Set (V3 Int)
type Signature = Set (V3 Int)
type Signed = (Beacons, Signature)
type SignedRotations = [Signed]

rotations :: Beacons -> [Beacons]
rotations bs = map (`S.map` bs) cubeSymmetries

signature :: Beacons -> Signature
signature bs = S.fromList [a - b | (a:as) <- tails (S.toList bs), b <- as]

sign :: Beacons -> Signed
sign bs = (bs, signature bs)

signedRotations :: Beacons -> SignedRotations
signedRotations bs = map sign (rotations bs)

translate :: V3 Int -> Beacons -> Beacons
translate d = S.map (+ d)

tryMatch :: Beacons -> Beacons -> Maybe Beacons
tryMatch bs cs =
  headMay
    [ cs'
    | b <- S.toList bs
    , c <- S.toList cs
    , let cs' = translate (b - c) cs
    , length (S.intersection bs cs') >= 12
    ]

solve bss = go (M.singleton 0 (sign (head bss))) [0]
 where
  input = V.fromList (map signedRotations bss)
  len = V.length input
  go done is | traceShow (M.keys done, is) False = undefined
  go done [] = length (S.unions (map fst (M.elems done)))
  go done (i : is) =
    let matches =
          [ (j, (match, sigj))
          | j <- [0 .. len - 1]
          , j `M.notMember` done
          , let (ri, sigi) = done M.! i
          , (rj, sigj) <- input V.! j
          , length (sigi `S.intersection` sigj) >= (12*11) `div` 2
          , match <- maybeToList (tryMatch ri rj)
          ]
     in go (done <> M.fromList matches) (map fst matches ++ is)