-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
-- #endregion

module Main where

-- #region imports

import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
import Linear (V2 (..), _x, _y)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe hiding (at)
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes, fromMaybe)
import Data.List (sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)

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

type Rules = Map [Bool] Bool
type Input = (Set Int, Rules)


main :: IO ()
main = getContents >>= putStrLn . solve . parse

parse :: String -> Input
parse str = (S.fromList initialStateIndices, M.fromList (map parseRule rules))
 where
   (header:_:rules) = lines str
   [_, _, initialState] = splitOn " " header
   initialStateIndices = zip [0..] initialState & filter ((=='#') . snd) & map fst
   parseRule rule =
     let [lhs, [rhs]] = splitOn " => " rule
     in (map (=='#') lhs, rhs == '#')

step :: Rules -> Set Int -> Set Int
step rules pots = S.fromList (filter isOn [lb-5..rb+5])
 where
   lb = S.findMin pots
   rb = S.findMax pots
   isOn i = fromMaybe False (rules M.!? [j `S.member` pots | j <- [i-2..i+2]])

solve (pots, rules) = iterate (step rules) pots & zip [0..] & map show & unlines

{-

after a while the same pattern keeps shifting on step to the right:

(120,fromList [49,65,91,96,101,109,117,122,128,133,141,149,157,163,168,173,181,186,194,202,210,216])
(121,fromList [50,66,92,97,102,110,118,123,129,134,142,150,158,164,169,174,182,187,195,203,211,217])
(122,fromList [51,67,93,98,103,111,119,124,130,135,143,151,159,165,170,175,183,188,196,204,212,218])
(123,fromList [52,68,94,99,104,112,120,125,131,136,144,152,160,166,171,176,184,189,197,205,213,219])
(124,fromList [53,69,95,100,105,113,121,126,132,137,145,153,161,167,172,177,185,190,198,206,214,220])
(125,fromList [54,70,96,101,106,114,122,127,133,138,146,154,162,168,173,178,186,191,199,207,215,221])
(126,fromList [55,71,97,102,107,115,123,128,134,139,147,155,163,169,174,179,187,192,200,208,216,222])

-}

step120 = [49,65,91,96,101,109,117,122,128,133,141,149,157,163,168,173,181,186,194,202,210,216]
solution = sum (map (+(50000000000-120)) step120)