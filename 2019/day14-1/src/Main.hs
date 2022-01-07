-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE TypeApplications #-}
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
{-# LANGUAGE MultiParamTypeClasses #-}
-- #endregion

module Main where

-- #region imports
import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4, over)
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
import Data.Ratio
import Control.Arrow (first, Arrow (second))
import Control.Monad.State
import Numeric.Optimization.MIP.Base
import Numeric.Optimization.MIP.Solver.Base

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

type Input = Map String (Int, [(Int, String)])

main :: IO ()
main = getContents >>= print . Main.solve . parse

parse :: String -> Input
parse = M.fromList . map parseLine . lines
 where
   parseLine str =
     let [lhs, rhs] = splitOn " => " str
         (quantity, product) = parseAtom rhs
     in (product, (quantity, map parseAtom (splitOn ", " lhs)))
   parseAtom str =
     let [i, name] = words str
     in (read i, name)


solve input = execState (go 1 "FUEL") mempty M.! "ORE"
 where
   go :: Int -> String -> State (Map String Int) ()
   go i "ORE" = modify (M.insertWith (+) "ORE" i)
   go i n = do
     let (units, lhs) = input M.! n
     surplus <- gets (fromMaybe 0 . M.lookup n)
     if surplus >= i
       then do
         modify (M.insert n (surplus-i))
         return ()
       else do
         modify (M.insert n 0)
         let needed = i - surplus
         let multiplier = needed `div` units + if needed `mod` units == 0 then 0 else 1
         lhs `forM_` \(j, m) -> go (j*multiplier) m
         modify (M.insertWith (+) n (multiplier*units-needed))
