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

import Control.Lens (view, at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.Maybe (fromJust, catMaybes)
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

input = 359

(numPlayers, maxMarble) = (441, 71032)
--(numPlayers, maxMarble) = (9, 25)

main :: IO ()
main = solve >>= print

insertAfterNext :: MUV.IOVector Int -> MUV.IOVector Int -> Int -> Int -> IO ()
insertAfterNext next prev cur val = do
     curNext <- MUV.read next cur
     curNextNext <- MUV.read next curNext
     MUV.write next curNext val
     MUV.write next val curNextNext
     MUV.write prev val curNext
     MUV.write prev curNextNext val

removeSevenBefore :: MUV.IOVector Int -> MUV.IOVector Int -> Int -> IO (Int, Int)
removeSevenBefore next prev cur = go cur 7
  where
    go cur 0 = do
      curNext <- MUV.read next cur
      curPrev <- MUV.read prev cur
      MUV.write next curPrev curNext
      MUV.write prev curNext curPrev
      return (cur, curNext)
    go cur i = do
      curPrev <- MUV.read prev cur
      go curPrev (i-1)

toList ::  MUV.IOVector Int -> IO [Int]
toList vec = go 0
 where
   go n | n == MUV.length vec = return []
        | otherwise = do
                val <- MUV.read vec n
                tail <- go (n+1)
                return (val:tail)

solve :: IO Int
solve = do
  next <- MUV.replicate (maxMarble+1) (0::Int)
  prev <- MUV.replicate (maxMarble+1) (0::Int)
  scores <- MUV.replicate numPlayers (0::Int)
  go next prev scores 0 1
  maximum <$> toList scores
 where
    go _ _ _ _ i | i > maxMarble = return ()
    go next prev scores cur i
      | i `mod` 23 == 0 = do
        let player = i `mod` numPlayers
        (valueSevenBefore, newCur) <- removeSevenBefore next prev cur
        playerScore <- MUV.read scores player
        MUV.write scores player (playerScore + i + valueSevenBefore)
        go next prev scores newCur (i+1)
      | otherwise = do
         insertAfterNext next prev cur i
         go next prev scores i (i+1)
