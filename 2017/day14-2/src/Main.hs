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

-- #endregion

module Main where

-- #region imports

import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, foldl', sortOn)
import Data.List.Split (chunksOf, splitOn)
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
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word8)
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..), _x, _y)
import Numeric (showHex, readHex, showIntAtBase)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Printf (printf)
import Text.Regex.PCRE ((=~))
import Util
import Control.Monad.State.Strict (execState)
import Control.Monad.State (gets)
import Control.Monad (when)
import Control.Monad (unless)
import Control.Monad.State (modify)

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

type Input = [Int]

input = "ffayrhll"

main :: IO ()
main = print (solve input)

parse :: String -> Input
parse = map ord . head . lines

rotateTo :: Int -> Seq a -> Seq a
rotateTo i seq = Seq.drop j seq <> Seq.take j seq
  where
    j = i `mod` length seq

sparseHash :: Input -> [Int]
sparseHash input =
  foldl' step (0, Seq.fromList [0 .. 255]) input'
    & snd
    & rotateTo (- totalRotated)
    & toList
  where
    input' = input & (++ [17, 31, 73, 47, 23]) & replicate 64 & concat
    totalRotated = sum (zipWith (+) input' [0 ..])
    step :: (Int, Seq Int) -> Int -> (Int, Seq Int)
    step (skipSize, seq) len =
      let (prefix, suffix) = Seq.splitAt len seq
       in (skipSize + 1, rotateTo (len + skipSize) (Seq.reverse prefix <> suffix))

knotHash :: Input -> String
knotHash input =
  sparseHash input
    & chunksOf 16
    & concatMap (printf "%02x" . foldr1 xor)

hexToBinary :: Char -> String
hexToBinary c = printf "%04b" (fst $ head $ readHex [c] :: Int)

countComponents :: [String] -> Int
countComponents input = count mempty
  where
    arr :: UA.Array (V2 Int) Char
    arr = arrayFromList2D input
    dims = arraySize arr
    isOne vertex = arr UA.! vertex == '1'
    vertices = S.fromList (UA.indices arr & filter isOne)
    count seen =
      let unseen = vertices `S.difference` seen
      in if S.null unseen
        then 0
        else 1 + count (execState (dfs (S.elemAt 0 unseen)) seen)
    dfs vertex = do
      seen <- gets (S.member vertex)
      unless seen $ do
        modify (S.insert vertex)
        mapM_ dfs (filter isOne (neighbors2D dims vertex))

solve :: [Char] -> Int
solve input =
  [0..127]
  & map (concatMap hexToBinary . knotHash . parse . rowKey)
  & countComponents
 where
  rowKey n = input ++ "-" ++ show n