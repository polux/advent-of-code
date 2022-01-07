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

import Control.Arrow ((>>>),(***), Arrow (first, second))
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
import Text.Pretty.Simple (pPrint, pShow, StringOutputStyle (Literal))
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe hiding (at)
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortOn, elemIndex, intercalate)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_, unless, when, replicateM)
import Control.Monad.State.Lazy

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

--type Output = Input

data Payload = PLiteral  Int | SubPackets [Packet]
  deriving (Eq, Ord, Show, Generic)

data Packet =
  Packet {
    packetVersion :: Int,
    packetType :: Int,
    packetPayload :: Payload
  }
  deriving (Eq, Ord, Show, Generic)

hexToBin :: String -> [Int]
hexToBin = concatMap translate
  where
    translate d = fromJust $ lookup d (zip "0123456789ABCDEF" (replicateM 4 [0,1]))

main :: IO ()
main = readFile "input" >>= pPrint . solve . parse

parse :: String -> Input
parse = hexToBin . head . lines

type BParser a = State (Int, [Int]) a

takeN :: Int -> BParser [Int]
takeN n = do
  (pos, xs) <- get
  put (pos + n, drop n xs)
  return (take n xs)

word :: Int -> BParser Int
word n = fromBinary <$> takeN n

literal :: BParser Int
literal = fromBinary <$> go
  where
    go :: BParser [Int]
    go = do
      byte <- takeN 5
      if head byte == 0
        then pure (tail byte)
        else (tail byte ++) <$> go

getPos :: BParser Int
getPos = fst <$> get

parsePacket :: BParser Packet
parsePacket = do
  version <- word 3
  pType <- word 3
  payload <-
   if pType == 4
    then PLiteral <$> literal
    else SubPackets <$> do
      mode <- word 1
      if mode == 1
        then do
          numPackets <- word 11
          replicateM numPackets parsePacket
        else do
          totalLength <- word 15
          pos <- getPos
          parsePacketsUntil (pos+totalLength)
  return (Packet version pType payload)

parsePacketsUntil endPos = do
  pos <- getPos
  if pos == endPos
    then pure []
    else do
      (:) <$> parsePacket <*> parsePacketsUntil endPos

eval :: Packet -> Int
eval (Packet _ 0 (SubPackets ps)) = sum (map eval ps)
eval (Packet _ 1 (SubPackets ps)) = product (map eval ps)
eval (Packet _ 2 (SubPackets ps)) = minimum (map eval ps)
eval (Packet _ 3 (SubPackets ps)) = maximum (map eval ps)
eval (Packet _ 4 (PLiteral i)) = i
eval (Packet _ 5 (SubPackets [p1, p2])) = if eval p1 > eval p2 then 1 else 0
eval (Packet _ 6 (SubPackets [p1, p2])) = if eval p1 < eval p2 then 1 else 0
eval (Packet _ 7 (SubPackets [p1, p2])) = if eval p1 == eval p2 then 1 else 0
eval other = error "unknown packet type"

--solve :: Input -> Output
solve input = evalState parsePacket (0, input) & eval