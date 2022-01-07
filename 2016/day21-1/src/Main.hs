-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?))
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (scanl', foldl', sortOn, elemIndex)
import Text.Regex.Pcre2

-- #endregion

data Instr = SwapPos Int Int | SwapLetter Char Char | RotateRight Int | RotateLeft Int | RotateBasedOn Char | Reverse Int Int | Move Int Int
  deriving (Show)
type Input = [Instr]


initStr :: Seq Char
initStr = Seq.fromList "abcdefgh"
--initStr = Seq.fromList "abcde"

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . T.lines . T.pack
 where
   toInt = read . T.unpack
   toChar = head . T.unpack
   parseLine [regex|swap position (?<x>\d+) with position (?<y>\d+)|] = SwapPos (toInt x) (toInt y)
   parseLine [regex|swap letter (?<x>.) with letter (?<y>.)|] = SwapLetter (toChar x) (toChar y)
   parseLine [regex|rotate right (?<x>\d+) steps?|] = RotateRight (toInt x)
   parseLine [regex|rotate left (?<x>\d+) steps?|] = RotateLeft (toInt x)
   parseLine [regex|rotate based on position of letter (?<x>.)|] = RotateBasedOn (toChar x)
   parseLine [regex|reverse positions (?<x>\d+) through (?<y>\d+)|] = Reverse (toInt x) (toInt y)
   parseLine [regex|move position (?<x>\d+) to position (?<y>\d+)|] = Move (toInt x) (toInt y)
   parseLine other = error ("parse error " <> T.unpack other)

solve = foldl' step initStr
 where
   step :: Seq Char -> Instr -> Seq Char
   step str (SwapPos i j) =
     str & Seq.update i (str `Seq.index` j)
         & Seq.update j (str `Seq.index` i)
   step str (SwapLetter a b) =
     let ai = fromJust $ Seq.elemIndexL a str
         bi = fromJust $ Seq.elemIndexL b str
     in str & Seq.update ai b
            & Seq.update bi a
   step str (RotateRight i) =
     let prefixLen = length str - (i `mod` length str)
     in Seq.drop prefixLen str <> Seq.take prefixLen str
   step str (RotateLeft i) =
     let i' = i `mod` length str
     in Seq.drop i' str <> Seq.take i' str
   step str (RotateBasedOn a) =
     let ai = fromJust $ Seq.elemIndexL a str
     in step str (RotateRight (1 + ai + if ai >= 4 then 1 else 0))
   step str (Reverse i j) =
     let prefix = Seq.take i str
         middle = Seq.take (j-i+1) (Seq.drop i str)
         suffix = Seq.drop (j+1) str
     in prefix <> Seq.reverse middle <> suffix
   step str (Move i j) =
     let ci = str `Seq.index` i
     in Seq.insertAt j ci (Seq.deleteAt i str)
