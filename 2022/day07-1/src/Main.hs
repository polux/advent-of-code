-- Copyright 2022 Google LLC.
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

import Control.Arrow (Arrow (first, second), (***), (>>>))
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_, unless, when)
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter, runWriter)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (Foldable (toList))
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
import Data.Tree (Tree (Node, rootLabel))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..), _x, _y)
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

data FS = Dir (Map String FS) | File Int
  deriving (Show)

root :: FS
root = Dir (M.singleton "/" (Dir mempty))

type Zipper = ([(String, Map String FS)], FS)

toZipper :: FS -> Zipper
toZipper fs = ([], fs)

down :: String -> Zipper -> Zipper
down d (stack, Dir m) = ((d, m) : stack, m M.! d)
down _ _ = error "cannot down"

up :: Zipper -> Zipper
up ((d, m) : stack, fs) = (stack, Dir (M.insert d fs m))
up _ = error "cannot up"

add :: String -> FS -> Zipper -> Zipper
add s f (stack, Dir m) = (stack, Dir (M.insert s f m))
add _ _ _ = error "cannot add"

zipUp :: Zipper -> Zipper
zipUp z@([], _) = z
zipUp z = zipUp (up z)

data Line = CdCmd String | LsCmd | FileOut Int String | DirOut String
  deriving (Show)

type Input = [Line]

type Output = Int

main :: IO ()
main = readFile "input" >>= pPrint . solve . parse

parse :: String -> Input
parse = map (parseLine . words) . lines
 where
  parseLine ["$", "cd", x] = CdCmd x
  parseLine ["$", "ls"] = LsCmd
  parseLine ["dir", x] = DirOut x
  parseLine [n, x] = FileOut (read n) x
  parseLine _ = error "parse error"

step :: Zipper -> Line -> Zipper
step z (CdCmd "..") = up z
step z (CdCmd d) = down d z
step z LsCmd = z
step z (FileOut n s) = add s (File n) z
step z (DirOut s) = add s (Dir mempty) z

build :: Input -> FS
build = snd . down "/" . zipUp . foldl' step (toZipper root)

sizes :: FS -> Writer [Int] Int
sizes (File n) = return n
sizes (Dir m) = do
  s <- sum <$> traverse sizes (M.elems m)
  tell [s]
  return s

solve :: Input -> Output
solve = sum . filter (<= 100_000) . execWriter . sizes . build
