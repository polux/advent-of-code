-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (use, (<+=), (<%=), (%=), each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?))
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
import Linear ((*^), V2 (..))
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
import Data.List.Split (splitOn)
import Control.Monad.Writer
import Control.Monad.State
import Data.Foldable (traverse_)

-- #endregion


data Turn = L | R
type Input = [(Turn, Int)]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = map parseEntry (splitOn ", " str)
  where
    parseEntry ('R':num) = (R, read num)
    parseEntry ('L':num) = (L, read num)
    parseEntry other = error ("unexpected " <> other)

rotate R (V2 x y) = V2 y (-x)
rotate L (V2 x y) = V2 (-y) x

data St = St { dir :: V2 Int, pos :: V2 Int }
  deriving (Generic)

solve :: Input -> Output
solve input =
  let V2 x y = findVisitedTwice (S.singleton (V2  0 0)) visited
  in abs x + abs y
 where
  findVisitedTwice :: Set (V2 Int) -> [V2 Int] -> V2 Int
  findVisitedTwice _ [] = error "not found"
  findVisitedTwice seen (pos:poss)
    | pos `S.member` seen = pos
    | otherwise = findVisitedTwice (S.insert pos seen) poss
  visited :: [V2 Int]
  visited =
    traverse_ next input
    & execWriterT
    & flip evalState (St (V2 0 1) (V2 0 0))
  next :: (Turn, Int) -> WriterT [V2 Int] (State St) ()
  next (turn, n) = do
    #dir %= rotate turn
    move n
  move :: Int -> WriterT [V2 Int] (State St) ()
  move 0 = return ()
  move n = do
    dir <- use #dir
    newPos <- #pos <+= dir
    tell [newPos]
    move (n-1)