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

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?))
import Control.Lens.Extras (is)
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
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Safe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Input = Seq Int

type Output = Seq Int

input :: String
--input = "389125467"
input = "916438275"

main :: IO ()
main = print (solve (parse input))

parse :: String -> Input
parse str = Seq.fromList $ map (read . (: [])) str

solve :: Input -> Output
solve input = iterate step input !! 100
  where
    closest i seq =
      maximum $ case Seq.filter (< i) seq of
        Empty -> seq
        smaller -> smaller
    indexOf i = Seq.findIndexL (== i)
    insertSliceAfter i seq slice = Seq.take (i + 1) seq <> slice <> Seq.drop (i + 1) seq
    step :: Seq Int -> Seq Int
    step Empty = error "empty seq"
    step seq@(head :<| tail) =
      let tailMissingThreeCups = Seq.drop 3 tail
          slice = Seq.take 3 tail
          destinationCup = closest head tailMissingThreeCups
          destinationCupIndex = fromJust $ indexOf destinationCup tailMissingThreeCups
          newTail = insertSliceAfter destinationCupIndex tailMissingThreeCups slice
       in newTail :|> head
