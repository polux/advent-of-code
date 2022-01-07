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
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
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

-- #endregion

type Input = [Int]
type Pos = Int


input = [13,0,10,12,1,5,8]

main :: IO ()
main = print (solve input)

next :: IM.IntMap Int -> Pos -> Int -> (IM.IntMap Int, Int)
next positions currentPos lastSpoken =
  (IM.insert lastSpoken currentPos positions,
   case IM.lookup lastSpoken positions of
       Nothing -> 0
       Just i -> currentPos - i)

solve input = sequence !! (30000000-1)
  where
    initMap = IM.fromList (zip (init input) [0..])
    initPos = length input - 1
    initLastSpoken = last input
    sequence = input ++ go initMap initPos initLastSpoken
    go a b c | b `mod` 1000000 == 0 && traceShow (b, length a) False = undefined
    go positions currentPos lastSpoken =
      let (positions', lastSpoken') = next positions currentPos lastSpoken
      in lastSpoken' : go positions' (currentPos + 1) lastSpoken'