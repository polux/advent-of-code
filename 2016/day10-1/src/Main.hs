-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}
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

import Control.Lens (Prism', to, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (biplate, is)
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
import Safe
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (foldl', sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)
import Data.Graph.Wrapper
import Text.Regex.Pcre2
import Data.Data (Data, Typeable)
import Control.Arrow ((&&&))

-- #endregion

data NodeId = Bot Int | Output Int | Source Int
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
data Instr = ValueGoesTo Int NodeId | BotGives Int NodeId NodeId | Noop
  deriving (Eq, Ord, Show, Generic, Data, Typeable)

type Input = [Instr]


main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = map parseLine . T.lines . T.pack
 where
   toInt = read . T.unpack
   toTarget [regex|bot (?<x>\d+)|] = Bot (toInt x)
   toTarget [regex|output (?<x>\d+)|] = Output (toInt x)
   toTarget s = error (T.unpack s)
   parseLine [regex|value (?<x>\d+) goes to (?<y>.+)|] = ValueGoesTo (toInt x) (toTarget y)
   parseLine [regex|bot (?<x>\d+) gives low to (?<y>.+) and high to (?<z>.+)|] = BotGives (toInt x) (toTarget y) (toTarget z)
   parseLine s = error (T.unpack s)

transform :: Input -> Graph NodeId Instr
transform input = fromList (map toNode input ++ outputs)
 where
   toNode instr@(ValueGoesTo val target) = (Source val, instr, [targetToId target])
   toNode instr@(BotGives val target1 target2) = (Bot val, instr, [targetToId target1, targetToId target2])
   targetToId (Bot i) = Bot i
   targetToId (Output i) = Output i
   outputs = input ^.. biplate . (#_Output :: Prism' NodeId Int) . to ((,Noop, []) . Output)

insert m (_, ValueGoesTo val target) = M.insertWith (++) target [val] m
insert m (id, BotGives val t1 t2) = M.unionWith (++) m (M.fromList [(t1, [minimum values]), (t2, [maximum values])])
 where values = m M.! id
insert m (_, Noop) = m

solve input = graph & topologicalSort & map (id &&& vertex graph) & foldl' insert mempty & M.filter isTarget & M.toList & head
  where
    graph = transform input
    isTarget is = minimum is == 17 && maximum is == 61
