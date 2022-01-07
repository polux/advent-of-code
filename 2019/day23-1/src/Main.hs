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

-- #region imports
import Control.Lens (at, ix, each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4, (<>~))
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
import Data.List (sortOn, elemIndex, foldl')
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)
import IntCode
import Control.Monad.Writer

-- #endregion

type Input = State

type Output = Input

type Machines = Map Integer Machine
newtype Messages = Messages (Map Integer (Seq Integer))
 deriving (Generic)

data State = State { machines :: Map Integer Machine, queues :: Messages }

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = State machines mempty
  where
    (ReadInput k) = parseToMachine str
    machines = M.fromList [(i, k i) | i <- [0..49]]

instance Semigroup Messages where
  (Messages m1) <> (Messages m2) = Messages (M.unionWith (<>) m1 m2)

instance Monoid Messages where
  mempty = Messages mempty

stepMachine :: Machine -> Seq Integer -> Writer Messages Machine
stepMachine (ReadInput k) (i :<| is) = stepMachine (k i) is
stepMachine (ReadInput k) Empty = return (k (-1))
stepMachine (WriteOutput i (WriteOutput x (WriteOutput y k))) is = do
  tell (Messages (M.singleton i (Seq.fromList [x,y])))
  stepMachine k is
stepMachine m is = error (show (m, is))

stepMachines :: State -> Writer Messages Machines
stepMachines (State machines (Messages inputs)) = M.fromList <$> mapM step (M.toList machines)
 where
   step (i, machine) = do
     machine' <- stepMachine machine (fromMaybe mempty (inputs M.!? i))
     return (i, machine')

--solve :: Input -> Output
solve input = go input
 where
   go input =
     let (machines', messages) = runWriter (stepMachines input)
     in case messages ^? #_Messages . ix 255 of
       Just inputs -> inputs
       Nothing -> go (State machines' messages)