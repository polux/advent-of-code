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

-- #region imports
import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (<>~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import Control.Monad.Writer
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, foldl', sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe)
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
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import IntCode
import Linear (V2 (..), _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util

-- #endregion

type Input = State

type Output = Input

type Machines = Map Integer Machine

newtype Messages = Messages (Map Integer (Seq Integer))
  deriving (Generic)

data State = State {machines :: Map Integer Machine, queues :: Messages, nat :: Maybe (Integer, Integer)}

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = State machines mempty Nothing
  where
    (ReadInput k) = parseToMachine str
    machines = M.fromList [(i, k i) | i <- [0 .. 49]]

instance Semigroup Messages where
  (Messages m1) <> (Messages m2) = Messages (M.unionWith (<>) m1 m2)

instance Monoid Messages where
  mempty = Messages mempty

stepMachine :: Machine -> Seq Integer -> Writer Messages Machine
stepMachine (ReadInput k) (i :<| is) = stepMachine (k i) is
stepMachine (ReadInput k) Empty = return (k (-1))
stepMachine (WriteOutput i (WriteOutput x (WriteOutput y k))) is = do
  tell (Messages (M.singleton i (Seq.fromList [x, y])))
  stepMachine k is
stepMachine m is = error (show (m, is))

stepMachines :: Machines -> Messages -> Writer Messages Machines
stepMachines machines (Messages inputs) = M.fromList <$> mapM step (M.toList machines)
  where
    step (i, machine) = do
      machine' <- stepMachine machine (fromMaybe mempty (inputs M.!? i))
      return (i, machine')

idle :: Machines -> Messages -> Bool
idle machines messages =
  isEmpty messages
    && isEmpty (execWriter (stepMachines machines messages))
  where
    isEmpty (Messages m) = M.null m

--solve :: Input -> Output
solve input = go input (-1)
  where
    go (State machines messages (Just (x, y))) lastY
      | idle machines messages =
        if y == lastY
          then y
          else go (State machines (Messages (M.singleton 0 (Seq.fromList [x, y]))) Nothing) y
    go (State machines messages nat) lastY =
      let (machines', messages') = runWriter (stepMachines machines messages)
       in case messages' ^? #_Messages . ix 255 of
            Just (_ :|> x :|> y) -> go (State machines' messages' (Just (x, y))) lastY
            Nothing -> go (State machines' messages' nat) lastY