-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Data.IORef
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

type Input = [Int]

type Output = Seq Int

data Node = Node
  { value :: Int,
    next :: Maybe (IORef Node)
  }
  deriving (Generic)

type Mapping = Map Int (IORef Node)

newNode :: Int -> IO (IORef Node)
newNode i = newIORef (Node i Nothing)

plug :: IORef Node -> IORef Node -> IO ()
plug left right = modifyIORef left (#next ?~ right)

listToRing :: [Int] -> IO (IORef Node)
listToRing [] = error "empty list"
listToRing (x : xs) = do
  ref <- newNode x
  go ref ref xs
  return ref
  where
    go :: IORef Node -> IORef Node -> [Int] -> IO ()
    go firstNodeRef previousNodeRef [] = do
      plug previousNodeRef firstNodeRef
    go firstNodeRef previousNodeRef (y : ys) = do
      ref <- newNode y
      plug previousNodeRef ref
      go firstNodeRef ref ys

ringToAssocs :: IORef Node -> IO [(Int, IORef Node)]
ringToAssocs firstNodeRef = go firstNodeRef
  where
    go ref = do
      Node val next <- readIORef ref
      tail <- case next of
        Just nextRef ->
          if nextRef == firstNodeRef
            then pure []
            else go nextRef
        Nothing -> pure []
      return ((val, ref) : tail)

ringToList :: IORef Node -> IO [Int]
ringToList ref = map fst <$> ringToAssocs ref

buildMapping :: IORef Node -> IO Mapping
buildMapping ref = M.fromList <$> ringToAssocs ref

input :: String
--input = "389125467"
input = "916438275"

main :: IO ()
main = solve (parse input) >>= print

parse :: String -> Input
parse str = take 1_000_000 (prefix <> [maximum prefix + 1 ..])
  where
    prefix = map readChar str
    readChar c = read [c]

removeSliceAfter ::
  IORef Node -> -- the node after which to remove the slice
  Int -> -- the length of the slice
  IO (IORef Node) -- the head of the removed slice
removeSliceAfter _ 0 = error "slice length must be >0"
removeSliceAfter firstNodeRef n = do
  Node _ (Just sliceHeadRef) <- readIORef firstNodeRef
  go firstNodeRef sliceHeadRef n
  return sliceHeadRef
  where
    go previousNodeRef ref 0 = do
      modifyIORef previousNodeRef (#next .~ Nothing)
      plug firstNodeRef ref
    go _ ref n = do
      Node _ (Just next) <- readIORef ref
      go ref next (n - 1)

insertSliceAfter ::
  IORef Node -> -- the node after which to insert the slice
  IORef Node -> -- the head of the slice
  IO ()
insertSliceAfter firstNodeRef sliceHeadRef = do
  Node _ (Just ref) <- readIORef firstNodeRef
  plug firstNodeRef sliceHeadRef
  plugAtTheEnd sliceHeadRef ref
  where
    plugAtTheEnd ref endRef = do
      Node _ next <- readIORef ref
      case next of
        Just nextRef -> plugAtTheEnd nextRef endRef
        Nothing -> plug ref endRef

step ::
  Int -> -- max value
  Mapping ->
  IORef Node ->
  IO (IORef Node) -- next current node
step maxValue mapping currentRef = do
  Node val _ <- readIORef currentRef
  sliceHeadRef <- removeSliceAfter currentRef 3
  slice <- ringToList sliceHeadRef
  let destinationRef = mapping M.! closest val slice
  insertSliceAfter destinationRef sliceHeadRef
  Node _ (Just nextRef) <- readIORef currentRef
  return nextRef
  where
    closest i slice = go (i - 1)
      where
        go 0 = go maxValue
        go j
          | j `elem` slice = go (j - 1)
          | otherwise = j

solve :: Input -> IO [Int]
solve numbers = do
  ringRef <- listToRing numbers
  mapping <- buildMapping ringRef
  go mapping 10_000_000 ringRef
  res <- ringToList ringRef
  return (take 3 (dropWhile (/= 1) res))
  where
    maxValue = maximum numbers
    go :: Mapping -> Int -> IORef Node -> IO ()
    go _ 0 _ = return ()
    go mapping n ref = do
      newRef <- step maxValue mapping ref
      go mapping (n - 1) newRef
