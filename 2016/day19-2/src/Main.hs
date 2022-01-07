-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
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
import Data.List (sortOn, elemIndex)
import Control.Monad (forM_)

-- #endregion


main :: IO ()
--main = print =<< solve 6
main = print =<< solve 3018458

toList ::  MUV.IOVector Int -> IO [(Int, Int)]
toList vec = go 0
 where
   go n | n == MUV.length vec = return []
        | otherwise = do
                val <- MUV.read vec n
                tail <- go (n+1)
                return ((n+1, val+1):tail)

solve :: Int -> IO Int
solve len = do
  vec <- MUV.unsafeNew len
  forM_ [0..len-2] $ \i -> do
    MUV.write vec i (i+1)
  MUV.write vec (len-1) 0
  res <- go (even len) (len `div` 2 - if even len then 2 else 1) vec
  return (res+1)
 where
  go :: Bool -> Int -> MUV.IOVector Int -> IO Int
  go skipOne i vec = do
--    ls <- toList vec
--    print (skipOne, i+1, ls)
    next <- MUV.read vec i
    nextNext <- MUV.read vec next
    if next == i
      then return i
      else do
        if skipOne
          then do
            nextNextNext <- MUV.read vec nextNext
            MUV.write vec next nextNextNext
            go False next vec
          else do
            MUV.write vec i nextNext
            go True i vec