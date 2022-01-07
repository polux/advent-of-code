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
import Data.List.Split (splitOn)
import Data.Foldable (traverse_, asum)

-- #endregion

data Rhs = Chr Char | Conj [[Int]]
  deriving (Show)
type Grammar = Map Int Rhs

type Input = (Grammar, [String])

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = (grammar, lines chunk2)
  where
    [chunk1, chunk2] = splitOn "\n\n" str
    grammar = M.unions (map parseRule (lines chunk1))
    parseRule str =
      let [ruleName, rhsString] = splitOn ":" str
          rhs =
               case rhsString =~ " *\"(.)\"" of
                [[_, [chr]]] -> Chr chr
                _ -> Conj $ map (map read . words) (splitOn "|" rhsString)
       in M.singleton (read ruleName) rhs

compile :: Grammar -> Parser ()
compile gr = translateRhs (gr M.! 0) <* P.eof
 where
   translateRhs :: Rhs -> Parser ()
   translateRhs (Chr chr) = do
     P.char chr
     return ()
   translateRhs (Conj seqs) = asum (map (P.try . translateSeq) seqs)
   translateSeq :: [Int] -> Parser ()
   translateSeq names = traverse_ (translateRhs . (gr M.!)) names

solve :: Input -> Output
solve (gr, strs) = length (filter valid strs)
  where
    parser = compile gr
    valid str = (P.runParser parser "" str) == Right ()
