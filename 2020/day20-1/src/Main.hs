-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (transpose, (\\))
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
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

type Input = [Tile]

type Bitmap = [String]

type Tile = (Int, Bitmap)

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = map parseTile (splitOn "\n\n" str)
  where
    parseTile (lines -> (title : bitmap)) = (parseId title, bitmap)
    parseId :: String -> Int
    parseId str =
      let [[_, id]] = str =~ "Tile ([0-9]+):"
       in read id

borders :: Bitmap -> [String]
borders bitmap =
  [ head bitmap,
    last bitmap,
    head (transpose bitmap),
    last (transpose bitmap)
  ]

type Index = Map String (Set Int)

makeIndex :: Input -> Index
makeIndex = M.unionsWith (<>) . map makeEntry
  where
    makeEntry (id, bitmap) =
      M.fromList
        [ (border, S.singleton id)
          | border <- borders bitmap
        ]

numBordersWithoutMatch :: Index -> Tile -> Int
numBordersWithoutMatch index (id, bitmap) = length (filter noMatchingTile (borders bitmap))
  where
    noMatchingTile border =
      S.null (S.delete id (index M.! border))
      && case index M.!? reverse border of
           Nothing -> True
           Just tiles -> S.null (S.delete id tiles)

solve :: Input -> Output
solve input = product $ map fst $ filter ((== 2) . numBordersWithoutMatch index) input
  where
    index = traceShowId $ makeIndex input
