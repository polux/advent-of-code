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
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

type Input = [String]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = lines

type P a = String -> (a, String)

solve :: Input -> Output
solve input = sum (map (fst . mults) input)
  where
    eatSpace str = dropWhile (== ' ') str
    mults :: P Int
    mults str =
      let (i, str') = adds str
          (j, str'') = multsCont str'
       in (i * j, str'')
    multsCont :: P Int
    multsCont ('*' : str) = mults (eatSpace str)
    multsCont str = (1, str)
    adds :: P Int
    adds str =
      let (i, str') = atom str
          (j, str'') = addsCont str'
       in (i + j, str'')
    addsCont :: P Int
    addsCont ('+' : str) = adds (eatSpace str)
    addsCont str = (0, str)
    atom :: P Int
    atom (c : str) | isDigit c = (read [c], eatSpace str)
    atom ('(' : str) =
      let (i, ')' : str') = mults str
       in (i, eatSpace str')
