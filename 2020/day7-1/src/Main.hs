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
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Query.TransClos as G
import Data.Graph.Inductive.PatriciaTree (Gr)
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
import Util (Parser, pTraceShow, pTraceShowId, runParserOrDie)

-- #endregion

type Color = (String, String)

type PreGraph = [(Color, [(Color, Int)])] --Gr Color Int

type Input = (Gr Color Int, Map Color Int)

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

lexeme = L.lexeme P.space

symbol = L.symbol P.space

parse :: String -> Input
parse str = toGraph $ runParserOrDie (inputParser <* P.eof) str
  where
    toGraph :: PreGraph -> Input
    toGraph pregraph = (G.mkGraph nodes edges, namedNodes)
      where
        namedNodes = nameNodes pregraph
        nodes = [(n, color) | (color, n) <- M.toList namedNodes]
        edges =
          [ (namedNodes M.! containee, namedNodes M.! container, num)
            | (container, bags) <- pregraph,
              (containee, num) <- bags
          ]
    nameNodes :: PreGraph -> M.Map Color Int
    nameNodes pregraph = M.fromList (zip (S.toList colors) [0 ..])
      where
        colors = S.fromList (map fst pregraph)
    inputParser :: Parser PreGraph
    inputParser = entryParser `P.endBy1` (P.char '.' *> P.newline)
    entryParser = do
      color <- colorParser
      symbol "bag" <* P.optional (symbol "s")
      symbol "contain" <* P.optional (symbol "s")
      bags <- (symbol "no other bags" *> return []) P.<|> (bagParser `P.sepBy` symbol ",")
      return (color, bags)
    bagParser = do
      num <- lexeme (read <$> P.some P.digitChar)
      color <- colorParser
      symbol "bag" <* P.optional (symbol "s")
      return (color, num)
    colorParser = do
      word1 <- lexeme (P.many P.alphaNumChar)
      word2 <- lexeme (P.many P.alphaNumChar)
      return (word1, word2)

solve :: Input -> Output
solve (graph, namedNodes) = length (edgesFrom (G.tc graph) ("shiny", "gold"))
 where
   edgesFrom gr color =
     let (Just (_, _, _, linksFrom), _) = G.match (namedNodes M.! color) gr
     in linksFrom