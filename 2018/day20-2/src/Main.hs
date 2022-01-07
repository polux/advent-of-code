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

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

-- #region imports

import Control.Lens (at, each, folded, isn't, ix, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import Data.List (elemIndex, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
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
import Linear (V2 (..), _x, _y)
import Safe hiding (at)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import Control.Arrow ((***))
import Data.Foldable (toList)
import Graphics.Gloss

-- #endregion

-- #region regex parsing
{-
import Text.Regex.Pcre2 (regex)

parseRegex :: String -> [Int]
parseRegex = map parseLine . T.lines . T.pack
 where
   toInt = read . T.unpack
   parseLine [regex|some example regex: (?<x>\d+)|] = toInt x
-}
-- #endregion

data Expr a = Concat (Expr a) (Expr a) | Branch (Expr a) (Expr a) | Leaf a | Eps
  deriving (Show, Generic, Eq, Ord)

data Dir = N | E | S | W
  deriving (Show, Generic, Eq, Ord)

type Input = Expr Dir

main :: IO ()
main = getContents >>= print . solve . parse
  --getContents >>= display FullScreen black . edgesToPicture . fst . exprToEdges . parse

parse :: String -> Input
parse = runParserOrDie inputParser . head . lines
  where
    inputParser = do
      P.char '^'
      tree <- concatParser
      P.char '$'
      P.eof
      return tree
    concatParser = foldr Concat Eps <$> P.many blockParser
    blockParser = leafParser P.<|> branchParser
    leafParser = Leaf <$> dirParser
    dirParser = P.choice [do P.char chr; return dir | (chr, dir) <- [('N', N), ('E', E), ('S', S), ('W', W)]]
    branchParser = foldr Branch Eps <$> P.between (P.char '(') (P.char ')') disjunctionParser
    disjunctionParser = concatParser `P.sepBy1` P.char '|'

dirToDelta :: Dir -> V2 Int
dirToDelta N = V2 0 1
dirToDelta E = V2 1 0
dirToDelta S = V2 0 (-1)
dirToDelta W = V2 (-1) 0

distances :: V2 Int -> Map (V2 Int) [V2 Int] -> Map (V2 Int) Int
distances start edges = go (M.singleton start 0) (Seq.singleton start)
 where
   go dists Empty = dists
   go dists (pos :<| queue) =
     let
       neighbors = filter (`M.notMember` dists) (edges M.! pos)
       posDist = dists M.! pos
     in go
          (dists <> M.fromList [(n, posDist+1) | n <- neighbors])
          (queue <> Seq.fromList neighbors)

type Graph = (Set (V2 Int, V2 Int), Set (V2 Int))

liftS2 :: (Ord t1, Ord b, Ord t2) => (t1 -> t2 -> b) -> Set t1 -> Set t2 -> Set b
liftS2 f s1 s2 = S.fromList [f x y | x <- toList s1, y <- toList s2]

chain :: Graph -> Graph -> Graph
chain (m1, os1) (m2, os2) = (m1 <> liftS2 shift os1 m2, liftS2 (+) os1 os2)
 where
   shift delta (p1, p2)= (p1+delta, p2+delta)

overlay :: Graph -> Graph -> Graph
overlay (m1, os1) (m2, os2) = (m1 <> m2, os1 <> os2)

inject :: Dir -> Graph
inject dir = (S.singleton (V2 0 0, pos), S.singleton pos)
 where
   pos = dirToDelta dir

exprToEdges :: Expr Dir -> Graph
exprToEdges (Concat e1 e2) = chain (exprToEdges e1) (exprToEdges e2)
exprToEdges (Branch e1 e2) = overlay (exprToEdges e1) (exprToEdges e2)
exprToEdges (Leaf d) = inject d
exprToEdges Eps = (mempty, S.singleton (V2 0 0))

edgesToGraph :: Set (V2 Int, V2 Int) -> Map (V2 Int) [V2 Int]
edgesToGraph edges = M.map toList $ M.fromListWith (<>) $ concat [[(a, S.singleton b), (b, S.singleton  a)] | (a,b) <- toList edges]

edgesToPicture :: Set (V2 Int, V2 Int) -> Picture
edgesToPicture (toList->edges) = color white $ pictures [line [toPoint p1, toPoint p2] | (p1, p2) <- edges]
 where
   toPoint (V2 x y) = (fromIntegral x, fromIntegral y)

solve = length . filter (>=1000) . M.elems . distances (V2 0 0) . edgesToGraph . fst . exprToEdges