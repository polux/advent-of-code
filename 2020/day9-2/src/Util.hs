-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}

module Util where

import qualified Data.Array.IArray as IA
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Int (Int32, Int64)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Data.Void (Void)
import Data.Word (Word8)
import Debug.Trace (trace)
import Linear (V2 (..))
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)
import Text.Pretty.Simple (pShow)

pTraceShow :: Show a => a -> b -> b
pTraceShow x = trace (LT.unpack (pShow x))

pTraceShowId :: Show a => a -> a
pTraceShowId x = pTraceShow x x

type Parser = Parsec Void String

runParserOrDie :: Parser a -> String -> a
runParserOrDie p str =
  case runParser p "" str of
    Left err -> error (errorBundlePretty err)
    Right res -> res

arrayFromList1D :: (IA.IArray a e, IA.Ix i, Num i, Enum i) => [e] -> a i e
arrayFromList1D es = IA.array (0, fromIntegral (length es) - 1) (zip [0 ..] es)

arrayFromList2D :: (IA.IArray a e, IA.Ix i, TwoD i) => [[e]] -> a i e
arrayFromList2D es =
  IA.array
    (mkV2 0 0, mkV2 (width -1) (height -1))
    [(mkV2 i j, e) | (j, row) <- zip [0 ..] es, (i, e) <- zip [0 ..] row]
  where
    height = length es
    width
      | height == 0 = 0
      | otherwise = length (head es)

class TwoD i where
  mkV2 :: Int -> Int -> i

instance Num a => TwoD (a, a) where
  mkV2 x y = (fromIntegral x, fromIntegral y)

instance Num a => TwoD (V2 a) where
  mkV2 x y = V2 (fromIntegral x) (fromIntegral y)

class PlusOneAble i where
  plusOne :: i -> i

instance PlusOneAble Int where
  plusOne i = i + 1

instance PlusOneAble Int32 where
  plusOne i = i + 1

instance PlusOneAble Int64 where
  plusOne i = i + 1

instance PlusOneAble Word8 where
  plusOne i = i + 1

instance (Num a, Num b) => PlusOneAble (a, b) where
  plusOne (i, j) = (i + 1, j + 1)

instance Num a => PlusOneAble (V2 a) where
  plusOne v = v + V2 1 1

arraySize :: (IA.IArray a e, IA.Ix i, PlusOneAble i) => a i e -> i
arraySize a = plusOne (snd (IA.bounds a))

graphFromAdjList :: Ord a => [(a, [(b, a)])] -> (Gr a b, Map a Int)
graphFromAdjList adjList = (G.mkGraph nodes edges, nodeIndices)
  where
    nodeIndices = M.fromList (zip nodeLabels [0 ..])
    nodeLabels = S.toList $ S.fromList $ map fst adjList ++ concatMap (map snd . snd) adjList
    nodes = [(n, label) | (label, n) <- M.toList nodeIndices]
    edges =
      [ (nodeIndices M.! source, nodeIndices M.! target, label)
        | (source, targets) <- adjList,
          (label, target) <- targets
      ]
