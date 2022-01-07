-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Util where

import qualified Data.Array.IArray as IA
import Data.Function ((&))
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Int (Int32, Int64)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Data.Void (Void)
import Data.Word (Word8)
import Debug.Trace (trace)
import Linear (V2 (..))
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)
import Text.Pretty.Simple (pShow)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8

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

arrayFromList2D :: (Num n, Enum n, IA.IArray a e, IA.Ix i, TwoD i n) => [[e]] -> a i e
arrayFromList2D es =
  IA.array
    (mkV2 0 0, mkV2 (fromIntegral (width -1)) (fromIntegral (height -1)))
    [(mkV2 i j, e) | (j, row) <- zip [0 ..] es, (i, e) <- zip [0 ..] row]
  where
    height = length es
    width
      | height == 0 = 0
      | otherwise = length (head es)

listFromArray2D :: (Num n, Enum n, IA.IArray a e, IA.Ix i, TwoD i n) => a i e -> [[e]]
listFromArray2D arr = [row j | j <- [0 .. height]]
  where
    row j = [arr IA.! mkV2 i j | i <- [0 .. width]]
    (width, height) = toPair (arraySize arr)

mapArrayWithIndex :: (IA.Ix i, IA.IArray a e, IA.IArray a e') => (i -> e -> e') -> a i e -> a i e'
mapArrayWithIndex f arr = IA.array (IA.bounds arr) [(i, f i e) | (i, e) <- IA.assocs arr]

class TwoD i n | i -> n where
  mkV2 :: n -> n -> i
  toPair :: i -> (n, n)

instance Num a => TwoD (a, a) a where
  mkV2 x y = (x, y)
  toPair = id

instance Num a => TwoD (V2 a) a where
  mkV2 x y = V2 x y
  toPair (V2 x y) = (x, y)

arraySize :: (Num n, IA.IArray a e, IA.Ix i, TwoD i n) => a i e -> i
arraySize a =
  let (maxI, maxJ) = toPair (snd (IA.bounds a))
   in mkV2 (maxI + 1) (maxJ + 1)

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

converge :: Ord a => (a -> a) -> a -> a
converge f x =
  let x' = f x
   in if x == x' then x else converge f x

groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy f xs = M.fromListWith (++) [(f x, [x]) | x <- xs]

bfs
  :: Ord a
  => (a -> [a])  -- ^ neighbors
  -> a           -- ^ source
  -> (a -> Bool) -- ^ isTarget
  -> [a]         -- ^ path from source to target
bfs neighbors source isTarget = go (S.singleton source) (Seq.singleton [source])
  where
    go _ Empty = error "not found"
    go seen ([] :<| _) = error "unexpected empty path"
    go seen (path@(cell : _) :<| paths) =
      let ns = S.fromList (neighbors cell)
       in if any isTarget ns
            then
              reverse (head (filter isTarget (S.toList ns)) : path)
            else
              go
                (S.union ns seen)
                (paths <> (ns & (`S.difference` seen) & S.toList & map (: path) & Seq.fromList))

md5 :: String -> String
md5 str =
  str
    & BS8.pack
    & MD5.hash
    & Base16.encode
    & BS8.unpack

neighbors2D :: TwoD i Int => i -> i -> [i]
neighbors2D (toPair-> (width, height)) (toPair-> (i, j)) =
  filter withinBounds [mkV2 i (j-1), mkV2 (i+1) j, mkV2 i (j+1), mkV2 (i-1) j]
 where
  withinBounds (toPair->(i,j)) = i >= 0 && i < width && j >= 0 && j < height