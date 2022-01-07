-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import qualified Data.Array.IArray as IA
import Data.Function ((&))
import qualified Data.Graph.Inductive as IG
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
import Linear (V2 (..), V3 (V3))
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)
import Text.Pretty.Simple (pShow)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (toList)
import Test.QuickCheck ((==>), Positive(..), NonZero(..), NonNegative(..))
import qualified Data.Graph as G
import Data.Graph.Inductive (neighbors)
import Data.List (tails, foldl')
import Numeric (readHex, showHex)
import qualified Data.OrdPSQ as PQueue
import Data.Maybe (fromMaybe)

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
listFromArray2D arr = [row j | j <- [0 .. height-1]]
  where
    row j = [arr IA.! mkV2 i j | i <- [0 .. width-1]]
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

iGraphFromAdjList :: Ord a => [(a, [(b, a)])] -> (Gr a b, Map a Int)
iGraphFromAdjList adjList = (IG.mkGraph nodes edges, nodeIndices)
  where
    nodeIndices = M.fromList (zip nodeLabels [0 ..])
    nodeLabels = S.toList $ S.fromList $ map fst adjList ++ concatMap (map snd . snd) adjList
    nodes = [(n, label) | (label, n) <- M.toList nodeIndices]
    edges =
      [ (nodeIndices M.! source, nodeIndices M.! target, label)
        | (source, targets) <- adjList,
          (label, target) <- targets
      ]

converge :: Eq t => (t -> t) -> t -> t
converge f x =
  let x' = f x
   in if x == x' then x else converge f x'

groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy f xs = M.fromListWith (++) [(f x, [x]) | x <- xs]

-- >>> let { f 1 = [2,3]; f 2 = [4]; f 3 = [5, 6]; f 5 = [4]; f _ = []} in bfs f (==4) 1
-- Just [1,2,4]
bfs
  :: Ord a
  => (a -> [a])  -- ^ neighbors
  -> (a -> Bool) -- ^ isTarget
  -> a           -- ^ source
  -> Maybe [a]   -- ^ path from source to target
bfs neighbors isTarget source = go mempty (Seq.singleton [source])
 where
  go _ Empty = Nothing
  go seen ([] :<| _) = error "unexpected empty path"
  go seen (path@(cell : _) :<| paths) =
    if isTarget cell
      then Just $ reverse path
      else
        go
          (S.insert cell seen)
          (paths <> Seq.fromList [n : path | n <- neighbors cell])

-- >>> let { f 1 = [(2,2::Int),(3,1)]; f 2 = [(4,1)]; f 3 = [(5,1), (6,1)]; f 5 = [(4,1)]; f _ = []} in dijkstra f (==4) 1
-- Just (2,[1,2,4])
dijkstra
  :: (Ord a, Num c, Ord c, Bounded c)
  => (a -> [(a, c)])   -- ^ neighbors
  -> (a -> Bool)       -- ^ isTarget
  -> a                 -- ^ source
  -> Maybe (c, [a]) -- ^ path from source to target
dijkstra neighbors isTarget source = go mempty (PQueue.singleton source 0 [source])
 where
  go done queue =
    case PQueue.minView queue of
      Nothing -> Nothing
      Just (u, distU, pathToU, queue') ->
        if isTarget u
          then Just (distU, reverse pathToU)
          else
            go
              (S.insert u done)
              (foldr (step done u pathToU distU) queue' (neighbors u & filter ((`S.notMember` done).fst)))
  step done u pathToU distU (v,c) queue =
    let alt = distU + c
        distV = maybe maxBound fst (PQueue.lookup v queue)
     in if alt < distV
          then PQueue.insert v alt (v : pathToU) queue
          else queue

-- >>> let { f 1 = [2,3]; f 2 = [4]; f 3 = [5, 6]; f _ = []} in reachableSet f 1
-- fromList [1,2,3,4,5,6]
reachableSet
  :: Ord a
  => (a -> [a]) -- ^ neighbors
  -> a          -- ^ source
  -> S.Set a
reachableSet neighbors source = dfs mempty [source]
 where
  dfs seen [] = seen
  dfs seen (v : vs)
    | v `S.member` seen = dfs seen vs
    | otherwise = dfs (S.insert v seen) (neighbors v <> vs)

md5 :: String -> String
md5 str =
  str
    & BS8.pack
    & MD5.hash
    & Base16.encode
    & BS8.unpack

neighbors2D
  :: TwoD i Int
  => i -- ^ (width, height)
  -> i -- ^ (x, y)
  -> [i] -- ^ neighbors between (0,0) incluced and (width, height) excluded
neighbors2D (toPair-> (width, height)) (toPair-> (i, j)) =
  filter withinBounds [mkV2 i (j-1), mkV2 (i+1) j, mkV2 i (j+1), mkV2 (i-1) j]
 where
  withinBounds (toPair->(i,j)) = i >= 0 && i < width && j >= 0 && j < height

-- | chineseRemainder [(a1,n1), (a2,n2), ...] finds the smallest x such that
-- x = a1 (mod n1)
-- x = a2 (mod n2)
-- ...
chineseRemainder :: [(Int, Int)] -> Int
chineseRemainder [] = error "unexpected empty input"
chineseRemainder ((a,n):eqs) = go eqs a n
 where
   go ((a,n):eqs) x m = go eqs (head [y | y <- [x,x+m..], y `mod` n == a]) (lcm n m)
   go [] x _ = x

-- | minv a m = x such that a . x = 1 (mod m)
-- prop> \(NonZero a) (Positive m) -> gcd a m == 1 && (m > 1) ==> (a * (minv a m)) `mod` m == 1
-- +++ OK, passed 100 tests; 68 discarded.
minv :: (Integral a, Show a) => a -> a -> a
minv a n
  | a == 0 || n <= 1 || g /= 1 = error (show a <> " has no inverse modulo " <> show n)
  | otherwise = x `mod` n
 where
  (x, y, g) = extGCD (a `mod` n) n
  extGCD 0 0 = undefined
  extGCD a 0 = (1, 0, a)
  extGCD a b =
    let (q, r) = a `quotRem` b
        (c, x, y) = extGCD b r
     in (x, c - q * x, y)

-- | solveLinearCongruence a b n finds x such that a . x = b (mod n)
-- prop> \(NonZero a) (NonNegative b) (Positive m) -> gcd a m == 1 && (b < m) && (m > 1) ==> (a * solveLinearCongruence a b m) `mod` m == b
-- +++ OK, passed 100 tests; 207 discarded.
solveLinearCongruence :: (Integral a, Show a) => a -> a -> a -> a
solveLinearCongruence a b n
  | b < 0 || b >= n = error "in solveLinearCongruence a b n, b must be >=0 and < n"
  | otherwise = b * minv a n

-- >>> findRecurringElement [0,1,2,3,1,4,5]
-- (1,4)
findRecurringElement :: Ord a => [a] -> (Int, Int)
findRecurringElement = go 0 mempty
  where
    go _ _ [] = error "non-infinite list"
    go i seen (x : xs)
      | Just j <- M.lookup x seen = (j, i)
      | otherwise = go (i + 1) (M.insert x i seen) xs

graphFromNeighbors
  :: Ord a
  => (a -> [a]) -- ^ neihbors
  -> [a]        -- ^ nodes
  -> (G.Graph, G.Vertex -> a)
graphFromNeighbors neighbors nodes =
  let (graph, fromVertex, _) = G.graphFromEdges [(n, n, neighbors n) | n <- nodes]
  in (graph, \v -> let (node, _, _) = fromVertex v in node)

-- >>> let { f 1 = [2,3]; f 2 = [4]; f 3 = [5, 6]; f 5 = [4]; f _ = [] } in topologicalSort f [1..6]
-- [1,3,6,5,2,4]
topologicalSort
  :: Ord a
  => (a -> [a]) -- ^ neighbors
  -> [a] -- ^ nodes
  -> [a] -- ^ topological sorting of nodes
topologicalSort neighbors nodes = map nodeFromVertex (G.topSort graph)
 where
   (graph, nodeFromVertex) = graphFromNeighbors neighbors nodes

-- >>> let { f 3 = [1]; f 1 = [2]; f 2 = [0]; f 0 = [1] } in scc f [0..3]
-- [[0,1,2],[3]]
scc
  :: Ord a
  => (a -> [a]) -- ^ neighbors
  -> [a] -- ^ nodes
  -> [[a]]
scc neighbors nodes = map (map nodeFromVertex . toList) (G.scc graph)
  where
    (graph, nodeFromVertex) = graphFromNeighbors neighbors nodes

-- >>> choose 2 [1,2,3,4]
-- [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
choose :: Int -> [Int] -> [[Int]]
choose len xs = go (len -1) xs
  where
    go :: Int -> [Int] -> [[Int]]
    go 0 xs = do
      x <- xs
      return [x]
    go n xs = do
      (y : ys) <- tails xs
      (y :) <$> go (n -1) ys

-- | Assumes candidates for changing state are currently 'on' cells and their neighbors.
stepCellAutomaton
  :: Ord a
  => (a -> [a]) -- ^ neighbors
  -> (Bool -> Int -> Bool) -- ^ next cell state given current cell state and num active neighbors
  -> S.Set a    -- ^ 'on' cells
  -> S.Set a    -- ^ new 'on' cells
stepCellAutomaton neighbors next grid = S.filter activeNext candidates
  where
    candidates = grid <> S.unions (S.map (S.fromList . neighbors) grid)
    active cell = cell `S.member` grid
    activeNext cell = next (active cell) (length (filter active (neighbors cell)))

hexDeltaN, hexDeltaNE, hexDeltaSE, hexDeltaS, hexDeltaSW, hexDeltaNW :: Num a => V3 a
hexDeltaN = V3 1 (-1) 0
hexDeltaNE = V3 0 (-1) 1
hexDeltaSE = V3 (-1) 0 1
hexDeltaS = V3 (-1) 1 0
hexDeltaSW = V3 0 1 (-1)
hexDeltaNW = V3 1 0 (-1)

hexNeighbors :: Num a => V3 a -> [V3 a]
hexNeighbors pos = [pos + delta | delta <- [hexDeltaN, hexDeltaNE, hexDeltaSE, hexDeltaS, hexDeltaSW, hexDeltaNW]]

hexDistance :: (Num a, Ord a) => V3 a -> V3 a -> a
hexDistance (V3 x1 y1 z1) (V3 x2 y2 z2) = maximum [abs (x2-x1), abs (y2-y1), abs (z2-z1)]

-- >>> fromBinary [0,1,0,0,1]
-- 9
fromBinary :: [Int] -> Int
fromBinary = foldl' (\acc b -> acc * 2 + b) 0

-- >>> toBinary 9
-- [1,0,0,1]
-- prop> \(Positive n) -> fromBinary (toBinary n) == n
-- +++ OK, passed 100 tests.
toBinary :: Int -> [Int]
toBinary = reverse . go
  where
    go 0 = []
    go n = let (q,r) = n `divMod` 2 in r : go q

-- >>> fromHex "a1b"
-- 2587
fromHex :: String -> Int
fromHex = fst . head . readHex

-- prop> \(Positive n) -> fromHex (toHex n) == n
-- +++ OK, passed 100 tests.
toHex :: Int -> String
toHex i = showHex i ""

singleton :: a -> [a]
singleton x = [x]

-- >>> bisect (\n -> n*n <= 152399025) 0 152399025
-- 12345
bisect
  :: (Int -> Bool) -- ^ pred
  -> Int -- ^ lower bound, must satisfy pred
  -> Int  -- ^ upper bound, must falsify pred
  -> Int -- ^ largest int satisfying pred
bisect p l u
  | l+1 == u = l
  | otherwise =
      let m = (l+u) `div` 2
      in if p m then bisect p m u else bisect p l m
