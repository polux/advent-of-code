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
import Data.List (transpose,filter,isPrefixOf)
import Data.List ((\\))

-- #endregion

data Input = Notes { rules :: [Rule], myTicket :: Ticket, nearbyTickets :: [Ticket]}
  deriving (Show)
data Rule = Rule { ruleName :: String, fistInterval :: (Int, Int), secondInterval :: (Int, Int)}
  deriving (Show)
type Ticket = [Int]


main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = runParserOrDie notesP str
 where
   notesP = do
     rules <- ruleP `P.endBy` P.newline
     P.newline
     P.string "your ticket:"
     P.newline
     myTicket <- ticketP
     P.newline
     P.newline
     P.string "nearby tickets:"
     P.newline
     nearbyTickets <- ticketP `P.endBy` P.newline
     return $ Notes rules myTicket nearbyTickets
   ruleP = do
     ruleName <- P.some (P.alphaNumChar P.<|> P.char ' ')
     P.string ": "
     firstInterval <- intervalP
     P.string " or "
     secondInterval <- intervalP
     return $ Rule ruleName firstInterval secondInterval
   intervalP = do
     low <- L.decimal
     P.char '-'
     high <- L.decimal
     return (low, high)
   ticketP = L.decimal `P.sepBy` P.char ','

validValue :: Rule -> Int -> Bool
validValue (Rule _ (lo1, hi1) (lo2, hi2)) v =
  (v >= lo1 && v <= hi1) || (v >= lo2 && v <= hi2)

validValueForAnyRule :: [Rule] -> Int -> Bool
validValueForAnyRule rules v = any (flip validValue v) rules

invalidValues :: [Rule] -> Ticket -> [Int]
invalidValues rules ticket = filter (not . validValueForAnyRule rules) ticket

candidateRules :: [Rule] -> [Int] -> [Rule]
candidateRules rules vs = filter (\rule -> all (validValue rule) vs) rules

prune :: [[String]] -> [[String]]
prune candidates | traceShow candidates False = undefined
prune candidates = [foldr remove candidate singletons | candidate <- candidates]
 where
   singletons = filter ((==1) . length) candidates
   remove singleton candidate | candidate /= singleton = candidate \\ singleton
   remove _ candidate = candidate

converge :: Eq t => (t -> t) -> t -> t
converge f x =
  let x' = f x
  in if x == x' then x else converge f x'

solve (Notes rules myTicket tickets) =
  filter (("departure" `isPrefixOf`) . fst) annotatedFields
  & map snd
  & product
 where
   validTickets = filter (all (validValueForAnyRule rules)) tickets
   columns = transpose validTickets
   candidates = map (map ruleName . candidateRules rules) columns
   fields = map head (converge prune candidates)
   annotatedFields = zip fields myTicket
