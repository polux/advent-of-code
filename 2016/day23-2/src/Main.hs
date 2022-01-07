-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
-- #region language extensions
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (Ixed (ix), Lens', each, folded, isn't, traversed, (%~), (&), (+~), (-~), (.~), (?~), (^.), (^..), (^?))
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
import Data.List (elemIndex, scanl', sortOn)
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

data Register = A | B | C | D
  deriving (Show)

data Atom = Reg Register | Lit Int
  deriving (Show)

data Instr
  = Cpy Atom Atom
  | Inc Atom
  | Dec Atom
  | Jnz Atom Atom
  | Tgl Atom
  | Mul Register Atom Atom -- mul a b c means a <- a + b*c
  deriving (Show)

type Input = Vector Instr

main :: IO ()
main = getContents >>= putStrLn . solve . parse

parse :: String -> Input
parse = V.fromList . map parseLine . lines
  where
    parseLine ('c' : 'p' : 'y' : ' ' : cs) =
      let [x, y] = words cs
       in Cpy (parseAtom x) (parseAtom y)
    parseLine ('i' : 'n' : 'c' : ' ' : cs) = Inc (parseAtom cs)
    parseLine ('m' : 'u' : 'l' : ' ' : cs) =
      let [[x], y, z] = words cs
       in Mul (parseReg x) (parseAtom y) (parseAtom z)
    parseLine ('d' : 'e' : 'c' : ' ' : cs) = Dec (parseAtom cs)
    parseLine ('j' : 'n' : 'z' : ' ' : cs) =
      let [x, y] = words cs
       in Jnz (parseAtom x) (parseAtom y)
    parseLine ('t' : 'g' : 'l' : ' ' : cs) = Tgl (parseAtom cs)
    parseAtom (c : cs)
      | c `elem` "abcd" = Reg (parseReg c)
      | otherwise = Lit (parseInt (c : cs))
    parseInt ('-' : cs) = - (read cs)
    parseInt cs = read cs
    parseReg 'a' = A
    parseReg 'b' = B
    parseReg 'c' = C
    parseReg 'd' = D

data MachineState = MachineState {prog :: Input, aReg :: Int, bReg :: Int, cReg :: Int, dReg :: Int, pc :: Int}
  deriving (Show, Generic)

showMachine MachineState {..} = show (prog V.! pc, aReg, bReg, cReg, dReg)

regLens :: Register -> Lens' MachineState Int
regLens A = #aReg
regLens B = #bReg
regLens C = #cReg
regLens D = #dReg

exec :: MachineState -> Instr -> MachineState
exec m (Cpy a (Reg r)) = m & regLens r .~ resolve m a & #pc +~ 1
exec m (Inc (Reg r)) = m & regLens r +~ 1 & #pc +~ 1
exec m (Mul r a b) = m & regLens r +~ (resolve m a * resolve m b) & #pc +~ 1
exec m (Dec (Reg r)) = m & regLens r -~ 1 & #pc +~ 1
exec m (Jnz a b) = m & #pc +~ (if resolve m a /= 0 then resolve m b else 1)
exec m (Tgl a) = m & #prog . ix (pc m + resolve m a) %~ toggle & #pc +~ 1
exec m _ = m & #pc +~ 1

toggle :: Instr -> Instr
toggle (Inc a) = Dec a
toggle (Dec a) = Inc a
toggle (Tgl a) = Inc a
toggle (Jnz a b) = Cpy a b
toggle (Cpy a b) = Jnz a b

step :: MachineState -> MachineState
step m = exec m (prog m V.! pc m)

resolve :: MachineState -> Atom -> Int
resolve _ (Lit n) = n
resolve m (Reg r) = m ^. regLens r

playout :: Input -> [MachineState]
playout prog = iterate step (MachineState prog 12 0 0 0 0)

endState :: Input -> MachineState
endState input = head $ filter ((>= length input) . pc) $ playout input

--solve input = playout input & map showMachine & unlines
solve input = show $ aReg (endState input)
