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
import Data.MemoTrie hiding (Reg)
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortOn, elemIndex)

-- #endregion

data Reg = A | B
 deriving (Show)
data Instr = Hlf Reg | Tpl Reg | Inc Reg | Jmp Int | Jie Reg Int | Jio Reg Int
 deriving (Show)

type Input = Vector Instr

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = V.fromList . map parseInstr . lines
 where
   parseInstr ('h':'l':'f':' ':reg:_) = Hlf (parseReg reg)
   parseInstr ('t':'p':'l':' ':reg:_) = Tpl (parseReg reg)
   parseInstr ('i':'n':'c':' ':reg:_) = Inc (parseReg reg)
   parseInstr ('j':'m':'p':' ':offset) = Jmp (parseOffset offset)
   parseInstr ('j':'i':'e':' ':reg:',':' ':offset) = Jie (parseReg reg) (parseOffset offset)
   parseInstr ('j':'i':'o':' ':reg:',':' ':offset) = Jio (parseReg reg) (parseOffset offset)
   parseInstr s = error ("unknown instr " <> s)
   parseReg 'a' = A
   parseReg 'b' = B
   parseReg c = error ("unknown register" <> [c])
   parseOffset ('+':num) = read num
   parseOffset ('-':num) = - (read num)
   parseOffset s = error ("unknown offset " <> s)

run :: Input -> (Int, Int)
run program = go 0 0 0
 where
   go a b pc | pc < length program = exec (program V.! pc)
             | otherwise = (a, b)
     where
       select A = a
       select B = b
       exec (Hlf A) = go (a `div` 2) b (pc+1)
       exec (Hlf B) = go a (b `div` 2) (pc+1)
       exec (Tpl A) = go (a * 3) b (pc+1)
       exec (Tpl B) = go a (b * 3) (pc+1)
       exec (Inc A) = go (a + 1) b (pc+1)
       exec (Inc B) = go a (b + 1) (pc+1)
       exec (Jmp offset) = go a b (pc+offset)
       exec (Jie reg offset) = go a b (pc + if even (select reg) then offset else 1)
       exec (Jio reg offset) = go a b (pc + if select reg == 1 then offset else 1)

solve :: Input -> Output
solve input = snd $ run input
