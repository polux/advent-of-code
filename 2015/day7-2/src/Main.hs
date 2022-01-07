-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex, sortOn)
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
import Data.Word (Word16)
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
import Data.Bits

-- #endregion

type Input = Map String Expr

data Expr = Lit Atom | And Atom Atom | Or Atom Atom | LShift Atom Atom | RShift Atom Atom | Not Atom
  deriving (Show)

data Atom = AName String | ANum Word16
  deriving (Show)

type Output = Word16

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = M.fromList $ runParserOrDie (lineP `P.endBy` P.newline <* P.eof) str
  where
    nameP :: Parser String
    nameP = P.some (P.oneOf ['a' .. 'z'])
    numP :: Parser Word16
    numP = L.decimal
    atomP = (AName <$> nameP) P.<|> (ANum <$> numP)
    lineP :: Parser (String, Expr)
    lineP = do
      expr <- notP P.<|> P.try binOpP P.<|> (Lit <$> atomP)
      P.string " -> "
      output <- nameP
      return (output, expr)
    opP :: Parser (Atom -> Atom -> Expr)
    opP =
      asum
        [ P.string opName *> pure op
          | (opName, op) <-
              [("AND", And), ("OR", Or), ("LSHIFT", LShift), ("RSHIFT", RShift)]
        ]
    notP = do
      P.string "NOT "
      Not <$> atomP
    binOpP = do
      x <- atomP
      P.space
      op <- opP
      P.space
      y <- atomP
      return (op x y)

eval :: Input -> String -> Word16
eval circuit x = goMemo x
 where
   goMemo = memo go
   go x = evalExpr (circuit M.! x)
   evalExpr (Lit atom) = evalAtom atom
   evalExpr (And atom1 atom2) = evalAtom atom1 .&. evalAtom atom2
   evalExpr (Or atom1 atom2) = evalAtom atom1 .|. evalAtom atom2
   evalExpr (LShift atom1 atom2) = evalAtom atom1 `shiftL` fromIntegral (evalAtom atom2)
   evalExpr (RShift atom1 atom2) = evalAtom atom1 `shiftR` fromIntegral (evalAtom atom2)
   evalExpr (Not atom) = complement (evalAtom atom)
   evalAtom (ANum i) = i
   evalAtom (AName y) = goMemo y

solve :: Input -> Output
solve input = eval (M.insert "b" (Lit (ANum (eval input "a"))) input) "a"
