-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
-- #endregion

module Main where

-- #region imports

import Control.Lens ((%=), use, (.=), Lens', (*~), Ixed(ix), at, (+~), each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _4)
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
import qualified Data.Vector.Unboxed.Mutable as MUV
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..), _x, _y)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Safe hiding (at)
import Data.Functor ((<&>))
import Data.MemoTrie
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortOn, elemIndex)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad ((>=>), ap, liftM, forM_)
import Control.Monad.State (execState, evalState, MonadState(get), State)
import Control.Arrow (Arrow((***), (&&&)))

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

data Atom = Lit Int | Reg Char
  deriving (Show)
data Instr = Snd Char | Set Char Atom | Add Char Atom | Mul Char Atom | Mod Char Atom | Rcv Char | Jgz Atom Atom
  deriving (Show)

type Input = Vector Instr

type Output = Int

data MState = MState { regs :: Map Char Int, pc :: Int, lastPlayed :: Maybe Int }
  deriving (Show, Generic)




main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse = V.fromList . map parseLine . lines
 where
   parseLine str =
     case words str of
       ["snd", [x]] -> Snd x
       ["set", [x], y] -> Set x (parseAtom y)
       ["add", [x], y] -> Add x (parseAtom y)
       ["mul", [x], y] -> Mul x (parseAtom y)
       ["mod", [x], y] -> Mod x (parseAtom y)
       ["rcv", [x]] -> Rcv x
       ["jgz", x, y] -> Jgz (parseAtom x) (parseAtom y)
       _ -> error ("unknown instr " <> str)
   parseAtom [c] | c `elem` ['a'..'z'] = Reg c
   parseAtom s = Lit (read s)

data Run :: * -> * where
  Done :: a -> Run a
  Receive :: (Int -> Run a) -> Run a
  Send :: Int -> Run a -> Run a

instance Show a => Show (Run a) where
  show (Done x) = "Done " ++ show x
  show (Receive _) = "Receive <k>"
  show (Send n _) = "Send " ++ show n ++ " <k>"

instance Functor Run where
  fmap = liftM

instance Applicative Run where
  pure = return
  (<*>) = ap

instance Monad Run where
  return = Done
  (Done x) >>= f = f x
  (Receive k) >>= f = Receive (k >=> f)
  (Send n k) >>= f = Send n (k >>= f)

send :: Int -> Run ()
send i = Send i (Done ())

receive :: Run Int
receive = Receive Done

type Machine = Run ()

eval :: Input -> Int -> Map Char Int -> Machine
eval instrs pc regs | pc < 0 || pc >= length instrs = return ()
eval instrs pc regs =
  case instrs V.! pc of
    Snd r -> do
      send (valueOf r)
      eval instrs (pc+1) regs
    Set r a -> eval instrs (pc+1) (regs & at r ?~ resolve a)
    Add r a -> eval instrs (pc+1) (regs & ix r +~ resolve a)
    Mul r a -> eval instrs (pc+1) (regs & ix r *~ resolve a)
    Mod r a -> eval instrs (pc+1) (regs & ix r %~ (`mod` resolve a))
    Rcv r -> do
      v <- receive
      eval instrs (pc+1) (regs & at r ?~ v)
    Jgz x a -> eval instrs (pc + (if resolve x > 0 then resolve a else 1)) regs
 where
  valueOf c = regs M.! c
  resolve (Lit i) = i
  resolve (Reg c) = valueOf c

run :: Int -> Input -> Machine
run pval input = eval input 0 (M.fromList (zip ['a'..'z'] (repeat 0)) & at 'p' ?~ pval)

data SharedState = SharedState { queue0 :: (Int, Seq Int), queue1 :: (Int, Seq Int) }
  deriving (Eq, Show, Generic)

step :: Machine -> Lens' SharedState (Int, Seq Int) -> Lens' SharedState (Int, Seq Int) -> State SharedState (Maybe Machine)
step m@(Receive k) incoming outgoing = do
  msgIn <- use (incoming._2)
  case msgIn of
    (is :|> i) -> do
      incoming._2 .= is
      return (Just (k i))
    Empty -> return Nothing
step (Send j k) incoming outgoing = do
  outgoing %= ((+1) *** (j :<|))
  return (Just k)
step _ _ _ = return Nothing

canProgress :: Machine -> Seq Int -> Bool
canProgress (Receive _) is = not (Seq.null is)
canProgress _ _ = False

evalConcurrent :: Machine -> Machine -> State SharedState ()
evalConcurrent m0 m1 = do
  mm0' <- step m0 #queue1 #queue0
  case mm0' of
    Just m0' -> evalConcurrent m0' m1
    Nothing -> do
      mm1' <- step m1 #queue0 #queue1
      case mm1' of
        Just m1' -> evalConcurrent m0 m1'
        Nothing -> return ()

solve :: Input -> Output
solve input = execState (evalConcurrent (run 0 input) (run 1 input)) (SharedState (0, Empty) (0, Empty)) ^. #queue1 . _1
