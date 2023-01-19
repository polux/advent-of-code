{-# LANGUAGE FlexibleContexts #-}
-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Intcode where

import Control.Monad.Reader
import Data.Function
import Data.Kind (Type)
import Data.List (sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import Debug.Trace (traceShow, traceShowId)
import Graphics.Gloss.Interface.Pure.Game

type Vec = V.Vector Integer

parseInstruction :: Integer -> (Integer, Int, Int, Int)
parseInstruction i =
  ( i `rem` 100
  , fromIntegral ((i `div` 100) `mod` 10)
  , fromIntegral ((i `div` 1000) `mod` 10)
  , fromIntegral ((i `div` 10000) `mod` 10)
  )

data Run :: Type -> Type where
  Done :: a -> Run a
  ReadInput :: (Integer -> Run a) -> Run a
  WriteOutput :: Integer -> Run a -> Run a

instance Show a => Show (Run a) where
  show (Done x) = "Done " ++ show x
  show (ReadInput _) = "ReadInput <k>"
  show (WriteOutput n _) = "WriteOutput " ++ show n ++ " <k>"

instance Functor Run where
  fmap = liftM

instance Applicative Run where
  pure = Done
  (<*>) = ap

instance Monad Run where
  (Done x) >>= f = f x
  (ReadInput k) >>= f = ReadInput (k >=> f)
  (WriteOutput n k) >>= f = WriteOutput n (k >>= f)

writeOutput :: Integer -> Run ()
writeOutput i = WriteOutput i (Done ())

readInput :: Run Integer
readInput = ReadInput Done

type Machine = Run ()

run :: Integer -> Integer -> Vec -> Machine
run b i vec
  | op == 99 = return ()
  | op == 1 = run b (i + 4) (vec // [(fromIntegral i3', v1 + v2)])
  | op == 2 = run b (i + 4) (vec // [(fromIntegral i3', v1 * v2)])
  | op == 3 = do
      input <- readInput
      run b (i + 2) (vec // [(fromIntegral i1, input)])
  | op == 4 = do
      writeOutput v1
      run b (i + 2) vec
  | op == 5 = run b (if v1 /= 0 then v2 else i + 3) vec
  | op == 6 = run b (if v1 == 0 then v2 else i + 3) vec
  | op == 7 = run b (i + 4) (vec // [(fromIntegral i3', if v1 < v2 then 1 else 0)])
  | op == 8 = run b (i + 4) (vec // [(fromIntegral i3', if v1 == v2 then 1 else 0)])
  | op == 9 = run (b + v1) (i + 2) vec
  | otherwise = error ("unknown op code " ++ show op)
 where
  (op, p1, p2, p3) = parseInstruction (vec ! fromIntegral i)
  i1 = vec ! fromIntegral (i + 1)
  i2 = vec ! fromIntegral (i + 2)
  i3 = vec ! fromIntegral (i + 3)
  i1' = i1 + if p1 == 0 then 0 else b
  i2' = i2 + if p2 == 0 then 0 else b
  i3' = i3 + if p3 == 0 then 0 else b
  v1
    | p1 == 1 = i1
    | otherwise = vec ! fromIntegral i1'
  v2
    | p2 == 1 = i2
    | otherwise = vec ! fromIntegral i2'

evalList :: [Integer] -> Run a -> [Integer]
evalList _ (Done _) = []
evalList [] (ReadInput _) = error "no input to read from"
evalList (i : is) (ReadInput k) = evalList is (k i)
evalList is (WriteOutput o k) = o : evalList is k

execute :: [Integer] -> Vec -> [Integer]
execute input vec = evalList input $ run 0 0 (vec <> V.replicate 10000 0)

type Pos = (Integer, Integer)
type Screen = M.Map Pos Integer

toScreen :: [Integer] -> Screen
toScreen (x : y : t : os) = M.insert (x, y) t (toScreen os)
toScreen _ = M.empty

parse :: String -> Vec
parse s = s & splitOn "," & map read & V.fromList

parseAndExecute :: String -> [Integer] -> [Integer]
parseAndExecute s input = parse s & execute input

data GameState = G
  { screen :: Screen
  , machine :: Machine
  , score :: Integer
  }

data AppState = A
  { gameState :: GameState
  , history :: [GameState]
  , auto :: Bool
  }

update :: AppState -> AppState
update s@A{gameState = G{machine = ReadInput _}, auto = True} =
  handle (EventKey (SpecialKey KeySpace) Down undefined undefined) s
update s@A{gameState = g@G{screen, machine = WriteOutput x (WriteOutput y (WriteOutput t machine'))}}
  | x == -1 && y == 0 = s{gameState = g{score = t, machine = machine'}}
  | otherwise = update (s{gameState = g{screen = M.insert (x, y) t screen, machine = machine'}})
update s = s

handle :: Event -> AppState -> AppState
handle
  (EventKey (SpecialKey KeyLeft) Down _ _)
  s@A{gameState = g@G{machine = ReadInput k}, history} =
    s{gameState = g{machine = k (-1)}, history = g : history}
handle
  (EventKey (SpecialKey KeyRight) Down _ _)
  s@A{gameState = g@G{machine = ReadInput k}, history} =
    s{gameState = g{machine = k 1}, history = g : history}
handle
  (EventKey (SpecialKey KeyUp) Down _ _)
  s@A{gameState = g@G{machine = ReadInput k}, history} =
    s{gameState = g{machine = k 0}, history = g : history}
handle
  (EventKey (Char 'z') Down _ _)
  s@A{history = g : gs} =
    s{gameState = g, history = gs}
handle
  (EventKey (Char 'a') Down _ _)
  s@A{auto} =
    s{auto = not auto}
handle
  (EventKey (SpecialKey KeySpace) Down _ _)
  s@A{gameState = g@G{screen, machine = ReadInput k}, history} =
    s{gameState = g{machine = k sign}, history = g : history}
   where
    sign
      | ballX < paddleX = -1
      | ballX > paddleX = 1
      | otherwise = 0
    ballX = M.filter (== 4) screen & M.keys & head
    paddleX = M.filter (== 3) screen & M.keys & head
handle _ s = s

render :: AppState -> Picture
render A{gameState = G{screen, score}} =
  mconcat
    [ translate 0 20 (scale 0.2 0.2 (color white (text (show score))))
    , scale 10 (-10) (mconcat (map tile (sortOn snd (M.toList screen))))
    ]
 where
  tile ((x, y), t) = color (colorOf t) (translate (fromIntegral x) (fromIntegral y) (rectangleSolid 1 1))
  colorOf 1 = white
  colorOf 2 = blue
  colorOf 3 = red
  colorOf 4 = green
  colorOf _ = black

game :: Vec -> IO ()
game vec =
  play
    FullScreen
    black
    200
    initialAppState
    render
    handle
    (const update)
 where
  initialAppState = A{gameState = G{screen = M.empty, machine = initialMachine, score = 0}, history = [], auto = False}
  initialMachine = run 0 0 ((vec // [(0, 2)]) <> V.replicate 20 0)
