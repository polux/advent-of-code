-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Intcode where

import Control.Monad.Reader
import Data.Function
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Vector ((!), (//))
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector as V
import qualified Data.Map as M
import Debug.Trace (traceShow, traceShowId)

type Vec = V.Vector Integer

parseInstruction :: Integer -> (Integer, Int, Int, Int)
parseInstruction i =
    ( i `rem` 100
    , fromIntegral  ((i `div` 100) `mod` 10)
    , fromIntegral  ((i `div` 1000) `mod` 10)
    , fromIntegral  ((i `div` 10000) `mod` 10)
    )

data Run :: * -> * where
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
  pure = return
  (<*>) = ap

instance Monad Run where
  return = Done
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
  | op == 1 = run b (i+4) (vec // [(fromIntegral imm3, v1 + v2)])
  | op == 2 = run b (i+4) (vec // [(fromIntegral imm3, v1 * v2)])
  | op == 3 = do
     input <- readInput
     run b (i+2) (vec // [(fromIntegral imm1, input)])
  | op == 4 = do
     writeOutput v1
     run b (i+2) vec
  | op == 5 = run b (if v1 /= 0 then v2 else i+3) vec
  | op == 6 = run b (if v1 == 0 then v2 else i+3) vec
  | op == 7 = run b (i+4) (vec // [(fromIntegral imm3, if v1 < v2 then 1 else 0)])
  | op == 8 = run b (i+4) (vec // [(fromIntegral imm3, if v1 == v2 then 1 else 0)])
  | op == 9 = run (b+v1) (i+2) vec
  | otherwise = error ("unknown op code " ++ show op)
 where
  (op, p1, p2, p3) = parseInstruction i0
  v1 | p1 == 1 = i1
     | p1 == 2 = r1 b
     | otherwise = r1 0
  v2 | p2 == 1 = i2
     | p2 == 2 = r2 b
     | otherwise = r2 0
  v3 | p3 == 1 = i3
     | p3 == 2 = r3 b
     | otherwise = r3 0
  i0 = vec ! fromIntegral i
  i1 = vec ! fromIntegral (i+1)
  i2 = vec ! fromIntegral (i+2)
  i3 = vec ! fromIntegral (i+3)
  r1 o = vec ! fromIntegral (i1+o)
  r2 o = vec ! fromIntegral (i2+o)
  r3 o = vec ! fromIntegral (i3+o)
  imm1 | p1 == 0 = i1
       | p1 == 2 = i1 + b
  imm2 | p2 == 0 = i2
       | p2 == 2 = i2 + b
  imm3 | p3 == 0 = i3
       | p3 == 2 = i3 + b

evalList :: [Integer] -> Run a -> [Integer]
evalList is (Done x) = []
evalList [] (ReadInput _) = error "no input to read from"
evalList (i:is) (ReadInput k) = evalList is (k i)
evalList is (WriteOutput o k) = o : evalList is k

execute :: [Integer] -> Vec -> [Integer]
execute input vec = evalList input $ run 0 0 (vec <> V.replicate 10000 0)

type Pos = (Integer, Integer)
type Screen = M.Map Pos Integer

toScreen :: [Integer] -> Screen
toScreen (x:y:t:os) = M.insert (x,y) t (toScreen os)
toScreen _ = M.empty

parse :: String -> Vec
parse s = s & splitOn "," & map read & V.fromList

parseAndExecute :: String -> [Integer] -> [Integer]
parseAndExecute s input = parse s & execute input

data GameState = GameState
  { screen :: Screen
  , machine :: Machine
  , score :: Integer
  , history :: [GameState]
  , auto :: Bool
  }

update :: GameState -> GameState
update s@GameState {machine = ReadInput _, auto = True} = handle (EventKey (SpecialKey KeySpace) Down undefined undefined) s
update s@GameState {screen, machine=WriteOutput x (WriteOutput y (WriteOutput t machine'))}
  | x == -1 && y == 0 = s { score = t, machine = machine' }
  | otherwise  = update (s { screen = M.insert (x,y) t screen, machine = machine' })
update s = s

handle :: Event -> GameState -> GameState
handle (EventKey (SpecialKey KeyLeft) Down _ _) s@GameState{machine = ReadInput k, history} = s { machine = k (-1), history = s:history }
handle (EventKey (SpecialKey KeyRight) Down _ _) s@GameState{machine = ReadInput k, history} = s { machine = k 1, history = s:history }
handle (EventKey (SpecialKey KeyUp) Down _ _) s@GameState{machine = ReadInput k, history} = s { machine = k 0, history = s:history }
handle (EventKey (Char 'z') Down _ _) GameState{history =(s:_)} = s
handle (EventKey (Char 'a') Down _ _) s@GameState{auto} = s { auto = not auto }
handle (EventKey (SpecialKey KeySpace) Down _ _) s@GameState{screen, machine = ReadInput k, history} = s { machine = k sign, history = s:history }
 where sign | ballX < paddleX = -1
            | ballX > paddleX = 1
            | otherwise = 0
       ballX = M.filter (==4) screen & M.keys & head
       paddleX = M.filter (==3) screen & M.keys & head
handle _ s = s

render :: GameState -> Picture
render GameState{screen, score} = translate (-500) 300 (color white (text (show score))) <> scale 10 (-10) (mconcat (map tile (sortOn snd (M.toList screen))))
 where
  tile ((x,y), 3) = color red (translate (fromIntegral x) (fromIntegral y) (rectangleSolid 3 1))
  tile ((x,y), t) = color (colorOf t) (translate (fromIntegral x) (fromIntegral y) (rectangleSolid 1 1))
  colorOf 1 = white
  colorOf 2 = blue
  colorOf 4 = green
  colorOf _ = black

game :: Vec -> IO ()
game vec =
  play
    FullScreen
    black
    200
    initialGameState
    render
    handle
    (const update)
 where
  initialGameState = GameState { screen = M.empty, machine = initialMachine, score = 0, history = [], auto = False }
  initialMachine = run 0 0 ((vec // [(0, 2)]) <> V.replicate 500 0)
