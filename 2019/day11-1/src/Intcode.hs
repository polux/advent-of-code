-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Intcode where

import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
--import Data.Array.IO
import Data.Function
import Data.List
import Data.List.Split (splitOn)
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import qualified Data.Map as M
import Debug.Trace (traceShow)

type Vec = V.Vector Integer

parseInstruction :: Integer -> (Integer, Int, Int, Int)
parseInstruction i =
    ( i `rem` 100
    , fromIntegral  ((i `div` 100) `mod` 10)
    , fromIntegral  ((i `div` 1000) `mod` 10)
    , fromIntegral  ((i `div` 10000) `mod` 10)
    )

class Monad m => MonadRun m where
   withInput :: (Integer -> m a) -> m a
   writeOutput :: Integer -> m()

run :: MonadRun m => Integer -> Integer -> Vec -> m ()
run b i vec
  | op == 99 = return ()
  | op == 1 = run b (i+4) (vec // [(fromIntegral imm3, v1 + v2)])
  | op == 2 = run b (i+4) (vec // [(fromIntegral imm3, v1 * v2)])
  | op == 3 = withInput $ \input ->
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

newtype RunList a = RunList { unRunList :: ReaderT [Integer] (Writer [Integer]) a }
  deriving (Functor, Applicative, Monad, MonadReader [Integer], MonadWriter [Integer])

instance MonadRun RunList where
  withInput f = do
    input <- asks head
    local tail (f input)
  writeOutput i = tell [i]

execute :: [Integer] -> Vec -> [Integer]
execute input vec = execWriter $ flip runReaderT input $ unRunList $ run 0 0 (vec <> V.replicate 10000 0)

data Direction = N | W | E | S
  deriving (Eq, Show)

type Position = (Int, Int)

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

turnLeft :: Direction -> Direction
turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N

turn :: Integer -> Direction -> Direction
turn 0 dir = turnLeft dir
turn 1 dir = turnRight dir
turn _ dir = error "invalid direction"

move :: Direction -> Position -> Position
move N (x, y) = (x, y-1)
move E (x, y) = (x+1, y)
move S (x, y) = (x, y+1)
move W (x, y) = (x-1, y)

data RobotState = WaitingColor | WaitingDirection

data World = World
   { position :: Position
   , direction :: Direction
   , grid :: M.Map Position Integer
   , robotState :: RobotState
   }

newtype RunRobot a = RunRobot { unRunRobot :: State World a }
  deriving (Functor, Applicative, Monad, MonadState World)

instance MonadRun RunRobot where
  withInput f = do
   World{position, grid} <- get
   f (M.findWithDefault 0 position grid)

  writeOutput i = do
   w@World{position, direction, grid, robotState} <- get
   case robotState of
     WaitingColor ->
      put w { grid = M.insert position i grid, robotState = WaitingDirection }
     WaitingDirection ->
      let
        newDir = turn i direction
        newPos = move newDir position
      in
        put (w { position = newPos, direction = newDir, robotState = WaitingColor })

paint :: Vec -> M.Map Position Integer
paint vec = grid $ flip execState initialState $ unRunRobot $ run 0 0 (vec <> V.replicate 10000 0)
 where
  initialState = World { position = (0,0), direction = N, grid = M.empty, robotState = WaitingColor }

parse :: String -> Vec
parse s = s & splitOn "," & map read & V.fromList

parseAndExecute :: String -> [Integer] -> [Integer]
parseAndExecute s input = parse s & execute input