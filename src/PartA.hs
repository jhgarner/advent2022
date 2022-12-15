{-# LANGUAGE RecordWildCards #-}

module PartA where

import Control.Arrow (Arrow (first))
import Control.Category ((>>>))
import Control.Lens (makeLenses, over, (&))
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, insert)
import Libraries hiding (insert, lookup)
import Parser

partA :: Problem -> IO Int
partA input = pure $ combine signalsToCheck $ checkAt signalsToCheck $ initMachine $ traceShowId input

combine :: [Int] -> [Int] -> Int
combine times registers = sum $ zipWith (*) registers $ totals times

totals :: [Int] -> [Int]
totals = snd . mapAccumL (\at x -> (at+x, at+x)) 0

checkAt :: [Int] -> Machine -> [Int]
checkAt [] _ = []
checkAt (0 : rest) machine = register machine : checkAt rest machine
checkAt toCheck machine = checkAt (approach toCheck) $ stepMachine machine

approach :: [Int] -> [Int]
approach [] = error "Bad thing"
approach (x : xs) = x - 1 : xs

signalsToCheck :: [Int]
signalsToCheck = [20, 40, 40, 40, 40, 40]

data Machine = Machine
  { register :: Int,
    remaining :: Int,
    op :: Op,
    code :: [Op]
  }
  deriving (Show)

initMachine :: [Op] -> Machine
initMachine code = Machine {register = 1, remaining = 0, op = Noop, code = code ++ repeat Noop}

stepMachine :: Machine -> Machine
stepMachine machine@Machine {..}
  | remaining == 0 = Machine {register = opOnRegister op register, remaining = timeToRun $ head code, op = head code, code = tail code}
  | otherwise = machine {remaining = remaining - 1}

opOnRegister :: Op -> Int -> Int
opOnRegister Noop r = r
opOnRegister (Addx x) r = x + r

timeToRun :: Op -> Int
timeToRun Noop = 0
timeToRun (Addx _) = 1
