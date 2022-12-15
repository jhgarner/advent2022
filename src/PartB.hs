{-# LANGUAGE RecordWildCards #-}

module PartB where

import Control.Arrow (Arrow (first))
import Control.Category ((>>>))
import Control.Lens (makeLenses, over, (&))
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, insert)
import Libraries hiding (insert, lookup)
import Parser
import Data.List.Split (chunksOf)

partB :: Problem -> IO String
partB input = pure $ drawCrt $ toAscii <$> runCrt (initMachine input) initCrt

drawCrt :: String -> String
drawCrt drawn =
  unlines $ chunksOf 40 drawn

toAscii :: Bool -> Char
toAscii True = '#'
toAscii False = '.'

data Machine = Machine
  { register :: Int,
    remaining :: Int,
    op :: Op,
    code :: [Op]
  }
  deriving (Show)

data Crt = Crt [Bool] Int

initCrt :: Crt
initCrt = Crt [] 0

runCrt :: Machine -> Crt -> [Bool]
runCrt _ (Crt drawn 240) = reverse drawn
runCrt machine (Crt drawn toDraw) =
  let newMachine = stepMachine machine
   in runCrt newMachine $
    Crt (isDrawn newMachine toDraw : drawn) (toDraw+1)

isDrawn :: Machine -> Int -> Bool
isDrawn Machine {..} at = abs (register - (at `mod` 40)) <= 1

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
