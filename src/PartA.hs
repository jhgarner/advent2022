module PartA where

import Libraries
import Parser

partA :: Problem -> IO Int
partA input = do
  let result = sum $ fmap (score . toRpc) input
  return result

score :: Fight RPC -> Int
score fight@(Fight _ you) = choiceScore you + outcomeScore (outcome fight)

choiceScore :: RPC -> Int
choiceScore Rock = 1
choiceScore Paper = 2
choiceScore Scissors = 3

outcomeScore :: Winner -> Int
outcomeScore Them = 0
outcomeScore You = 6
outcomeScore Draw = 3

outcome :: Fight RPC -> Winner
outcome (Fight Rock Scissors) = Them
outcome (Fight Paper Rock) = Them
outcome (Fight Scissors Paper) = Them
outcome (Fight a b)
  | a == b = Draw
  | otherwise = You

toRpc :: Fight Char -> Fight RPC
toRpc (Fight them you) = Fight (theirMove them) (yourMove you)

theirMove :: Char -> RPC
theirMove 'A' = Rock
theirMove 'B' = Paper
theirMove 'C' = Scissors
theirMove c = error $ "Invalid character " ++ [c]

yourMove :: Char -> RPC
yourMove 'X' = Rock
yourMove 'Y' = Paper
yourMove 'Z' = Scissors
yourMove c = error $ "Invalid character " ++ [c]
