module PartB where

import Libraries
import Parser
import PartA

partB :: Problem -> IO Int
partB input = return $ sum $ fmap (score . toRpc2) input

toRpc2 :: Fight Char -> Fight RPC
toRpc2 (Fight them winner) =
  let them' = theirMove them
      winner' = toWinner winner
   in Fight them' (whatToPick them' winner')

toWinner :: Char -> Winner
toWinner 'X' = Them
toWinner 'Y' = Draw
toWinner 'Z' = You
toWinner c = error $ "Invalid character " ++ [c]

whatToPick :: RPC -> Winner -> RPC
whatToPick Rock You = Paper
whatToPick Paper You = Scissors
whatToPick Scissors You = Rock
whatToPick Rock Them = Scissors
whatToPick Paper Them = Rock
whatToPick Scissors Them = Paper
whatToPick pick Draw = pick
