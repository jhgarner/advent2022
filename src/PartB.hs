module PartB where

import Libraries
import Parser
import PartA

partB :: Problem -> IO Int
partB input
  | nub (take 14 input) == take 14 input = return 14
  | otherwise = succ <$> partB (tail input)
