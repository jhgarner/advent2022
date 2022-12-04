module PartB where

import Libraries
import Parser
import PartA

partB :: Problem -> IO Int
partB input = return $ sum $ fmap anyOverlap input

anyOverlap :: (Elf, Elf) -> Int
anyOverlap ((start1, end1), (start2, end2))
  | start1 >= start2 && start1 <= end2 = 1
  | end1 >= start2 && end1 <= end2 = 1
  | start2 >= start1 && start2 <= end1 = 1
  | end2 >= start1 && end2 <= end1 = 1
  | otherwise = 0
