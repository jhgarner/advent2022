module PartA where

import Libraries
import Parser

partA :: Problem -> IO Int
partA input = return $ sum $ fmap handlePair input

handlePair :: (Elf, Elf) -> Int
handlePair ((start1, end1), (start2, end2))
  | start1 >= start2 && end1 <= end2 = 1
  | start2 >= start1 && end2 <= end1 = 1
  | otherwise = 0
