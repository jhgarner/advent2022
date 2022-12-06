module PartA where

import Libraries
import Parser

partA :: Problem -> IO Int
partA (a:b:c:d:rest)
  | nub [a, b, c, d] == [a, b, c ,d] = return 4
  | otherwise = succ <$> partA (b:c:d:rest)
partA _ = undefined
