module PartA where

import Libraries
import Parser

partA :: Problem -> IO Int
partA input = return $ sum $ fmap score input

score :: (String, String) -> Int
score (left, right) = sum $ fmap scoreLetter $ nub $ left `intersect` right

scoreLetter :: Char -> Int
scoreLetter c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1
