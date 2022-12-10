module Parser where

import Libraries

type Problem = [[Int]]

parser :: Parse Problem
parser = many (digitToInt <$> numberChar) `sepBy` newline
