module Parser where

import Libraries

data Op = Noop | Addx Int
  deriving (Show)

type Problem = [Op]

parser :: Parse Problem
parser = line `sepEndBy` newline

line :: Parse Op
line = noop <|> addx

noop :: Parse Op
noop = string "noop" $> Noop

addx :: Parse Op
addx = fmap Addx $ string "addx" >> hspace >> num
