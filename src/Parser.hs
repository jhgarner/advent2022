module Parser where

import Libraries

data Direction = R | U | L | D
type Problem = [(Direction, Int)]

parser :: Parse Problem
parser = line `sepEndBy` newline

line :: Parse (Direction, Int)
line = (,) <$> direction <*> (hspace *> decimal)

direction :: Parse Direction
direction = choice [char 'R' $> R, char 'U' $> U, char 'D' $> D, char 'L' $> L]
