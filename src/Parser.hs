{-# LANGUAGE RecordWildCards #-}

module Parser where

import Libraries

data Op = P | M
  deriving (Show)

data Monkey = Monkey
  { items :: [Integer],
    operation :: Op,
    rhs :: Maybe Int,
    divBy :: Int,
    ifTrue :: Int,
    ifFalse :: Int,
    numInspected :: Integer
  }
  deriving (Show)

type Problem = [Monkey]

parser :: Parse Problem
parser = monkey `sepEndBy` many newline

monkey :: Parse Monkey
monkey = do
  let numInspected = 0
  string "Monkey " >> decimal >> string ":" >> space1
  items <- string "Starting items: " >> num `sepBy` string ", "
  operation <- space1 >> string "Operation: new = old " >> op
  rhs <- hspace >> choice [Just <$> num, string "old" $> Nothing]
  divBy <- space1 >> string "Test: divisible by " >> num
  ifTrue <- space1 >> "If true: throw to monkey " >> num
  ifFalse <- space1 >> "If false: throw to monkey " >> num
  pure Monkey {..}

op :: Parse Op
op = choice [char '+' $> P, char '*' $> M]


