{-# LANGUAGE RecordWildCards #-}
module Parser where

import Libraries
import Data.Maybe (catMaybes)

type Docks = [[Char]]

type Problem = (Docks, [Move])

data Move = Move
  { times :: Int,
    from :: Int,
    to :: Int
  }

parser :: ParsecT Void Text IO Problem
parser = do
  docks <- dockStart
  (hspace >> decimal >> hspace) `sepBy` hspace
  newline >> newline
  moves <- steps
  pure (docks, moves)

crate :: ParsecT Void Text IO Char
crate = char '[' *> letterChar <* char ']'

air :: ParsecT Void Text IO (Maybe a)
air = string "   " $> Nothing

crateOrAir :: ParsecT Void Text IO (Maybe Char)
crateOrAir = air <|> fmap Just crate

rowOfCrates :: ParsecT Void Text IO [Maybe Char]
rowOfCrates = crateOrAir `sepBy` char ' '

rowsOfCrates :: ParsecT Void Text IO [[Maybe Char]]
rowsOfCrates = rowOfCrates `sepBy` newline

dockStart :: ParsecT Void Text IO [[Char]]
dockStart = fmap catMaybes . transpose <$> rowsOfCrates


steps :: ParsecT Void Text IO [Move]
steps = step `sepEndBy` newline

step :: ParsecT Void Text IO Move
step = do
  string "move "
  times <- decimal
  string " from "
  from <- fmap pred decimal
  string " to "
  to <- fmap pred decimal
  pure Move {..}
