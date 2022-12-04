module Parser where

import Libraries

type Elf = (Int, Int)

type Problem = [(Elf, Elf)]

parser :: ParsecT Void Text IO Problem
parser = pair `sepEndBy` newline

pair :: ParsecT Void Text IO (Elf, Elf)
pair = do
  first <- elf
  char ','
  second <- elf
  return (first, second)

elf :: ParsecT Void Text IO Elf
elf = do
  start <- decimal
  char '-'
  end <- decimal
  return (start, end)

halves :: String -> (String, String)
halves ls = splitAt (length ls `div` 2) ls
