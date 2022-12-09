module Parser where

import Libraries

data File = Directory String | File Int String
  deriving (Show)
data Command = CdRoot | CdOut | Cd String | Ls [File]
  deriving (Show)

type Problem = [Command]

parser :: Parse Problem
parser = choice [cdRootCommand, cdOutCommand, cdCommand, lsCommand] `sepEndBy` newline

cdRootCommand :: Parse Command
cdRootCommand = string "$ cd /" $> CdRoot

cdOutCommand :: Parse Command
cdOutCommand = string "$ cd .." $> CdOut

cdCommand :: Parse Command
cdCommand = fmap Cd $ string "$ cd " >> name

lsCommand :: Parse Command
lsCommand = do
  string "$ ls"
  newline
  Ls <$> choice [dir, file] `trySepBy` try newline

dir :: Parse File
dir = fmap Directory $ string "dir " >> name

file :: Parse File
file = File <$> decimal <*> (hspace *> name)

name :: Parse String
name = many (anySingleBut '\n')

trySepBy :: Parse a -> Parse sep -> Parse [a]
trySepBy p sep = do
  r <- optional p
  case r of
    Nothing -> return []
    Just  x -> (x:) <$> many (try $ sep >> p)
