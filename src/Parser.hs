module Parser where

import Libraries

type Problem = String

parser :: ParsecT Void Text IO Problem
parser = many asciiChar
