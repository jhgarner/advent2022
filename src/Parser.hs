module Parser where

import Libraries

type Problem = [(String, String)]
parser :: ParsecT Void Text IO Problem
parser = (halves <$> some letterChar) `sepEndBy` newline

halves :: String -> (String, String)
halves ls = splitAt (length ls `div` 2) ls
