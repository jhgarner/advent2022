module Parser where

import Libraries

data RPC = Rock | Paper | Scissors
  deriving Eq

data Winner = Them | Draw | You

data Fight a = Fight a a


type Problem = [Fight Char]
parser :: ParsecT Void Text IO Problem
parser = liftA2 Fight (anySingle <* hspace) anySingle `sepEndBy` newline
