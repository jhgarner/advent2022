module Lib
  ( parser,
    partA,
    partB,
  )
where

import Libraries

type Problem = [[Int]]

parser :: ParsecT Void Text IO Problem
parser = (decimal `sepEndBy` newline) `sepBy` newline

partA :: Problem -> IO Int
partA input = return $ maximum $ fmap sum input

partB :: Problem -> IO Int
partB input = return $ sum $ take 3 $ sortOn Down $ fmap sum input
