{-# LANGUAGE RecordWildCards #-}
module PartB where

import Libraries
import Parser
import PartA
import Control.Lens.Operators
import Control.Lens.Combinators

partB :: Problem -> IO String
partB (start, moves) = do 
  print $ unlines start
  return $ head <$> foldl' (flip actB) start moves

actB :: Move -> Docks -> Docks
actB Move{times = 0} docks = docks
actB move@Move{..} docks =
  let (top, rest) = times `splitAt` (docks !! from)
   in docks & element from .~ rest & element to %~ (top ++)
