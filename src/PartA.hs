{-# LANGUAGE RecordWildCards #-}
module PartA where

import Libraries
import Parser
import Control.Lens (element, (%~), (&), (.~))

partA :: Problem -> IO String
partA (start, moves) = do 
  print $ unlines start
  return $ head <$> foldl' (flip act) start moves

act :: Move -> Docks -> Docks
act Move{times = 0} docks = docks
act move@Move{..} docks = act move{times = times-1} $ switch from to docks

switch :: Int -> Int -> Docks -> Docks
switch from to docks =
  let (top:rest) = docks !! from
   in docks & element from .~ rest & element to %~ (top :)
