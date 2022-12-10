module PartB where

import Libraries
import Parser
-- import PartA
import Data.Coerce (coerce)
import Data.Map (elems)
import Data.Maybe (fromJust)

partB :: Problem -> IO (Max Int)
partB input = pure $ mconcat $ mconcat $ checkAllTrees input


checkAllTrees :: [[Int]] -> [[Max Int]]
checkAllTrees grid = (\(y, row) -> (\(x, _) -> coerce $ checkDirs grid (x, y)) <$> zip [0..] row) <$> zip [0..] grid


type Dir = (Int, Int) -> (Int, Int)
up :: Dir
up (x, y) = (x, y+1)
down :: Dir
down (x, y) = (x, y-1)
right :: Dir
right (x, y) = (x+1, y)
left :: Dir
left (x, y) = (x-1, y)

dirs :: [Dir]
dirs = [up, down, left, right]

checkDirs :: [[Int]] -> (Int, Int) -> Product Int
checkDirs grid loc =
  let height = fromJust $ grid `at` loc
   in foldMap (checkDir height loc grid) dirs

checkDir :: Int -> (Int, Int) -> [[Int]] -> Dir -> Product Int
checkDir height loc grid dir =
  case grid `at` dir loc of
    Just target ->
      if target >= height
         then 1
         else 1 + checkDir height (dir loc) grid dir
    Nothing -> 0

at :: [[Int]] -> (Int, Int) -> Maybe Int
at grid (x, y) = grid `atRow` y >>= (`atRow` x)

atRow :: [a]  -> Int -> Maybe a
atRow row x
  | x < 0 || x >= length row = Nothing
  | otherwise = Just $ row !! x


-- isVisible :: (Int -> (Bool, Int) -> (Int, (Bool, Int))
-- isVisible tallest (visible, height) =
--   if height > tallest
--      then (height, (True, height))
--      else (tallest, (visible, height))
