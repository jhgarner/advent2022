module PartA where

import Control.Arrow (Arrow (first))
import Control.Category ((>>>))
import Control.Lens ((&))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, insert)
import Libraries hiding (insert, lookup)
import Parser
import Data.Bifunctor

partA :: Problem -> IO Int
partA input = pure $ howManyVisited $ foldl' action (fromList [(0, 0)], (0, 0), (0, 0)) input

type Position = (Int, Int)

type World = (Set Position, Position, Position)

howManyVisited :: World -> Int
howManyVisited (visited, _, _) = length visited

addPos :: World -> World
addPos (set, head, loc) = (insert loc set, head, loc)

action :: World -> (Direction, Int) -> World
action world (_, 0) = world
action world (dir, n) = action (addPos $ stepIn dir world) (dir, n - 1)

stepIn :: Direction -> World -> World
stepIn dir = moveTail . moveHead dir

moveHead :: Direction -> World -> World
moveHead U (visited, (x, y), tail) = (visited, (x, y + 1), tail)
moveHead D (visited, (x, y), tail) = (visited, (x, y - 1), tail)
moveHead L (visited, (x, y), tail) = (visited, (x - 1, y), tail)
moveHead R (visited, (x, y), tail) = (visited, (x + 1, y), tail)

moveTail :: World -> World
moveTail (visited, head, tail) = (visited, head, approach tail head)

dir :: Int -> Int -> Int
dir a b
  | a < b = a + 1
  | a > b = a - 1
  | otherwise = a

approach :: Position -> Position -> Position
approach tail head
  | dist tail head >= 2 = bimap (dir (fst tail)) (dir (snd tail)) head
  | otherwise = tail

dist :: Position -> Position -> Float
dist (a, b) (x, y) =
  sqrt $ (fromIntegral a - fromIntegral x) ** 2 + (fromIntegral b - fromIntegral y) ** 2
