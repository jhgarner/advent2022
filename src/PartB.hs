module PartB where

import Control.Arrow (Arrow (first))
import Control.Category ((>>>))
import Control.Lens ((&))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, insert)
import Libraries hiding (insert, lookup)
import Parser
import Data.Bifunctor

partB :: Problem -> IO Int
partB input = pure $ howManyVisited $ foldl' action (fromList [(0, 0)], replicate 10 (0, 0)) input

type Position = (Int, Int)

type World = (Set Position, [Position])

howManyVisited :: World -> Int
howManyVisited (visited, _) = length visited

addPos :: World -> World
addPos (set, knots) = (insert (last knots) set, knots)

action :: World -> (Direction, Int) -> World
action world (_, 0) = world
action world (dir, n) = action (addPos $ stepIn dir world) (dir, n - 1)

stepIn :: Direction -> World -> World
stepIn dir = moveTail . moveHead dir

moveHead :: Direction -> World -> World
moveHead U (visited, (x, y):tail) = (visited, (x, y + 1):tail)
moveHead D (visited, (x, y):tail) = (visited, (x, y - 1):tail)
moveHead L (visited, (x, y):tail) = (visited, (x - 1, y):tail)
moveHead R (visited, (x, y):tail) = (visited, (x + 1, y):tail)
moveHead _ (_, _) = error "Bad"

moveTail :: World -> World
moveTail (visited, []) = error "bad"
moveTail (visited, [x]) = (visited, [x])
moveTail (visited, head:tail:rest) = (head:) <$> moveTail (visited, approach tail head:rest)

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
