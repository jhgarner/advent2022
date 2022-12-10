module PartA where

import Libraries hiding (lookup)
import Parser
import Data.Map (Map, fromList, elems, lookup, alterF)
import Control.Arrow (Arrow(first))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Control.Category ((>>>))
import Control.Lens ((&))

partA :: Problem -> IO (Sum Int)
partA input = pure $ numVisible $ traceShowId $ checkSides $ starting input

numVisible :: [[(Bool, Int)]] -> Sum Int
numVisible = foldMap (foldMap $ Sum . fromEnum . fst)


starting :: Problem -> [[(Bool, Int)]]
starting = fmap (fmap (False,))

checkSides :: [[(Bool, Int)]] ->[[(Bool, Int)]]
checkSides input = get . fmap reverse $ get . transpose . fmap reverse $ get . fmap reverse $ get input

get :: [[(Bool, Int)]] -> [[(Bool, Int)]]
get = fmap getRow

getRow :: [(Bool, Int)] -> [(Bool, Int)]
getRow = snd . mapAccumL isVisible (-1)


-- get :: [[Int]] -> Sum Int
-- get = foldMap' getRow

-- getRow :: [Int] -> Sum Int
-- getRow = snd . foldl' visible (-1, 0)

isVisible :: Int -> (Bool, Int) -> (Int, (Bool, Int))
isVisible tallest (visible, height) =
  if height > tallest
     then (height, (True, height))
     else (tallest, (visible, height))
