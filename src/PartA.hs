{-# LANGUAGE RecordWildCards #-}

module PartA where

import Control.Arrow (Arrow (first))
import Control.Category ((>>>))
import Control.Lens (element, makeLenses, over, (&))
import Control.Lens.Operators ((%~))
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, insert)
import Libraries hiding (insert, lookup)
import Parser

partA :: Problem -> IO Integer
partA input = pure $ product $ take 2 $ reverse $ sort $ numInspected <$> runNTimes 20 input

runNTimes :: Int -> Problem -> Problem
runNTimes 0 jungle = jungle
runNTimes n jungle = runNTimes (n - 1) $ runSteps jungle

runSteps :: Problem -> Problem
runSteps jungle = fst $ mapAccumL (\jungle i -> (runMonkeyAt jungle i, ())) jungle [0 .. length jungle - 1]

runMonkeyAt :: Problem -> Int -> Problem
runMonkeyAt start i =
  runMonkey start (start !! i)
    & element i %~ \monkey ->
      monkey
        { items = [],
          numInspected = numInspected monkey + fromIntegral (length (items monkey))
        }

runMonkey :: Problem -> Monkey -> Problem
runMonkey jungle Monkey {items = []} = jungle
runMonkey start Monkey {items = myItems, ..} = foldl' runItem start myItems
  where
    runItem jungle item =
      let newWorry = applyOp operation item (maybe item fromIntegral rhs) `div` 3
          to = if newWorry `mod` fromIntegral divBy == 0 then ifTrue else ifFalse
       in jungle & element to %~ \monkey -> monkey {items = newWorry : items monkey}

applyOp :: Op -> Integer -> Integer -> Integer
applyOp P = (+)
applyOp M = (*)
