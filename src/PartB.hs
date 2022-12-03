module PartB where

import Libraries
import Parser
import PartA

partB :: Problem -> IO Int
partB input = return $ groups $ fmap combine input

combine = uncurry (++)

groups :: [[Char]] -> Int
groups (x : y : z : rest) = sum (scoreLetter <$> nub (x `intersect` y `intersect` z)) + groups rest
groups [] = 0
groups xs = error $ show xs
