module PartB where

import Libraries
import Parser
import PartA
import Data.Coerce (coerce)
import Data.Map (elems)
import Data.Maybe (fromJust)

partB :: Problem -> IO (Maybe (Min Int))
partB input =
  let tree = execute input ([], Dir mempty)
   in pure $ findFreeable (total tree) tree

total :: DirTree -> Sum Int
total = cata \case
  ItemF size -> Sum size
  DirF items -> mconcat $ elems items

findFreeable :: Sum Int -> DirTree -> Maybe (Min Int)
findFreeable total = fst . cata \case
  ItemF size -> (Nothing, Sum size)
  DirF items ->
    let (output, size) = mconcat $ elems items
     in (if size >= (30000000 - 70000000 + total) then Just (coerce size) <> output else output, size)
