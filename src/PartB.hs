module PartB where

import Libraries
import Parser
import PartA
import Data.Coerce (coerce)
import Data.Map (elems, mapWithKey)
import Data.Maybe (fromJust)
import Control.Arrow (Arrow(first))

partB :: Problem -> IO (Maybe MinName)
partB input =
  let tree = execute input ([], Dir mempty)
   in pure $ findFreeable (total tree) tree

total :: DirTree -> Sum Int
total = cata \case
  ItemF size -> Sum size
  DirF items -> mconcat $ elems items

findFreeable :: Sum Int -> DirTree -> Maybe MinName
findFreeable total = fst . cata \case
  ItemF size -> (Nothing, Sum size)
  DirF items ->
    let (output, size) = mconcat $ elems $ mapWithKey (\k v -> first (fmap \(MinName (name, value)) -> MinName (k ++ name, value)) v) items
     in (if size >= (total - 30000000) then Just (MinName ("", coerce size)) <> output else output, size)

newtype MinName = MinName (String, Int)
  deriving (Show)

instance Semigroup MinName where
  MinName (na, a) <> MinName (nb, b) = coerce if a < b then (na, a) else (nb, b)

-- newtype SmallestBigEnough = SBE (Maybe Int)

-- instance Semigroup SmallestBigEnough where
--   SBE Nothing <> a = a
--   a <> SBE Nothing = a
--   SBE (Just a) <> SBE (Just b) = SBE $ Just $ min a b
