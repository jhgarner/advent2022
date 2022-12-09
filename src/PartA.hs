{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module PartA where

import Libraries hiding (lookup)
import Parser
import Data.Map (Map, fromList, elems, lookup, alterF)
import Control.Arrow (Arrow(first))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)

data DirTree = Item Int | Dir (Map String DirTree)
  deriving (Show)
makeBaseFunctor ''DirTree

partA :: Problem -> IO Int
partA input = pure $ coerce $ mconcat $ findSmallDirs $ execute input ([], Dir mempty)



type Machine = ([String], DirTree)

execute :: Problem -> Machine -> DirTree
execute [] machine = snd machine
execute (CdRoot:rest) machine = execute rest $ first (const []) machine
execute (CdOut:rest) machine = execute rest $ first tail machine
execute (Cd to:rest) machine = execute rest $ first (to:) machine
execute (Ls files:rest) machine = execute rest $ addToTree (first reverse machine) files

addToTree :: Machine -> [File] -> Machine
addToTree ([], tree) files = ([], Dir $ fromList $ toListOfFiles files)
addToTree (l:ls, tree) files = first (++ [l]) $ getOrCreate tree l \tree -> addToTree (ls, tree) files

getOrCreate :: DirTree -> String -> (DirTree -> Machine) -> Machine
getOrCreate (Item _) _ _ = error "Got file"
getOrCreate (Dir items) to f =
  Dir <$> alterF (fmap Just . f . fromMaybe (Dir mempty)) to items

toListOfFiles :: [File] -> [(String, DirTree)]
toListOfFiles = fmap \case
  File size name -> (name, Item size)
  Directory name -> (name, Dir mempty)


findSmallDirs :: DirTree -> [Sum Int]
findSmallDirs = fst . cata \case
  ItemF size -> ([], Sum size)
  DirF items ->
    let (output, size) = mconcat $ elems items
     in (if size <= 100000 then size:output else output, size)
