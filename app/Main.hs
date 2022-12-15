module Main where

import Prelude
import Data.Text.IO as T
import Lib
import Text.Megaparsec (runParserT, errorBundlePretty)

main :: IO ()
main = do
  parsedE <- T.getContents >>= runParserT parser ""
  let parsed = either (error . errorBundlePretty) id parsedE
  partA parsed >>= print
  partB parsed >>= Prelude.putStrLn
