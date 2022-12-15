module Libraries (traceShowId, traceWithContext, Parse, num, module All) where

import Control.Applicative as All (liftA2)
import Control.Comonad as All
import Control.Zipper as All
import Data.Char as All
import Data.Foldable as All
import Data.Functor.Foldable as All hiding (fold)
import Data.Functor.Foldable.TH as All
import Data.List as All
import Data.Ord as All
import Data.Semigroup as All hiding (option)
import Data.Text as All (Text)
import Data.Void as All
import Text.Megaparsec as All
import Text.Megaparsec.Char as All hiding (space)
import Text.Megaparsec.Char.Lexer as All
import Prelude as All
import Debug.Trace (trace)

traceShowId :: Show a => a -> a
traceShowId a = trace (show a ++ "\n") a

traceWithContext :: (Show a, Show b) => b -> a -> a
traceWithContext b a = trace (show (a, b) ++ "\n") a

type Parse a = ParsecT Void Text IO a

negative :: Parse Int
negative = char '-' >> fmap negate decimal

num :: Parse Int
num = negative <|> decimal
