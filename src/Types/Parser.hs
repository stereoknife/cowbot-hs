module Types.Parser where

import           Data.Text     (Text)
import           Parser.Parser (Parser)

class Monad m => Par m where
  par :: Parser a -> m (Maybe (a, Text))
