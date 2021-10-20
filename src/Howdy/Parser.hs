{-# LANGUAGE ViewPatterns #-}

module Howdy.Parser ( module Howdy.Parser.Types
                    , string
                    , word
                    , firstof
                    ) where

import Howdy.Parser.Types
import qualified Data.Text as T
import Control.Monad (guard)
import Control.Applicative (Alternative(empty, (<|>), many))
import Data.Text (Text)

char :: Char -> Parser Char
char c = Parser go
    where go (T.uncons -> Just (x, xs)) = guard (x == c) >> Just (x, xs)
          go _                          = Nothing

notChar :: Char -> Parser Char
notChar c = Parser go
    where go (T.uncons -> Just (x, xs)) = guard (x /= c) >> Just (x, xs)
          go _                          = Nothing

anyChar :: Parser Char
anyChar = Parser go
    where go (T.uncons -> Just (x, xs)) = Just (x, xs)
          go _                          = Nothing

chars :: [Char] -> Parser Char
chars [] = empty
chars (c:cs) = char c <|> chars cs

string :: Text -> Parser Text
string (T.uncons -> Nothing)      = pure ""
string (T.uncons -> Just (x, xs)) = do
    h <- char x
    s <- string xs
    pure $ T.singleton h <> s

text :: Char -> Parser Text
text c = T.pack <$> many (notChar c)

word :: Parser Text
word = many (char ' ') >> text ' '

flag :: Parser Text
flag = string "--" >> text ' '

firstof :: (a -> Parser b) -> [a] -> Parser b
firstof f = foldr ((<|>) . f) empty