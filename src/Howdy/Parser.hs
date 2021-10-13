{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Howdy.Parser (Parser, runParser, rest, word, char, chars, ws, string, anyString) where

import           Control.Applicative (Alternative (empty), many, (<|>))
import           Control.Monad       (join)
import           Data.Text.Lazy      (Text, pack, singleton, uncons)
import qualified Data.Text.Lazy      as T (empty)
import           Debug.Trace         (trace)
import           Howdy.Parser.Types  (Parser (Parser), runParser)
import           UnliftIO            (MonadIO (liftIO))

anyChar :: Parser Char
anyChar = Parser uncons
  -- (uncons -> Nothing)      -> Nothing
  -- (uncons -> Just (x, xs)) -> Just (x, xs)

char :: Char -> Parser Char
char c = do
  c' <- anyChar
  if c == c' then return c
  else empty

chars :: [Char] -> Parser Char
chars [] = empty
chars (c:cs) = do
  char c <|> chars cs

notChar :: Char -> Parser Char
notChar c = do
  c' <- anyChar
  if c /= c' then return c'
  else empty

string :: Text -> Parser Text
string (uncons -> Nothing) = empty
string (uncons -> Just (x, xs)) =
  (singleton <$> char x) <> (string xs <|> pure "")

anyString :: Char -> Parser Text
anyString c =
  (singleton <$> notChar c) <> (anyString c <|> pure "")

word :: Parser Text
word = ws $ anyString ' '

rest :: Parser Text
rest = Parser $ \s -> Just (s, "")

ws :: Parser Text -> Parser Text
ws p = many s *> p <* many s
  where s = char ' '
