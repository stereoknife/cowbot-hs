{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Parser ( Parser (..)
              , CommandData (..)
              , command
              ) where

import Data.Char
import Control.Applicative ( Alternative (..))
import Control.Monad (guard, when)
import Control.Applicative.Combinators
import Data.Text (Text)
import qualified Data.Text as T

data CommandData = CommandData { prefix :: Text
                               , name   :: Text
                               , args   :: [Text]
                               } deriving (Show)

newtype Parser a = Parser { parse :: Text -> Maybe (a, Text) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (v, rest) <- p s
    return (f v, rest)

instance Applicative Parser where
  pure v = Parser $ \s -> Just (v, s)
  pf <*> p = Parser $ \s -> do
    (f, s') <- parse pf s
    parse (f <$> p) s'

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s ->
    parse p1 s <|> parse p2 s

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \s ->
    case parse p s of Just (p', s') -> parse (f p') s'
                      Nothing -> Nothing

instance Semigroup a => Semigroup (Parser a) where
  p1 <> p2 = Parser $ \s ->
    parse p1 s <> parse p2 s

instance Semigroup a => Monoid (Parser a) where
  mempty = empty

provided :: Parser a -> (a -> Bool) -> Parser a
p `provided` f = Parser $ \s -> do
  (v, rest) <- parse p s
  guard $ f v
  return (v, rest)

-- -----------------
-- Parser generators
-- -----------------
char :: Char -> Parser Char
char c = anyChar `provided` (==) c

anyChar :: Parser Char
anyChar = Parser $ \s -> if
  | s == T.empty -> Nothing
  | otherwise -> Just (T.head s, T.tail s)

chars :: Text -> Parser Char
chars "" = empty
chars t = char (T.head t) <|> chars (T.tail t)

string :: Text -> Parser Text
string t = T.pack <$> str (T.unpack t)
  where str = \case (x:xs) -> mappend . pure <$> char x <*> str xs
                    [] -> pure []

quotedString :: Parser Text
quotedString = between (char '"') (char '"') (fmap T.pack $ some $ anyChar `provided` (/=) '"')

word :: Parser Text
word = fmap T.pack $ some $ anyChar `provided` (' '/=)

digit :: Parser Integer
digit = read . show <$> (chars $ T.pack ['0'..'9'])

number :: Parser Integer
number = fmap read $ some $ chars $ T.pack ['0'..'9']

ws :: Parser Text
ws = fmap T.pack $ many $ char ' '

trim :: Parser a -> Parser a
trim p = ws *> p <* ws

-- -----------------
-- Token generators
-- -----------------

command :: Parser CommandData
command = do
  p <- trim $ string "🤠" <|> string "please cowbot would you "
  c <- trim word
  a <- many $ trim $ quotedString <|> word
  return CommandData { prefix = p, name = c, args = a }
