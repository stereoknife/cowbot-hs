{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Parser.Parser ( Parser (..)
                     , ParseResult (..)
                     , alias
                     , prefix
                     , command
                     , args
                     , rest
                     , flag
                     , trim
                     , string
                     ) where

import           Control.Applicative             (Alternative (..))
import           Control.Applicative.Combinators (between)
import           Control.Monad                   (guard)
import           Control.Monad.State
import           Data.Text                       (Text)
import qualified Data.Text                       as T

data ParseResult = ParseResult { getPrefix :: Text
                               , getName   :: Text
                               , getArgs   :: [Text]
                               } deriving (Show)

newtype Parser a = Parser { runParser :: Text -> Maybe (a, Text) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (v, rest) <- p s
    return (f v, rest)

instance Applicative Parser where
  pure v = Parser $ \s -> Just (v, s)
  pf <*> p = Parser $ \s -> do
    (f, s') <- runParser pf s
    runParser (f <$> p) s'

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s ->
    runParser p1 s <|> runParser p2 s

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \s ->
    case runParser p s of Just (p', s') -> runParser (f p') s'
                          Nothing       -> Nothing

instance Semigroup a => Semigroup (Parser a) where
  p1 <> p2 = Parser $ \s ->
    runParser p1 s <> runParser p2 s

instance Semigroup a => Monoid (Parser a) where
  mempty = empty

provided :: Parser a -> (a -> Bool) -> Parser a
p `provided` f = Parser $ \s -> do
  (v, rest) <- runParser p s
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
  | otherwise    -> Just (T.head s, T.tail s)

chars :: Text -> Parser Char
chars "" = empty
chars t  = char (T.head t) <|> chars (T.tail t)

string :: Text -> Parser Text
string t = T.pack <$> str (T.unpack t)
  where str = \case (x:xs) -> mappend . pure <$> char x <*> str xs
                    []     -> pure []

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

rest :: Parser Text
rest = Parser $ \s -> Just (s, "")

-- -----------------
-- Token generators
-- -----------------

command :: Parser ParseResult
command = do
  p <- trim $ string "🤠" <|> string "please cowbot would you "
  c <- trim word
  a <- many . trim $ quotedString <|> word
  return $ ParseResult p c a

prefix :: Parser Text
prefix = trim $ string "🤠" <|> string "please cowbot would you "

alias :: Parser Text
alias = trim word

args :: Parser [Text]
args = many . trim $ quotedString <|> word

flag :: Text -> Parser Text
flag = trim . string . ("--" <>)
