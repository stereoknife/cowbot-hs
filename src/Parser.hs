{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Parser ( Parser
              , char
              , string
              ) where

import Data.Char
import Control.Applicative ( Alternative (..)
                           , many
                           )
import Control.Monad (guard, unless)
import Control.Monad.Combinators (between)
import qualified Data.Text as T

type Text = T.Text

data Token = NumTok Int 
           | StringTok String
           | FlagTok String

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s ->
    case p s of Just (v, rest) -> Just (f v, rest)
                Nothing -> Nothing

instance Applicative Parser where
  pure v = Parser $ \s -> Just (v, s)
  pf <*> p = Parser $ \s ->
    let Just (f, s') = parse pf s
    in parse (f `fmap` p) s'

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
    let p1' = parse p1 s
        p2' = parse p2 s
    in case (p1', p2') of
      (Nothing, Nothing) -> Nothing
      (Just v, Nothing) -> Just v
      (Nothing, Just v) -> Just v
      (Just v, Just v') -> Just (v <> v')

instance Semigroup a => Monoid (Parser a) where
  mempty = empty

(<<) :: Monad m => m a -> (a -> m b) -> m a
p << f = p >>= \v -> f v >> return v

-- Parser generators
char :: Char -> Parser Char
char c = anyChar << (guard . (c==))
-- char c = anyChar >>= guard . (c==) >> return c

anyChar :: Parser Char
anyChar = Parser $ \case
  (x:xs) -> Just (x, xs)
  _ -> Nothing

chars :: String -> Parser Char
chars (x:xs) = char x <|> chars xs

string :: String -> Parser String
string [] = Parser $ \s -> Just("", s)
string (x:xs) = mappend [x] <$> (char x >> string xs)

anyString :: Parser String
anyString = Parser $ \s ->
  let Just (c, rest) = parse anyChar s
  in parse (mappend [c] <$> anyString) s

word :: Parser String
word = between (char ' ') (char ' ') (many $ anyChar << (guard . (' ' /=)))