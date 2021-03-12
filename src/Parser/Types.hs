{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Parser.Types ( Parser (..), Parse (..) ) where

import           Control.Applicative (Alternative (..))
import           Data.Text.Lazy      (Text)
import           Discord             (DiscordHandler)

newtype Parser a = Parser { runParser :: Text -> Maybe (a, Text) }

class Monad m => Parse m where
  parse :: Parser a -> Text -> m (Maybe a)

instance Parse DiscordHandler where
  parse p t = return $ fst <$> runParser p t

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
  p1 <> p2 = do
    r1 <- p1
    r2 <- p2
    return $ r1 <> r2

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty
