{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Parse where

import           Control.Monad.State (MonadState (get, put))
import           Data.Bot            (Command)
import           Data.Discord        (Exposes, Updates (..), askExposed)
import           Data.Text.Lazy      (Text)
import           Parser              (Parser, runParser)

class (Monad m, Exposes Text m) => Parse m where
    parse :: Parser a -> m (Maybe a)

instance Updates Text Command where
    setExposed a = get >>= \(m, _) -> put (m, a)

instance Parse Command where
    parse p = do
        t <- askExposed
        case runParser p t of Nothing        -> pure Nothing
                              Just (a, rest) -> setExposed rest >> pure (Just a)

