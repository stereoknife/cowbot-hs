{-# LANGUAGE ConstraintKinds #-}

module Bot.Internal.Net where

import           Control.Applicative    (Alternative)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)

type Net a = (MonadIO a, MonadThrow a, Alternative a)
