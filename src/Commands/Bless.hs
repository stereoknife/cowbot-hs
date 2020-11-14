{-# LANGUAGE FlexibleContexts #-}

module Commands.Bless (bless) where

import           Bot.Internal           (Reply (reply))
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Net.Bless              (blessRequest)

bless :: (Reply m, MonadIO m, MonadThrow m) => m ()
bless = do
    res <- blessRequest
    case res of Right a -> reply a
                _       -> return ()
