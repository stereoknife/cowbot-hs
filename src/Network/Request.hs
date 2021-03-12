{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.Request where

import           Control.Monad.Reader (MonadIO (..), ReaderT (..), ask)
import           Data.ByteString.Lazy (ByteString)
import           Network.HTTP.Client  (Manager, Request, Response, httpLbs)


class MonadIO m => Network m where
    manager :: m Manager
    http :: Request -> m (Response ByteString)
    http r = do
        m <- manager
        liftIO $ httpLbs r m

instance MonadIO m => Network (ReaderT Manager m) where
    manager = ask
