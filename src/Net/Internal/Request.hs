{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Net.Internal.Request where

import           Control.Monad.Catch     (MonadThrow)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (FromJSON, eitherDecode)
import           Data.Bifunctor
import           Data.ByteString.Lazy    (ByteString)
import           Data.Text               (Text, pack, unpack)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

newtype Query = Query Text
newtype URL = URL Text


get :: forall a m. (MonadIO m, MonadThrow m, FromJSON a) => URL -> Query -> m (Either String a)
get (URL u) (Query q) = do
    manager <- newTlsManager
    request <- parseRequest $ unpack $ u <> "?" <> q
    response <- liftIO $ httpLbs request manager
    return $ eitherDecode . responseBody $ response

post :: forall a m. (MonadIO m, MonadThrow m, FromJSON a) => URL -> ByteString -> m (Either String a)
post (URL u) b = do
    manager <- newTlsManager
    initialRequest <- parseRequest $ unpack u
    let request = initialRequest { method = "POST", requestBody = RequestBodyLBS b }
    response <- liftIO $ httpLbs request manager
    return $ eitherDecode . responseBody $ response
