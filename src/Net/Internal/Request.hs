{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Net.Internal.Request where

import           Control.Monad.Catch     (MonadThrow)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (FromJSON, eitherDecode, encode)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Text               (Text, unpack)
import           Network.HTTP.Client     (Response (responseBody), httpLbs,
                                          parseRequest)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.HTTP.Simple     (setRequestBodyLBS, setRequestHeader,
                                          setRequestMethod, setRequestPath, setRequestQueryString)
import           Network.HTTP.Types      (hContentType)
import Data.Text.Encoding (encodeUtf8)
import Secrets

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
    key <- liftIO $ tr_key
    initialRequest <- parseRequest $ unpack u
    let request
            = setRequestPath "/language/translate/v2"
            $ setRequestMethod "POST"
            $ setRequestQueryString [("key", Just $ encodeUtf8 key)]
            $ setRequestBodyLBS b
            $ setRequestHeader hContentType ["application/json; charset=utf-8"]
            $ initialRequest

    liftIO $ print request
    response <- liftIO $ httpLbs request manager
    liftIO $ print response
    return $ eitherDecode . responseBody $ response
