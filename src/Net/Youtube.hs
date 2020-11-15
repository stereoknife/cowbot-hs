{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Net.Youtube (youtubeRequest) where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON (parseJSON), ToJSON,
                                         withObject, (.:))
import           Data.Text              (Text, pack, unpack)
import           GHC.Generics           (Generic)
import           Net.Internal.Request   (Query (..), URL (..), get)
import           Network.HTTP.Base      (urlEncodeVars)
import           Secrets                (yt_key)

data Body = Body
    { q          :: Text
    , key        :: Text
    , maxResults :: Text
    , part       :: Text
    } deriving (Generic, Show)

newtype APIResponse = APIResponse { getText :: Text } deriving (Show)

instance ToJSON Body where
instance FromJSON Body where

instance FromJSON APIResponse where
    parseJSON = withObject "data" $ \v -> do
        (i:_) <- v .: "items"
        id <- i .: "id"
        vid <- id .: "videoId"
        return $ APIResponse vid

gapi :: URL
gapi = URL "https://www.googleapis.com/youtube/v3/search/"

youtubeRequest :: (MonadIO m, MonadThrow m) => Text -> m (Either String Text)
youtubeRequest query = do
    key <- unpack <$> liftIO yt_key
    response <- get @APIResponse gapi $ Query . pack $ urlEncodeVars [("key", key),("q", unpack query)]
    return $ getText <$> response
