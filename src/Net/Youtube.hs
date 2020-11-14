{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Net.Youtube (youtubeRequest, YoutubeResponse) where

import           Control.Applicative    (Alternative ((<|>)))
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON (parseJSON), ToJSON, encode,
                                         withObject, (.:), (.:?))
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Net.Internal.Request   (URL (..), post)

data Body = Body
    { q          :: Text
    , key        :: Text
    , maxResults :: Text
    , part       :: Text
    } deriving (Generic, Show)

newtype YoutubeResponse = YoutubeResponse { videoId :: Text }

newtype APIResponse = APIResponse Text

instance ToJSON Body where
instance FromJSON Body where

instance FromJSON APIResponse where
    parseJSON = withObject "data" $ \v -> do
        (i:_) <- v .: "items"
        id <- i .: "id"
        vid <- id .: "videoId"
        return $ APIResponse vid

gapi :: URL
gapi = URL "www.googleapis.com/youtube/v3/search/"

youtubeRequest :: (MonadIO m, MonadThrow m) => Text -> m (Either String Text)
youtubeRequest query = do
    response <- get @APIResponse gapi $ Query query
    return response
