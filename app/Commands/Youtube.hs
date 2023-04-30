{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Commands.Youtube where

import           Control.Applicative    (Alternative)
import           Control.Monad          (guard)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (asks)
import           Data.Aeson             (FromJSON (parseJSON), ToJSON,
                                         withObject, (.:))
import           Data.Maybe             (fromJust, isJust)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           GHC.Generics           (Generic)
import           Howdy.Comptime.Command (Command, CommandInput (..))
import           Howdy.Internal.Discord (send)
import           Network.HTTP.Simple    (getResponseBody, httpJSON,
                                         parseRequest, setRequestIgnoreStatus,
                                         setRequestMethod, setRequestPath,
                                         setRequestQueryString)
import           Secrets                (youtubeKey)

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

youtubeRequest :: (MonadIO m, MonadThrow m) => Text -> m (Either String Text)
youtubeRequest query = do
    key <- liftIO youtubeKey

    defReq <- parseRequest "https://www.googleapis.com"

    let request
            -- = setRequestHost "https://translation.googleapis.com/"
            = setRequestPath "/youtube/v3/search"
            $ setRequestMethod "GET"
            $ setRequestQueryString [ ("key", Just $ encodeUtf8 key)
                                    , ("part", Just $ encodeUtf8 "snippet")
                                    , ("q", Just $ encodeUtf8 query)
                                    , ("maxResults", Just $ encodeUtf8 "1")
                                    ]
            $ setRequestIgnoreStatus
            defReq

    response <- httpJSON request

    let resBody = getResponseBody @APIResponse response

    pure $ Right $ getText resBody

yt :: Command
yt c = do
    id <- youtubeRequest c.args
    case id of Right id -> send $ "https://youtube.com/watch?v=" <> id
               _        -> send "Couldn't find anything 😔"
