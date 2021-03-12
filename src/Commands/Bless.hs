{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Commands.Bless (bless) where


import           Control.Monad.Catch (MonadThrow)
import           Data.Aeson          (FromJSON)
import           Data.Text           (Text)
import           Data.Text.Encoding  (encodeUtf8)
import           Data.Text.Lazy      (fromStrict)
import           GHC.Generics        (Generic)
import           Network.Discord     (Reply, reply)
import           Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest,
                                      setRequestIgnoreStatus, setRequestMethod,
                                      setRequestPath, setRequestQueryString)
import           UnliftIO            (MonadIO)


data APIResponse = APIResponse { bookname :: Text
                               , chapter  :: Text
                               , verse    :: Text
                               , text     :: Text
                               } deriving (Generic, Show)

instance FromJSON APIResponse where

blessRequest :: (MonadIO m, MonadThrow m) => m (Either String Text)
blessRequest = do
    defReq <- parseRequest "https://labs.bible.org/"

    let request
            = setRequestPath "/api"
            $ setRequestMethod "GET"
            $ setRequestQueryString [ ("passage", Just $ encodeUtf8 "random")
                                    , ("type", Just $ encodeUtf8 "json")
                                    ]
            $ setRequestIgnoreStatus
            defReq

    response <- httpJSON request

    let r = head $ getResponseBody @[APIResponse] response

    pure . Right $ "**" <> bookname r <> " " <> chapter r <> ":" <> verse r <> "** " <> text r

bless :: (Reply m, MonadIO m, MonadThrow m) => m ()
bless = do
    res <- blessRequest
    case res of Right a -> reply $ fromStrict a
                _       -> return ()
