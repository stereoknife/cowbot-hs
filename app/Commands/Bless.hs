{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Commands.Bless where
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Aeson             (FromJSON, decode)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           GHC.Generics           (Generic)
import           Howdy.Error            (HowdyException (..), report)
import           Network.HTTP.Simple    (getResponseBody, httpJSON,
                                         parseRequest, setRequestIgnoreStatus,
                                         setRequestMethod, setRequestPath,
                                         setRequestQueryString)
import Howdy.Internal.Command
import Howdy.Internal.Discord

data APIResponse = APIResponse { bookname :: Text
                               , chapter  :: Text
                               , verse    :: Text
                               , text     :: Text
                               } deriving (Generic, Show)

instance FromJSON APIResponse where

bless :: Command
bless = do
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

    send $ "**" <> bookname r <> " " <> chapter r <> ":" <> verse r <> "** " <> text r
