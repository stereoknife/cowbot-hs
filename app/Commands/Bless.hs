{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Commands.Bless where
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Aeson             (FromJSON, decode)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           GHC.Generics           (Generic)
import           Howdy.Action           (CommandRunner)
import           Howdy.Discord.Class    (MonadReply, reply)
import           Howdy.Error            (HowdyException (..), report)
import           Network.HTTP.Simple    (getResponseBody, httpJSON,
                                         parseRequest, setRequestIgnoreStatus,
                                         setRequestMethod, setRequestPath,
                                         setRequestQueryString)

data APIResponse = APIResponse { bookname :: Text
                               , chapter  :: Text
                               , verse    :: Text
                               , text     :: Text
                               } deriving (Generic, Show)

instance FromJSON APIResponse where

bless :: (MonadIO m, MonadReply m, MonadThrow m) => m ()
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

    reply $ "**" <> bookname r <> " " <> chapter r <> ":" <> verse r <> "** " <> text r
