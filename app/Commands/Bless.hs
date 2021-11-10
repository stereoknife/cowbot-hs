{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Commands.Bless where
import           Control.Monad.Catch     (MonadThrow)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Data.Aeson              (FromJSON, decode)
import           Data.Text               (Text)
import           GHC.Generics            (Generic)
import           Howdy.Action            (CommandRunner)
import           Howdy.Discord.Class     (Reply, reply)
import           Howdy.Error             (HowdyException (..), report)
import           Network.HTTP.Client     (Response (responseBody), httpLbs,
                                          newManager, parseRequest)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

data APIResponse = APIResponse { bookname :: Text
                               , chapter  :: Text
                               , verse    :: Text
                               , text     :: Text
                               } deriving (Generic, Show)

instance FromJSON APIResponse where

bless :: (MonadIO m, Reply m, MonadThrow m) => m ()
bless = do
    manager <- liftIO $ newManager tlsManagerSettings
    request <- parseRequest "http://labs.bible.org/api?passage=random&type=json"

    response <- liftIO $ httpLbs request manager

    payload <- report UnknownError $ decode @[APIResponse] $ responseBody response

    let r = head payload

    reply $ "**" <> bookname r <> " " <> chapter r <> ":" <> verse r <> "** " <> text r
