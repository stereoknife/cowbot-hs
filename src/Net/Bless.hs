{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Net.Bless where

import           Bot.Internal.Discord (MonadIO, liftIO)
import           Control.Monad.Catch  (MonadThrow)
import           Data.Aeson           (FromJSON (..), withArray)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Net.Internal.Request (Query (Query), URL (..), get)

data APIResponse = APIResponse { bookname :: Text
                               , chapter  :: Text
                               , verse    :: Text
                               , text     :: Text
                               } deriving (Generic, Show)

instance FromJSON APIResponse where

api :: URL
api = URL "https://labs.bible.org/api"

blessRequest :: (MonadIO m, MonadThrow m) => m (Either String Text)
blessRequest = do
    response' <- get @[APIResponse] api $ Query "passage=random&type=json"
    let response = head <$> response'
    return $ flip fmap response $ \res -> "**" <> bookname res <> " " <> chapter res <> ":" <> verse res <> "** " <> text res
