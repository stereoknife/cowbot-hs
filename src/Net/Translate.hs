{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Net.Translate (translateRequest, TranslationResponse (..), Translate) where

import           Control.Applicative    (Alternative ((<|>)))
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON (parseJSON), ToJSON, encode,
                                         withObject, (.:), (.:?))
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           Data.Text.Lazy         (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           GHC.Generics           (Generic)
import           HTMLEntities.Decoder   (htmlEncodedText)
import           Net.Internal.Request   (URL (..), post)

type Translate m = (MonadIO m, MonadThrow m)

data Body = Body
    { q      :: Text
    , source :: Maybe Text
    , target :: Text
    , format :: Text
    } deriving (Generic, Show)

data TranslationResponse = TranslationResponse
    { fromText :: Text
    , fromLang :: Text
    , toText   :: Text
    , toLang   :: Text
    } deriving (Show)

newtype APIResponse = APIResponse (Text, Maybe Text)

instance ToJSON Body where
instance FromJSON Body where

instance FromJSON APIResponse where
    parseJSON = withObject "data" $ \v -> do
        (t:_) <- v .: "translations"
        tx <- t .: "translatedText"
        tl <- t .:? "detectedSourceLanguage"
        return $ APIResponse (tx, tl)

gapi :: URL
gapi = URL "https://translation.googleapis.com/language/translate/v2"


translateRequest :: (MonadIO m, MonadThrow m) => Maybe Text -> Text -> Text -> m (Either String TranslationResponse)
translateRequest from to query = do
    let initialData = TranslationResponse { fromText = query
                                          , fromLang = fromMaybe "" from
                                          , toText = ""
                                          , toLang = ""
                                          }

    response <- post @APIResponse gapi $ encode $ Body query from to "text"
    asserted <- return $ assert response "nothing to translate.."

    return $ flip fmap asserted $ \(t, ml) ->
        initialData { fromLang = fromMaybe "who knever knows.." $ ml <|> from
                    , toText = t
                    , toLang = toStrict . toLazyText $ htmlEncodedText to
                    }

    where assert (Right (APIResponse (t, ml))) fail = if t == query then Left fail else Right (t, ml)
          assert _ fail = Left fail
