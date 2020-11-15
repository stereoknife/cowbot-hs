{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Net.Translate (translateRequest, TranslationResponse (..), Translate) where

import           Control.Applicative    (Alternative ((<|>)))
import           Control.Monad          (guard)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON (parseJSON), ToJSON, encode,
                                         withObject, (.:), (.:?))
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text, pack, unpack)
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Text.Lazy         (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Debug.Trace            (trace)
import           GHC.Generics           (Generic)
import           HTMLEntities.Decoder   (htmlEncodedText)
import           Net.Internal.Request   (URL (..), post)
import           Network.HTTP.Simple
import           Network.HTTP.Types     (hContentType)
import           Secrets                (tr_key)
import           Types.Translate        (Lang, asShortCodeT, fromShortCode,
                                         langNameT)

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

data APIResponse = APIResponse { translation  :: Text
                               , detectedLang :: Maybe Lang
                               } deriving (Show)

instance ToJSON Body where
instance FromJSON Body where

instance FromJSON APIResponse where
    parseJSON = withObject "" $ \v -> do
        d <- v .: "data"
        (t:_) <- d .: "translations"
        tx <- t .: "translatedText"
        tl <- t .:? "detectedSourceLanguage"
        return $ APIResponse tx tl

translateRequest :: (MonadIO m, MonadThrow m, Alternative m) => Maybe Lang -> Lang -> Text -> m (Either String TranslationResponse)
translateRequest from to query = do

    key <- liftIO $ tr_key

    initialRequest <- parseRequest "https://translation.googleapis.com/"
    let request
            = setRequestPath "/language/translate/v2"
            $ setRequestMethod "POST"
            $ setRequestQueryString [("key", Just $ encodeUtf8 key)]
            $ setRequestBodyJSON (Body query (asShortCodeT <$> from) (asShortCodeT to) "text")
            $ setRequestHeader hContentType ["application/json; charset=utf-8"]
            $ setRequestIgnoreStatus
            $ initialRequest

    response <- httpJSON request
    guard $ (let s = getResponseStatusCode response in s >= 200 && s < 300)

    let resBody = getResponseBody @APIResponse response
        resText = toStrict . toLazyText $ htmlEncodedText $ translation resBody
    return $
        if resText == query
        then Left "nothing to translate.."
        else Right $
            TranslationResponse { fromText = query
                                , fromLang =  fromMaybe "who knever knows.." $ langNameT <$> (detectedLang resBody <|> from)
                                , toText = resText
                                , toLang = langNameT to
                                }
