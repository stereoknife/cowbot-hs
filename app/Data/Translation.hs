{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Translation ( LocalizedText (..)
                        , KL (..)
                        , translate
                        , whatLang
                        , whatLangs
                        , locText
                        , locTexts
                        , locTextArray
                        ) where

import           Control.Applicative        (Alternative ((<|>)))
import           Control.Monad              (when)
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON (parseJSON), ToJSON,
                                             withObject, (.:), (.:?))
import           Data.Language              (Lang, toShortCodeT)
import           Data.Maybe                 (fromJust, isNothing)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy             (toStrict)
import           GHC.Generics               (Generic)
import           Howdy.Error                (HowdyException (UnknownError),
                                             MonadError (throwError))
import           HTMLEntities.Decoder       (htmlEncodedText)
import           Network.HTTP.Simple        (getResponseBody, httpJSON,
                                             parseRequest, setRequestBodyJSON,
                                             setRequestHeader,
                                             setRequestIgnoreStatus,
                                             setRequestMethod, setRequestPath,
                                             setRequestQueryString)
import           Network.HTTP.Types         (hContentType)
import           Secrets                    (translateKey)

-- This is all taken straight from legacy cowbot
-- might be worth taking a look at and refactoring

data KL = KAuto | KLang

data LocalizedText (a :: KL) where
    Auto :: Text -> LocalizedText KAuto
    LocalizedText :: Lang -> Text -> LocalizedText KLang
    TranslatedText :: Lang -> Text -> LocalizedText KLang -> LocalizedText KLang

instance Show (LocalizedText a) where
    show (Auto t)               = "Lang: Auto, Text: " ++ show t
    show (LocalizedText l t)    = "Lang: " ++ show l ++ ", Text: " ++ show t
    show (TranslatedText l t n) = "Lang: " ++ show l ++ ", Text: " ++ show t ++ " <== " ++ show n

data ReqBody = ReqBody
    { q      :: Text
    , source :: Maybe Text
    , target :: Text
    , format :: Text
    } deriving (Generic, Show)

instance ToJSON ReqBody where
instance FromJSON ReqBody where

data APIResponse = APIResponse { translation  :: T.Text
                               , detectedLang :: Maybe Lang
                               } deriving (Show)

instance FromJSON APIResponse where
    parseJSON = withObject "" $ \v -> do
        d <- v .: "data"
        (t:_) <- d .: "translations"
        tx <- t .: "translatedText"
        tl <- t .:? "detectedSourceLanguage"
        return $ APIResponse tx tl

whatLang :: LocalizedText KLang -> Lang
whatLang (LocalizedText l _)    = l
whatLang (TranslatedText l _ _) = l

whatLang' :: LocalizedText a -> Maybe Lang
whatLang' (Auto _)               = Nothing
whatLang' (LocalizedText l _)    = Just l
whatLang' (TranslatedText l _ _) = Just l

whatLangs :: LocalizedText KLang -> [Lang]
whatLangs (LocalizedText l _)     = [l]
whatLangs (TranslatedText l _ tl) = l : whatLangs tl

locText :: LocalizedText n -> Text
locText (Auto t)               = t
locText (LocalizedText _ t)    = t
locText (TranslatedText _ t _) = t

locTexts :: LocalizedText n -> [Text]
locTexts (Auto t)                = [t]
locTexts (LocalizedText _ t)     = [t]
locTexts (TranslatedText _ t tl) = t : locTexts tl

locTextArray :: LocalizedText KLang -> [LocalizedText KLang]
locTextArray l@(LocalizedText _ _)   = [l]
locTextArray (TranslatedText l t lt) = LocalizedText l t : locTextArray lt

translate :: forall a m. (MonadIO m, MonadThrow m) => LocalizedText a -> Lang -> m (LocalizedText KLang)
translate t' tl = do
    let t  = locText t'
        fl = whatLang' t'

    key <- liftIO translateKey

    defReq <- parseRequest "https://translation.googleapis.com/"

    let request
            -- = setRequestHost "https://translation.googleapis.com/"
            = setRequestPath "/language/translate/v2"
            $ setRequestMethod "POST"
            $ setRequestQueryString [("key", Just $ encodeUtf8 key)]
            $ setRequestBodyJSON (ReqBody t (toShortCodeT <$> fl) (toShortCodeT tl) "text")
            $ setRequestHeader hContentType ["application/json; charset=utf-8"]
            $ setRequestIgnoreStatus
            defReq

    response <- httpJSON request

    let resBody = getResponseBody @APIResponse response
        resText = toStrict . toLazyText . htmlEncodedText $ translation resBody
        detLang = fl <|> detectedLang resBody

    when (isNothing detLang) $ throwM UnknownError

    let otl :: LocalizedText KLang = case t' of TranslatedText {} -> t'
                                                _                 -> LocalizedText (fromJust detLang) t

    pure $ TranslatedText tl resText $ LocalizedText (fromJust detLang) t
