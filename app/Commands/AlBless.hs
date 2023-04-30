{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Commands.AlBless where

import           Data.Aeson              (FromJSON)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           GHC.Generics            (Generic)
import           Howdy.Comptime.Command  (Command)
import           Howdy.Error             (HowdyException (UnknownError), report)
import           Howdy.Internal.Discord  (send)
import           Network.HTTP.Client     (parseRequest, setRequestIgnoreStatus)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.HTTP.Simple     (getResponseBody, httpJSON,
                                          parseRequest, setRequestIgnoreStatus,
                                          setRequestMethod,
                                          setRequestQueryString)

data APIResponsePayload = APIResponsePayload { text_imlaei_simple  :: Text
                                        --      , verse_number        :: Integer
                                        --      , juz_number          :: Integer
                                        --      , hizb_number         :: Integer
                                        --      , rub_number          :: Integer
                                             } deriving (Generic, Show)

newtype APIResponse = APIResponse { verse :: APIResponsePayload } deriving (Generic, Show)

instance FromJSON APIResponsePayload where
instance FromJSON APIResponse where

alBless :: Command
alBless c = do
    defReq <- parseRequest "https://api.quran.com/api/v4/verses/random"

    let request
            = setRequestMethod "GET"
            $ setRequestQueryString [ ("words", Just $ encodeUtf8 "false")
                                    , ("fields", Just $ encodeUtf8 "text_imlaei_simple")
                                    ]
            $ setRequestIgnoreStatus
            defReq

    response <- httpJSON request

    let b = getResponseBody @APIResponse response
    let r = verse b

    send $ text_imlaei_simple r
