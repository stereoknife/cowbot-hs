{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Commands.AlBless where
import           Control.Monad.Catch    
import           Control.Monad.IO.Class 
import           Data.Aeson             
import Data.Text ( Text )              
import           Data.Text.Encoding     
import           GHC.Generics           
import           Network.HTTP.Simple    
import Howdy.Internal.Discord 
import Howdy.Internal.Command

data APIResponsePayload = APIResponsePayload { text_imlaei_simple  :: Text
                                             , verse_number        :: Integer
                                             , juz_number          :: Integer
                                             , hizb_number         :: Integer
                                             , rub_number          :: Integer
                                             } deriving (Generic, Show)

newtype APIResponse = APIResponse { verse :: APIResponsePayload } deriving (Generic, Show)

instance FromJSON APIResponsePayload where
instance FromJSON APIResponse where

alBless :: Command
alBless = do
    defReq <- parseRequest "https://api.quran.com/"

    let request
            = setRequestPath "/api/v4/verses/random"
            $ setRequestMethod "GET"
            $ setRequestQueryString [ ("words", Just $ encodeUtf8 "false")
                                    , ("fields", Just $ encodeUtf8 "text_imlaei_simple")
                                    ]
            $ setRequestIgnoreStatus
            defReq

    response <- httpJSON request

    let r = verse $ getResponseBody @APIResponse response

    -- pure ()
    send $ text_imlaei_simple r
