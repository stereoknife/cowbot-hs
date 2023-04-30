{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
module Commands.Clock where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Scientific
import           Data.Text.Encoding     (encodeUtf8)
import           GHC.Generics           (Generic)
import           Howdy.Comptime.Command (Command)
import           Network.HTTP.Simple

newtype LocAPIResponse = LocAPIResponse { unwrapLoc :: (Scientific, Scientific)}

instance FromJSON LocAPIResponse where
    parseJSON = withObject "LatLon" $
        \v -> curry LocAPIResponse <$> v .: "lat" <*> v .: "lon"

clock :: Command
clock i = do
    locReq' <- parseRequest "https://nominatim.openstreetmap.org/"

    let locReq
            = setRequestPath "/search"
            $ setRequestMethod "GET"
            $ setRequestQueryString
                [ ("q", Just $ encodeUtf8 "random")
                , ("format", Just $ encodeUtf8 "json")
                , ("limit", Just $ encodeUtf8 "1")
                ]
            $ setRequestIgnoreStatus
            locReq

    locRes <- httpJSON locReq
    let (lat, lon) = unwrapLoc . head $ getResponseBody @[LocAPIResponse] locRes

    pure ()
