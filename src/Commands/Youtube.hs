{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Commands.Youtube where

import           Control.Applicative (Alternative)
import           Control.Monad       (guard)
import           Data.Aeson          (Result (Success), withObject, (.:))
import qualified Data.Aeson.Types    as JSON
import           Data.Maybe          (isJust)
import qualified Data.Text           as T
import           Network.HTTP.Req    (GET (GET), NoReqBody (NoReqBody),
                                      defaultHttpConfig, https, jsonResponse,
                                      req, responseBody, runReq, (/:), (=:))
import           Parser.Parser       (rest)
import           Secrets             (yt_key)
import           Types               (Parser (parse), Reply (reply))
import           UnliftIO            (MonadIO (liftIO))

yt :: (Reply m, MonadIO m, Alternative m, Parser m) => m ()
yt = do
    query <- parse rest
    guard $ isJust query

    key <- liftIO yt_key

    request <- runReq defaultHttpConfig $ req
       {-Method-} GET
          {-URL-} (https "www.googleapis.com"/:"youtube"/:"v3"/:"search")
         {-Body-} NoReqBody
{-Response type-} jsonResponse
        {-Query-}  $ "part" =: ("snippet" :: T.Text)
                  <> "maxResults" =: ("1" :: T.Text)
                  <> "key" =: key
                  <> "q" =: query
    json <- return $ withObject "data" $
            \dat -> do
              items   <- dat    .: "items"
              item    <- return  $  head items
              id      <- item   .: "id"
              vid     <- id     .: "videoId"
              return (vid :: T.Text)

    id <- return $ JSON.parse json $ responseBody request

    case id of Success id -> reply $ "https://youtube.com/watch?v=" <> id
               _          -> reply $ "Couldn't find anything 😔"
