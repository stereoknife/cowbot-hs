{-# LANGUAGE OverloadedStrings #-}

module Commands.Youtube where

import           Commands.Base             (Command, parse)
import           Control.Monad.Combinators (empty)
import           Control.Monad.Reader      (MonadIO (liftIO), asks)
import           Data.Aeson                (Result (Success), withObject, (.:))
import qualified Data.Aeson.Types          as JSON
import qualified Data.Text                 as T
import           Discord                   (restCall)
import qualified Discord.Requests          as R
import           Discord.Types             (Message (messageChannel))
import           Network.HTTP.Req          (GET (GET), NoReqBody (NoReqBody),
                                            defaultHttpConfig, https,
                                            jsonResponse, req, responseBody,
                                            runReq, (/:), (=:))
import           Parser                    (rest)
import           Secrets                   (yt_key)

yt :: Command ()
yt = do
    key <- liftIO yt_key
    ch <- asks messageChannel
    mArgs <- parse rest

    wrappedId <- return $ do
      jArgs <- mArgs
      return $ do
        r <- runReq defaultHttpConfig $ req
       {-Method-} GET
          {-URL-} (https "www.googleapis.com"/:"youtube"/:"v3"/:"search")
         {-Body-} NoReqBody
{-Response type-} jsonResponse
        {-Query-}  $ "part" =: ("snippet" :: T.Text)
                  <> "maxResults" =: ("1" :: T.Text)
                  <> "key" =: key
                  <> "q" =: jArgs

        parserId <- return $ withObject "data" $
          \dat -> do
            items   <- dat    .: "items"
            item    <- return  $  head items
            id      <- item   .: "id"
            vid     <- id     .: "videoId"
            return (vid :: T.Text)

        return $ JSON.parse parserId $ responseBody r

    videoId <- case wrappedId of Just a -> liftIO a
                                 _      -> empty

    return $ case videoId of Success id -> restCall $ R.CreateMessage ch $ "https://youtube.com/watch?v=" <> id
                             _          -> restCall $ R.CreateMessage ch "Couldn't find anything 😔"

    pure ()
