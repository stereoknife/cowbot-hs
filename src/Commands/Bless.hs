{-# LANGUAGE FlexibleContexts #-}

module Commands.Bless (bless) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Aeson             (Result (Success), withArray, (.:))
import qualified Data.Aeson.Types       as JSON
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Network.HTTP.Req       (GET (GET), NoReqBody (NoReqBody),
                                         defaultHttpConfig, https, jsonResponse,
                                         req, responseBody, runReq, (/:), (=:))
import           Types                  (Reply (reply))

bless :: (Reply m, MonadIO m) => m ()
bless = do
    r <- liftIO $ runReq defaultHttpConfig $ req
       {-Method-} GET
          {-URL-} (https "labs.bible.org" /: "api")
         {-Body-} NoReqBody
{-Response type-} jsonResponse
        {-Query-} $ "passage" =: ("random" :: T.Text)
                  <> "type" =: ("json" :: T.Text)

    pb <- return $ withArray "response" $
      \arr -> do
        case V.head arr of
          JSON.Object res -> do
            b <- res .: "bookname"
            c <- res .: "chapter"
            v <- res .: "verse"
            t <- res .: "text"
            return ((b, c, v, t) :: (T.Text, T.Text, T.Text, T.Text))
          _ -> return mempty

    rb <- return $ JSON.parse pb $ responseBody r

    case rb of Success (b, c, v, t) -> reply $ "**" <> b <> " " <> c <> ":" <> v <> "** " <> t
               _                    -> return ()
