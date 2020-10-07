{-# LANGUAGE OverloadedStrings #-}

module Commands.Bless (bless) where

import           Commands.Base             (Command)
import           Control.Monad.Combinators (empty)
import           Control.Monad.Reader      (asks, liftIO)
import           Data.Aeson                (Result (Success), withArray, (.:))
import qualified Data.Aeson.Types          as JSON
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Discord                   (restCall)
import qualified Discord.Requests          as R
import           Discord.Types             (Message (messageChannel))
import           Network.HTTP.Req          (GET (GET), NoReqBody (NoReqBody),
                                            defaultHttpConfig, https,
                                            jsonResponse, req, responseBody,
                                            runReq, (/:), (=:))

bless :: Command ()
bless = do
    ch <- asks messageChannel
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

    case rb of Success (b, c, v, t) -> return $ restCall $ R.CreateMessage ch $
                                        "**" <> b <> " " <> c <> ":" <> v <> "** " <> t
               _ -> empty

    pure ()
